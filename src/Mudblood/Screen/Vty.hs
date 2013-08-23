{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, BangPatterns #-}

module Mudblood.Screen.Vty
    ( runScreen
    , Screen (Screen)
    , screen
    , nmap
    , mb
    ) where

import Data.Char
import Data.Word
import Data.Monoid
import Data.Dynamic
import qualified Data.Map as M
import qualified Data.Trie as Trie

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Free (Free (Free, Pure))

import Control.Concurrent
import Control.Concurrent.STM

import System.IO
import qualified Graphics.Vty as V

import qualified Codec.Binary.UTF8.String as UTF8

import Mudblood.Core
import Mudblood.Telnet
import Mudblood.Keys
import Mudblood.Text

data Mode = NormalMode | CommandMode

data ScreenState = ScreenState {
    scrPrompt :: String,
    scrMarkedPrompt :: String,

    scrNormalBuffer :: String,
    scrCommandBuffer :: String,
    scrNormalKeybuffer :: [Key],

    scrBindings :: Trie.Trie Key (Screen ()),

    scrMBState :: MBState,
    scrMBConfig :: MBConfig,

    scrEventChan :: TChan Event,
    scrSocket :: Maybe TelnetSocket,

    scrMode :: Mode,

    scrVty :: V.Vty,

    scrQuit :: Bool
    }

newtype Screen a = Screen (StateT ScreenState IO a)
    deriving (Monad, MonadIO, MonadState ScreenState)

io :: (MonadIO m) => IO a -> m a
io = liftIO

data Event = ReceiveEvent String
           | TelnetEvent TelnetNeg
           | CloseEvent
           | KeyEvent Key
           | FifoEvent String
    deriving (Show)

mb :: MB a -> Screen a
mb mb = do
    st <- get
    (a, s) <- interpMB $ runMB (scrMBConfig st) (scrMBState st) mb
    modify $ \st -> st { scrMBState = s }
    return a
  where
    interpMB (Pure r) = return r
    interpMB (Free (MBFIO action g)) = io action >>= interpMB . g
    interpMB (Free (MBFConnect h p x)) = connectScreen h p >> interpMB x
    interpMB (Free (MBFLine l x)) = interpMB x -- DOES NOTHING FOR NOW
    interpMB (Free (MBFSend d x)) = sendSocket d >> interpMB x
    interpMB (Free (MBFQuit x)) = modify (\st -> st { scrQuit = True }) >> interpMB x

mapKey :: V.Key -> Maybe Key
mapKey k = case k of
    V.KASCII c      -> Just $ KAscii c
    V.KEnter        -> Just $ KEnter
    V.KBS           -> Just $ KBS
    V.KEsc          -> Just $ KEsc
    _               -> Nothing

mapColor :: Color -> V.MaybeDefault V.Color
mapColor c = case c of
    DefaultColor    -> V.Default
    Black           -> V.SetTo V.black
    White           -> V.SetTo V.white
    Cyan            -> V.SetTo V.cyan
    Magenta         -> V.SetTo V.magenta
    Blue            -> V.SetTo V.blue
    Yellow          -> V.SetTo V.yellow
    Green           -> V.SetTo V.green
    Red             -> V.SetTo V.red

mapStyle :: Style -> V.MaybeDefault V.Style
mapStyle s = case s of
    StyleNormal     -> V.Default
    StyleBold       -> V.SetTo V.underline
    StyleUnderline  -> V.SetTo V.bold

inputLoop :: V.Vty -> TChan Event -> IO ()
inputLoop v chan =
    do
    forever $ do
              vev <- V.next_event v
              case vev of
                V.EvKey k mod   -> case mapKey k of
                                    Just k' -> liftIO $ atomically $ writeTChan chan $ KeyEvent k'
                                    Nothing -> return ()
                _               -> return ()


-- | Run the screen
runScreen :: MBConfig -> MBState -> Screen () -> IO ()
runScreen conf initMBState action =
    do
    v <- V.mkVty
    chan <- newTChanIO
    forkIO $ inputLoop v chan
    runScreen' (initState v conf chan initMBState) action
    V.shutdown v
  where
    runScreen' st (Screen s) = runStateT s st

    initState v conf chan initMBState = ScreenState {
        scrPrompt = "",
        scrMarkedPrompt = "",
        scrNormalBuffer = "",
        scrCommandBuffer = "",
        scrNormalKeybuffer = [],
        scrBindings = Trie.empty,
        scrMode = NormalMode,
        scrVty = v,
        scrMBConfig = conf,
        scrMBState = initMBState,
        scrSocket = Nothing,
        scrEventChan = chan,
        scrQuit = False
    }

-- | Connect to host:port.
connectScreen :: String -> String -> Screen ()
connectScreen host port =
    do
    state <- get
    case scrSocket state of
        Nothing -> return ()
        Just oldsock -> io $ closeTelnet oldsock
    sock <- io . forkTelnet host port $ telnetProc (scrEventChan state)
    put $ state { scrSocket = Just sock }
  where
    telnetProc chan =
        do
        blocks <- recvTelnet
        case blocks of
            Right bs ->
                do
                forM bs $ \b ->
                    case b of
                         TelnetRawBlock s -> io $ atomically $ writeTChan chan $ ReceiveEvent $ UTF8.decode s
                         TelnetNegBlock n -> io $ atomically $ writeTChan chan $ TelnetEvent n
                telnetProc chan
            Left err ->
                io $ atomically $ writeTChan chan CloseEvent

sendSocket :: [Word8] -> Screen ()
sendSocket dat =
    do
    state <- get
    case (scrSocket state) of
         Just sock -> io $ sendTelnet sock dat
         Nothing -> return ()

-- | Screen main loop
screen :: Screen ()
screen =
    do
    st <- get
    ev <- io $ atomically $ readTChan $ scrEventChan st
    case ev of
        ReceiveEvent chars  -> do
                               (p, _) <- mb $ process (scrPrompt st) chars defaultAttr -- TODO: where to save current attr?
                               modify $ \st -> st { scrPrompt = p }
        CloseEvent          -> mb $ outputString "Connection closed"
        KeyEvent k          -> handleKey k
        TelnetEvent neg     -> do
                               handleTelneg neg
                               mb $ logger LogDebug $ show neg
        _                   -> return ()

    drawScreen
    gets scrQuit >>= (flip when screen) . not

handleTelneg :: TelnetNeg -> Screen ()
handleTelneg neg = case neg of
    TelnetNeg (Just CMD_EOR) Nothing [] ->
        modify $ \st -> st { scrPrompt = "", scrMarkedPrompt = scrPrompt st }
    TelnetNeg (Just CMD_WILL) (Just OPT_EOR) [] ->
        sendSocket $ telnegToBytes $ TelnetNeg (Just CMD_DO) (Just OPT_EOR) []        
    _ -> return ()

handleKey :: Key -> Screen ()
handleKey k =
    do
    st <- get
    case (scrMode st) of
        NormalMode      -> do
            wasBinding <- handleKeybinding k
            if wasBinding
                then return ()
                else case k of
                        KEnter      -> do
                                       mb $ sendString (scrNormalBuffer st ++ "\n")
                                       mb $ outputAString $
                                          (toAttrString $ (scrMarkedPrompt st) ++ (scrPrompt st))
                                          `mappend`
                                          (setFg Yellow $ toAttrString $ scrNormalBuffer st)
                                       modify $ \s -> s { scrNormalBuffer = "", scrPrompt = "" }
                        KBS         -> let b = scrNormalBuffer st
                                       in modify $ \s -> s { scrNormalBuffer = (take (length b - 1) b) }
                        --KAscii 'q'  -> mb $ quit
                        KAscii '~'  -> modify $ \s -> s { scrMode = CommandMode }
                        KAscii k'   -> modify $ \s -> s { scrNormalBuffer = (scrNormalBuffer s ++ [k']) }
        CommandMode     ->
            case k of
                KEsc        -> modify $ \s -> s { scrCommandBuffer = "", scrMode = NormalMode }
                KEnter      -> do
                               mb $ command (scrCommandBuffer st)
                               modify $ \s -> s { scrMode = NormalMode, scrCommandBuffer = "" }
                KBS         -> let b = scrCommandBuffer st
                               in modify $ \s -> s { scrCommandBuffer = (take (length b - 1) b) }
                KAscii k'   -> modify $ \s -> s { scrCommandBuffer = (scrCommandBuffer s ++ [k']) }

handleKeybinding :: Key -> Screen Bool
handleKeybinding k = do
    st <- get
    let kb = (scrNormalKeybuffer st) ++ [k]
    if Trie.isPrefix (scrBindings st) kb
        then do
             case Trie.lookup (scrBindings st) kb of
                    Nothing -> modify (\s -> s { scrNormalKeybuffer = kb })
                    Just ac -> ac >> modify (\s -> s { scrNormalKeybuffer = [] })
             return True
        else modify (\s -> s { scrNormalKeybuffer = [] }) >> return False

nmap :: [Key] -> Screen () -> Screen ()
nmap keys ac = modify $ \s -> s { scrBindings = Trie.insert (scrBindings s) keys ac }

drawScreen :: Screen ()
drawScreen =
    do
    st <- gets scrMBState
    scrst <- get

    display_region <- V.display_bounds (V.terminal (scrVty scrst))
    let (w, h) = (fromIntegral (V.region_width display_region), fromIntegral (V.region_height display_region))

    statusLines <- mb $ getUiValue "status"

    let img_lines = mconcat (map (drawLine scrst) (reverse $ take (h-3) $ concat $ map (wrap w) $ mbLinebuffer st))
    let img_prompt = drawPrompt scrst
    let img_status = drawStatus scrst (show statusLines)

    let cursor = case (scrMode scrst) of
            NormalMode  -> V.Cursor (V.image_width img_prompt) (V.image_height img_lines)
            CommandMode -> V.Cursor (fromIntegral $ length (scrCommandBuffer scrst) + 1) (V.image_height img_lines + 2)
            _           -> V.NoCursor

    io $ V.update (scrVty scrst) (V.Picture
        cursor
        (img_lines V.<-> img_prompt V.<-> (V.char_fill V.def_attr '-' w 1) V.<-> img_status)
        (V.Background ' ' V.def_attr)
        )

drawLine :: ScreenState -> AttrString -> V.Image
drawLine s l = V.pad (1,1) $ V.horiz_cat $ map drawChunk (groupAttrString $ untab 8 l)
    where drawChunk (c, a) = V.string (V.Attr (mapStyle $ attrStyle a) (mapColor $ attrFg a) (mapColor $ attrBg a)) c

drawPrompt :: ScreenState -> V.Image
drawPrompt s = (V.string V.def_attr (scrMarkedPrompt s))
               V.<|>
               (V.string V.def_attr (scrPrompt s))
               V.<|>
               (V.string (V.with_fore_color V.def_attr V.yellow) (scrNormalBuffer s))

drawStatus :: ScreenState -> String -> V.Image
drawStatus s status = V.pad (1,1) $
    case (scrMode s) of
        CommandMode -> (V.pad (20,1) $ V.string (V.with_style V.def_attr V.bold) (":" ++ (scrCommandBuffer s)))
        _           -> (V.background_fill 20 1)
    V.<|>
    (V.string V.def_attr status)
