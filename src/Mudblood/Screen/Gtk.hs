{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, BangPatterns #-}

module Mudblood.Screen.Gtk
    ( execScreen
    , Screen (Screen)
    , screen
    , mb
    , bind
    ) where

import Prelude hiding (error)

import Data.Char
import Data.Word
import Data.Monoid
import Data.Dynamic
import Data.IORef
import qualified Data.Map as M
import qualified Data.Trie as Trie

import Text.Printf (printf)

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Free (Free (Free, Pure))

import System.IO
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Builder as GB
import Graphics.UI.Gtk (AttrOp (..), on, after)
import Graphics.Rendering.Pango as P

import qualified Codec.Binary.UTF8.String as UTF8

import Mudblood.Core
import Mudblood.Telnet
import Mudblood.Keys
import Mudblood.Text
import Mudblood.UI
import Mudblood.Mapper

import Paths_mudblood

-- data Mode = NormalMode | CommandMode

-- | Socket Events
data SocketEvent = DataEvent String
                 | TelnetEvent TelnetNeg
                 | CloseEvent

------------------------------------------------------------------------------

data ScreenState = ScreenState {
    scrPrompt :: String,
    scrMarkedPrompt :: String,

    scrNormalBuffer :: String,
    scrCommandBuffer :: String,
    scrNormalKeybuffer :: [Key],

    scrBindings :: Trie.Trie Key (Screen ()),

    scrMBState :: MBState,
    scrMBConfig :: MBConfig,

    scrSocket :: Maybe TelnetSocket,

    --scrMode :: Mode,

    scrQuit :: Bool
    }

data ScreenControls = ScreenControls
    { ctlMainView       :: G.TextView
    , ctlMainScroll     :: G.ScrolledWindow
    , ctlMainBuffer     :: G.TextBuffer
    , ctlMainInput      :: G.Entry
    , ctlStatusKeyseq   :: G.Label
    , ctlStatusUser     :: G.Label
    , ctlStatusSystem   :: G.Label
    }

newtype Screen a = Screen (ReaderT (IORef ScreenState, ScreenControls) IO a)
    deriving (Monad, MonadIO, MonadReader (IORef ScreenState, ScreenControls))

instance MonadState ScreenState Screen where
    get   = Screen (ask >>= liftIO . readIORef . fst)
    put s = Screen (ask >>= liftIO . flip writeIORef s . fst)

askControls :: Screen ScreenControls
askControls = ask >>= return . snd

runScreen ctrls stref (Screen s) = runReaderT s (stref, ctrls)

------------------------------------------------------------------------------

mb :: MB a -> Screen a
mb mb = do
    st <- get
    (a, s) <- interpMB $ runMB (scrMBConfig st) (scrMBState st) mb
    modify $ \st -> st { scrMBState = s }
    return a
  where
    interpMB (Pure r) = return r
    interpMB (Free (MBFIO action g)) = liftIO action >>= interpMB . g
    interpMB (Free (MBFConnect h p x)) = connectScreen h p >> interpMB x
    interpMB (Free (MBFLine l x)) = appendToMainBuffer l >> interpMB x
    interpMB (Free (MBFSend d x)) = sendSocket d >> interpMB x
    interpMB (Free (MBFQuit x)) = liftIO (G.mainQuit) >> interpMB x
    interpMB (Free (MBFUI a x)) = execUIAction a >> interpMB x

------------------------------------------------------------------------------

-- | Open a connection
connectScreen :: String -> String -> Screen ()
connectScreen host port =
    do
    state <- get
    completeState <- ask
    case scrSocket state of
        Nothing -> return ()
        Just oldsock -> liftIO $ telnetClose oldsock
    sock <- liftIO . telnetConnect host port $ telnetRecvHandler $ telnetProc completeState
    case sock of
        Right sock' -> modify $ \s -> s { scrSocket = Just sock' }
        Left err -> mb $ error err
  where
    telnetProc state ev = case ev of
        TelnetRawEvent s -> liftIO $ telnetReceiveProc state $ DataEvent $ UTF8.decode s
        TelnetNegEvent n -> liftIO $ telnetReceiveProc state $ TelnetEvent n
        TelnetCloseEvent reason -> liftIO $ telnetReceiveProc state $ CloseEvent

    telnetReceiveProc st ev = let (Screen s) = telnetReceive ev
                              in G.postGUIAsync $ runReaderT s st

-- | Handle a SocketEvent
telnetReceive :: SocketEvent -> Screen ()
telnetReceive ev = do
    st <- get
    case ev of
        DataEvent chars  -> do (p, _) <- mb $ process (scrPrompt st) chars defaultAttr -- TODO: where to save current attr?
                               modify $ \st -> st { scrPrompt = p }
        CloseEvent          -> mb $ echo "Connection closed"
        TelnetEvent neg     -> do
                               handleTelneg neg
                               mb $ logger LogDebug $ show neg

-- | Send a byte array to the socket (if existent)
sendSocket :: [Word8] -> Screen ()
sendSocket dat =
    do
    state <- get
    case (scrSocket state) of
         Just sock -> liftIO $ telnetSend sock dat
         Nothing -> appendToMainBuffer (toAttrString "No socket")

-- | The screen main loop
screen :: Screen ()
screen = liftIO G.mainGUI

-- | Append a line to the main line buffer
appendToMainBuffer :: AttrString -> Screen ()
appendToMainBuffer astr = do mapM_ appendChunk (groupAttrString $ untab 8 astr)
                             ctrls <- askControls
                             endIter <- liftIO $ G.textBufferGetEndIter (ctlMainBuffer ctrls)
                             liftIO $ G.textBufferInsert (ctlMainBuffer ctrls) endIter "\n"
    where appendChunk :: (String, Attr) -> Screen ()
          appendChunk (s, a) = do
            ctrls <- askControls
            let mainbuf = (ctlMainBuffer ctrls)
            liftIO $ do startIter <- G.textBufferGetEndIter mainbuf
                        startOffset <- G.textIterGetOffset startIter
                        G.textBufferInsert mainbuf startIter s
                        endOffset <- G.textBufferGetEndIter mainbuf >>= G.textIterGetOffset
                        when (tagNameForAttr a /= "") $ do
                            startIter' <- G.textBufferGetIterAtOffset mainbuf startOffset
                            endIter' <- G.textBufferGetIterAtOffset mainbuf endOffset
                            G.textBufferApplyTagByName mainbuf (tagNameForAttr a) startIter' endIter'
                            return ()

execUIAction :: UIAction -> Screen ()
execUIAction action = case action of
    UIStatus str -> askControls >>= (\l -> liftIO $ G.labelSetText l str) . ctlStatusUser
    UIUpdateMap map -> do
        ctls <- askControls
        liftIO $ G.labelSetText (ctlStatusSystem ctls) $
            printf "Room: %d" (mapCurrent map)

------------------------------------------------------------------------------

tagNameForAttr :: Attr -> String
tagNameForAttr a = case attrFg a of
    DefaultColor    -> ""
    Black           -> "black"
    White           -> "white"
    Cyan            -> "cyan"
    Magenta         -> "magenta"
    Blue            -> "blue"
    Yellow          -> "yellow"
    Green           -> "green"
    Red             -> "red"

mapKey :: Maybe Char -> String -> Maybe Key
mapKey chr name = case name of
    "Return"    -> Just KEnter
    "Escape"    -> Just KEsc
    "BackSpace" -> Just KBS
    _           -> fmap KAscii chr

initUI :: String -> IORef ScreenState -> IO ScreenControls
initUI path stref = do
    builder <- GB.builderNew
    GB.builderAddFromFile builder path

    window          <- GB.builderGetObject builder G.castToWindow       "mainWindow"
    mainView        <- GB.builderGetObject builder G.castToTextView     "mainView"
    mainScroll      <- GB.builderGetObject builder G.castToScrolledWindow "mainScroll"
    mainBuffer      <- G.textViewGetBuffer mainView
    mainInput       <- GB.builderGetObject builder G.castToEntry        "mainInput"
    statusKeyseq    <- GB.builderGetObject builder G.castToLabel        "statusKeyseq"
    statusUser      <- GB.builderGetObject builder G.castToLabel        "statusUser"
    statusSystem    <- GB.builderGetObject builder G.castToLabel        "statusSystem"

    let controls = ScreenControls
            { ctlMainView       = mainView
            , ctlMainScroll     = mainScroll
            , ctlMainBuffer     = mainBuffer
            , ctlMainInput      = mainInput
            , ctlStatusKeyseq   = statusKeyseq
            , ctlStatusUser     = statusUser
            , ctlStatusSystem   = statusSystem
            }

    monoFont <- P.fontDescriptionFromString "monospace"

    G.widgetModifyFont mainView $ Just monoFont
    G.widgetModifyFg mainView G.StateNormal $ P.Color 60000 60000 60000
    G.widgetModifyBase mainView G.StateNormal $ P.Color 0 0 0

    G.widgetModifyFont mainInput $ Just monoFont

    G.widgetModifyFont statusKeyseq $ Just monoFont
    G.widgetModifyFont statusUser $ Just monoFont
    G.widgetModifyFont statusSystem $ Just monoFont

    tagTable <- G.textBufferGetTagTable mainBuffer

    forM_ ["black", "white", "cyan", "magenta", "blue", "yellow", "green", "red"] $ \x -> do
        tag <- G.textTagNew $ Just x
        G.set tag [ G.textTagForeground := x ]
        G.textTagTableAdd tagTable tag

    {-
    window `on` G.entryActivate $ runScreen controls stref $ do
        text <- liftIO $ G.get mainInput G.entryText
        mb $ outputAString (setFg Yellow (toAttrString text))
        mb $ sendString text
        liftIO $ G.set mainInput [ G.entryText := "" ]
        -}

    -- Key press handler
    mainInput `on` G.keyPressEvent $ do
        keyname <- G.eventKeyName
        keychar <- fmap G.keyToChar G.eventKeyVal
        case mapKey keychar keyname of
            Just k -> do
                      handled <- liftIO $ runScreen controls stref $ handleKey k
                      if handled then return True
                                 else return False
            Nothing -> return False

    -- Always scroll to end when new text arrives
    mainView `on` G.sizeAllocate $ \_ -> runScreen controls stref $ do
        endIter <- liftIO $ G.textBufferGetEndIter mainBuffer
        liftIO $ G.textViewScrollToIter mainView endIter 0 Nothing
        return ()

    {-
    window `on` G.configureEvent $ do
            liftIO $ runScreen controls stref $ do
                (w, h) <- getViewSize
                sendSocket $ telnegToBytes $ telnetNegNaws w h
            return False
    -}

    -- Quit when the window is closed
    window `on` G.deleteEvent $ liftIO G.mainQuit >> return False

    G.widgetShowAll window

    return controls

-- | Run the screen
execScreen :: MBConfig -> MBState -> Screen () -> IO ()
execScreen conf initMBState action =
    do
    G.initGUI
    stref <- newIORef $ initState conf initMBState
    --gladepath <- getDataFileName "gui.glade"
    --ctrls <- initUI gladepath stref
    ctrls <- initUI "gui.glade" stref
    runScreen ctrls stref action
    st <- readIORef stref
    case scrSocket st of
        Just sock -> telnetClose sock
        Nothing -> return ()
  where
    initState conf initMBState = ScreenState {
        scrPrompt = "",
        scrMarkedPrompt = "",
        scrNormalBuffer = "",
        scrCommandBuffer = "",
        scrNormalKeybuffer = [],
        scrBindings = Trie.empty,
        --scrMode = NormalMode,
        scrMBConfig = conf,
        scrMBState = initMBState,
        scrSocket = Nothing,
        scrQuit = False
    }

------------------------------------------------------------------------------

handleKey :: Key -> Screen Bool
handleKey key = do
    wasBinding <- handleKeybinding key
    ctrls <- askControls
    st <- get
    liftIO $ G.labelSetText (ctlStatusKeyseq ctrls) $ concat [show x | x <- scrNormalKeybuffer st]
    if wasBinding then return True
                  else case key of
                           KEnter -> do
                                     text <- liftIO $ G.get (ctlMainInput ctrls) G.entryText
                                     case text of
                                        (':':cmd) -> mb $ command cmd
                                        _ -> do
                                             mb $ echoA (setFg Yellow (toAttrString text))
                                             mb $ send text
                                     liftIO $ G.set (ctlMainInput ctrls) [ G.entryText := "" ]
                                     return True
                           _ -> return False

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

bind :: [Key] -> Screen () -> Screen ()
bind keys ac = modify $ \s -> s { scrBindings = Trie.insert (scrBindings s) keys ac }

------------------------------------------------------------------------------

{-
getCharSize :: Screen (Double, Double)
getCharSize = do
    ctls <- askControls
    liftIO $ do
             pc <- G.widgetGetPangoContext (ctlMainView ctls)
             (item:[]) <- P.pangoItemize pc "W" []
             glyph <- P.pangoShape item
             ((P.PangoRectangle _ _ w h), _) <- P.glyphItemExtents glyph
             return (w, h)
        >>= return

getViewSize :: Screen (Int, Int)
getViewSize = do
    ctls <- askControls
    (cw, ch) <- getCharSize
    --(G.Rectangle _ _ w h) <- liftIO $ G.textViewGetVisibleRect (ctlMainView ctls)
    (G.Rectangle _ _ w h) <- liftIO $ G.widgetGetAllocation (ctlMainScroll ctls)
    -- ATTENTION: division may fail!
    return $ (w `div` (ceiling cw), h `div` (ceiling ch))
    -}

-- | Handle telnet negotiations
handleTelneg :: TelnetNeg -> Screen ()
handleTelneg neg = case neg of
    -- WILL EOR
    TelnetNeg (Just CMD_WILL) (Just OPT_EOR) [] ->
        sendSocket $ telnetNegToBytes $ TelnetNeg (Just CMD_DO) (Just OPT_EOR) []        
    -- EOR
    TelnetNeg (Just CMD_EOR) Nothing [] ->
        modify $ \st -> st { scrPrompt = "", scrMarkedPrompt = scrPrompt st }
    {-
    -- NAWS (defunct)
    TelnetNeg (Just CMD_DO) (Just OPT_NAWS) [] -> do
        sendSocket $ telnegToBytes $ TelnetNeg (Just CMD_WILL) (Just OPT_NAWS) []        
        (w, h) <- getViewSize
        sendSocket $ telnegToBytes $ telnetNegNaws w h
    -}
    -- All other telnegs are just printed
    _ ->
        mb $ echoA $ (setFg Magenta $ toAttrString $ show neg)
