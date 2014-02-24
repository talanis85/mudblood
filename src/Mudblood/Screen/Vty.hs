{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, BangPatterns, FlexibleInstances #-}

module Mudblood.Screen.Vty
    ( execScreen
    , Screen (Screen)
    , screen
    , bind
    , menu
    , mb
    , createTextWindow
    , createWidgetWindow
    , prompt
    , setStatus
    ) where

import Data.Char
import Data.Word
import Data.Monoid
import Data.Dynamic
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Trie as Trie

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Free (Free (Free, Pure))

import Control.Concurrent
import Control.Concurrent.STM

import System.IO
import System.Process
import System.Exit
import qualified Graphics.Vty as V

import qualified Codec.Binary.UTF8.String as UTF8

import Mudblood.Core
import Mudblood.Telnet
import Mudblood.Keys
import Mudblood.Text
import Mudblood.UI
import Mudblood.Mapper.Map
import qualified Mudblood.Screen as SC
import Mudblood.Screen (mb_)

import Data.Time.Clock.POSIX

import qualified Data.History as Hist
import qualified Data.PrefixZipper as Z

data Mode u = NormalMode
            -- | CommandMode Buffer
            | PromptMode Buffer String (String -> Screen u ())

data ScreenState u = ScreenState {
    scrPrompt :: String,
    scrMarkedPrompt :: String,

    scrNormalBuffer :: Buffer,
    scrNormalKeybuffer :: [Key],
    scrNormalHistory :: Hist.History String,

    scrLinebuffer :: [AttrString],
    scrScroll :: Int,

    scrWidgets :: MB u [UIWidget],

    scrBindings :: Trie.Trie Key (Last (Screen u ())),
    scrMenu :: Maybe (String, KeyMenu Key (Screen u ())),

    scrMBState :: MBState u,
    scrMBConfig :: MBConfig u,

    scrStatus :: MB u String,

    scrEventChan :: TChan (Event u),
    scrSocket :: Maybe TelnetSocket,

    scrMode :: Mode u,
    scrTime :: Int,

    scrVty :: V.Vty,

    scrQuit :: Bool
    }

newtype Screen u a = Screen (StateT (ScreenState u) IO a)
    deriving (Functor, Monad, MonadIO, MonadState (ScreenState u))

instance SC.ScreenClass u (Screen u) where
    mb = mb
    prompt = prompt
    bind = bind
    setStatus = setStatus
    menu = menu

data Event u = ReceiveEvent String
             | TelnetEvent TelnetNeg
             | CloseEvent
             | KeyEvent Key [KeyMod]
             | FifoEvent String
             | TimeEvent Int
             | MBEvent (MB u ())

mb :: MB u a -> Screen u (Maybe a)
mb mb = do
    st <- get
    ret <- interpMB $ runMB (scrMBConfig st) (scrMBState st) mb
    case ret of
        Left err -> do
            screenEcho $ show err
            return Nothing
        Right (a,s) -> do
            modify $ \st -> st { scrMBState = s }
            return $ Just a
  where
    interpMB (Pure r) = return r
    interpMB (Free (MBFIO action g))            = liftIO action >>= interpMB . g
    interpMB (Free (MBFConnect h p x))          = connectScreen h p >> interpMB x
    interpMB (Free (MBFLine buf l x))           = appendLine l >> interpMB x
    interpMB (Free (MBFSend d x))               = sendSocket d >> interpMB x
    interpMB (Free (MBFQuit x))                 = modify (\st -> st { scrQuit = True }) >> interpMB x
    interpMB (Free (MBFUI a x))                 = execUI a >> interpMB x -- NO OP
    interpMB (Free (MBFDialog desc handler x))  = runDialog desc handler >> interpMB x
    interpMB (Free (MBFGetTime g))              = gets scrTime >>= interpMB . g

mapKey :: V.Key -> Maybe Key
mapKey k = case k of
    V.KPageUp       -> Just KPgUp
    V.KPageDown     -> Just KPgDn
    V.KUp           -> Just KUp
    V.KDown         -> Just KDown
    V.KLeft         -> Just KLeft
    V.KRight        -> Just KRight
    V.KASCII '\t'   -> Just KTab
    V.KEnter        -> Just KEnter
    V.KBS           -> Just KBS
    V.KASCII '\b'   -> Just KBS
    V.KEsc          -> Just KEsc
    V.KFun 1        -> Just KF1
    V.KFun 2        -> Just KF2
    V.KFun 3        -> Just KF3
    V.KFun 4        -> Just KF4
    V.KFun 5        -> Just KF5
    V.KFun 6        -> Just KF6
    V.KFun 7        -> Just KF7
    V.KFun 8        -> Just KF8
    V.KFun 9        -> Just KF9
    V.KFun 10       -> Just KF10
    V.KFun 11       -> Just KF11
    V.KFun 12       -> Just KF12
    V.KASCII c      -> Just $ KAscii c
    _               -> Nothing

mapKeyMod :: V.Modifier -> KeyMod
mapKeyMod m = case m of
    V.MShift    -> MShift
    V.MCtrl     -> MCtrl
    V.MMeta     -> MMeta
    V.MAlt      -> MAlt

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

inputLoop :: V.Vty -> TChan (Event u) -> IO ()
inputLoop v chan =
    do
    forever $ do
              vev <- V.next_event v
              case vev of
                V.EvKey k mod   -> case mapKey k of
                                    Just k' -> liftIO $ atomically $ writeTChan chan $ KeyEvent k' (map mapKeyMod mod)
                                    Nothing -> return ()
                _               -> return ()


-- | Run the screen
execScreen :: MBConfig u -> MBState u -> Screen u () -> IO ()
execScreen conf initMBState action =
    do
    v <- V.mkVty
    chan <- newTChanIO
    forkIO $ inputLoop v chan
    forkIO $ runTimer chan
    runScreen' (initState v conf chan initMBState) action
    V.shutdown v
  where
    runScreen' st (Screen s) = runStateT s st

    initState v conf chan initMBState = ScreenState {
        scrPrompt = "",
        scrMarkedPrompt = "",
        scrNormalBuffer = bufferEmpty,
        scrNormalKeybuffer = [],
        scrNormalHistory = Hist.empty,
        scrWidgets = return [],
        scrBindings = Trie.empty,
        scrMenu = Nothing,
        scrMode = NormalMode,
        scrVty = v,
        scrMBConfig = conf,
        scrMBState = initMBState,
        scrSocket = Nothing,
        scrEventChan = chan,
        scrQuit = False,
        scrLinebuffer = [],
        scrScroll = 0,
        scrTime = 0,
        scrStatus = return ""
    }

-- | Connect to host:port.
connectScreen :: String -> String -> Screen u ()
connectScreen host port =
    do
    state <- get
    case scrSocket state of
        Nothing -> return ()
        Just oldsock -> liftIO $ telnetClose oldsock
    sock <- liftIO . telnetConnect host port $ telnetRecvHandler $ telnetProc (scrEventChan state)
    case sock of
        Right sock' -> modify $ \s -> s { scrSocket = Just sock' }
        Left err -> mb_ $ echoE err
  where
    telnetProc chan ev = case ev of
        TelnetRawEvent s -> liftIO $ telnetReceiveProc chan $ ReceiveEvent $ UTF8.decode s
        TelnetNegEvent n -> liftIO $ telnetReceiveProc chan $ TelnetEvent n
        TelnetCloseEvent reason -> liftIO $ telnetReceiveProc chan $ CloseEvent

    telnetReceiveProc chan ev = atomically $ writeTChan chan ev

sendSocket :: Communication -> Screen u ()
sendSocket dat =
    do
    state <- get
    case (scrSocket state) of
         Just sock -> liftIO $ telnetSend sock dat
         Nothing -> return ()

runTimer :: TChan (Event u) -> IO ()
runTimer chan = forever $ do
    t <- getPOSIXTime
    liftIO $ atomically $ writeTChan chan $ TimeEvent $ floor $ toRational t
    threadDelay 1000000

-- | Screen main loop
screen :: Screen u ()
screen =
    do
    st <- get
    ev <- liftIO $ atomically $ readTChan $ scrEventChan st
    handleEvent ev
    handleAllEvents $ scrEventChan st
    drawScreen
    gets scrQuit >>= (flip when screen) . not
  where
    handleAllEvents chan = do
        nextEv <- liftIO $ atomically $ do
            empty <- isEmptyTChan chan
            if empty then return Nothing else readTChan chan >>= return . Just
        case nextEv of
            Nothing -> return ()
            Just ev -> handleEvent ev >> handleAllEvents chan

    handleEvent ev = do
        st <- get
        case ev of
            ReceiveEvent chars  -> do
                                   ret <- mb $ process (scrPrompt st) chars defaultAttr -- TODO: where to save current attr?
                                   case ret of
                                      Nothing -> return ()
                                      Just (p, _) -> modify $ \st -> st { scrPrompt = p }
            CloseEvent          -> screenEcho "Connection closed"
            KeyEvent k mod      -> handleKey k mod
            TelnetEvent neg     -> do
                                   handleTelneg neg
                                   --mb $ logger LogDebug $ show neg
            TimeEvent t         -> do
                                   modify $ \st -> st { scrTime = t }
                                   mb_ $ processTime t
            MBEvent action      -> mb_ action
            _                   -> return ()

appendLine :: AttrString -> Screen u ()
appendLine line = modify $ \st -> st { scrLinebuffer = line : (scrLinebuffer st) }

screenEcho :: String -> Screen u ()
screenEcho str = appendLine (toAS str)

handleTelneg :: TelnetNeg -> Screen u ()
handleTelneg neg = do
    case neg of
        TelnetNeg (Just CMD_EOR) Nothing [] -> do
            modify $ \st -> st { scrPrompt = "", scrMarkedPrompt = scrPrompt st }
            --screenEcho $ show neg
        TelnetNeg (Just CMD_WILL) (Just OPT_EOR) [] ->
            mb_ $ send $ TelnetNeg (Just CMD_DO) (Just OPT_EOR) []        
        _ -> return ()
    mb_ $ processTelnet neg

execUI :: UIAction (MB u) -> Screen u ()
execUI (UISetCompletion comp) = modify $ \s -> s { scrNormalBuffer = bufferSetCompletion comp (scrNormalBuffer s) }
execUI UIBell = do
    liftIO $ runInteractiveCommand $ "aplay /home/philip/res/synth1.wav"
    return ()
execUI _ = return ()

handleBufferKey :: Buffer -> Key -> Maybe Buffer
handleBufferKey buf k = case k of
    KBS       -> Just $ bufferDeleteLeft buf
    KLeft     -> Just $ bufferModifyCursor (max 0 . (flip (-) 1)) buf
    KRight    -> Just $ bufferModifyCursor (min (length $ bufferContent buf) . ((+) 1)) buf
    KAscii k' -> Just $ bufferInsert k' buf
    KTab      -> if bufferCompletionState buf
                    then Just $ bufferUpdateContent $ bufferModifyCompletion Z.right buf
                    else Just $ bufferUpdateContent $ bufferSetCompletionState True buf
    KEsc      -> Just $ bufferSetCompletionState False buf
    _         -> Nothing

handleKey :: Key -> [KeyMod] -> Screen u ()
handleKey k m =
    do

    when ((k, m) == (KAscii 'c', [MCtrl])) $ modify $ \s -> s { scrQuit = True }

    st <- get
    case (scrMode st) of
        NormalMode -> do
            wasBinding <- handleKeybinding k
            if wasBinding
                then return ()
                else case k of
                    KEnter      -> do
                                   let bufcont = bufferContent $ scrNormalBuffer st

                                   appendLine $
                                      (toAS $ (scrMarkedPrompt st) ++ (scrPrompt st))
                                      `mappend`
                                      (setFg Yellow $ toAS $ bufcont)

                                   case bufcont of
                                      ('(':_) -> mb $ command bufcont
                                      _       -> mb $ processSend bufcont

                                   modify $ \s -> s
                                        { scrNormalBuffer = bufferSetContent "" (scrNormalBuffer st)
                                        , scrPrompt = ""
                                        , scrNormalHistory = Hist.insert bufcont $ Hist.rewind $ scrNormalHistory s
                                        }
                    KPgUp       -> modify $ \s -> s { scrScroll = (scrScroll s + 20) }
                    KPgDn       -> modify $ \s -> s { scrScroll = max 0 (scrScroll s - 20) }
                    KUp         -> let newhist = Hist.up $ scrNormalHistory st
                                   in modify $ \s -> s { scrNormalHistory = newhist
                                                       , scrNormalBuffer = bufferSetContent
                                                            (fromMaybe "" (Hist.cursor newhist))
                                                            (scrNormalBuffer s)
                                                       }
                    KDown       -> let newhist = Hist.down $ scrNormalHistory st
                                   in modify $ \s -> s { scrNormalHistory = newhist
                                                       , scrNormalBuffer = bufferSetContent
                                                            (fromMaybe "" (Hist.cursor newhist))
                                                            (scrNormalBuffer s)
                                                       }
                    _           -> case handleBufferKey (scrNormalBuffer st) k of
                                            Just buf' -> modify $ \s -> s { scrNormalBuffer = buf' }
                                            Nothing -> return ()
        PromptMode buf title action ->
            case k of
                KEsc        -> modify $ \s -> s { scrMode = NormalMode }
                KEnter      -> do
                               modify $ \s -> s { scrMode = NormalMode }
                               action (bufferContent buf)
                _           -> case handleBufferKey buf k of
                                        Just buf' -> modify $ \s -> s { scrMode = PromptMode buf' title action }
                                        Nothing -> return ()

handleMenu :: Key -> Screen u Bool
handleMenu k = do
    st <- get
    case (scrMenu st) of
        Nothing -> return False
        Just (_, menu) ->
            case stepMenu k menu of
                Just (desc, KeyAction action) ->
                    action >> modify (\s -> s { scrMenu = Nothing }) >> return True
                Just (desc, KeyMenu m) ->
                    modify (\s -> s { scrMenu = Just (desc, KeyMenu m) }) >> return True
                Nothing ->
                    modify (\s -> s { scrMenu = Nothing }) >> return False

handleKeybinding :: Key -> Screen u Bool
handleKeybinding k = do
    didMenu <- handleMenu k
    if didMenu
        then return True
        else do
            st <- get
            let kb = (scrNormalKeybuffer st) ++ [k]
            if Trie.isPrefix (scrBindings st) kb
                then do
                     case Trie.lookup (scrBindings st) kb of
                            Last Nothing -> modify (\s -> s { scrNormalKeybuffer = kb })
                            Last (Just ac) -> ac >> modify (\s -> s { scrNormalKeybuffer = [] })
                     return True
                else modify (\s -> s { scrNormalKeybuffer = [] }) >> return False

bind :: [Key] -> Screen u () -> Screen u ()
bind keys ac = modify $ \s -> s { scrBindings = Trie.insert keys (Last $ Just ac) (scrBindings s) }

menu :: String -> KeyMenu Key (Screen u ()) -> Screen u ()
menu desc m = modify $ \s -> s { scrMenu = Just (desc, m) }

-- | Omg, this is soooo ugly... :(
drawScreen :: Screen u ()
drawScreen =
    do
    scrst <- get

    display_region <- V.display_bounds (V.output_iface (scrVty scrst))
    let (w, h) = ((fromIntegral $ fst display_region), (fromIntegral $ snd display_region))

    statusLines <- mb $ scrStatus scrst

    let all_lines = concat $ map (reverse . wrapAS (w - 5)) $ scrLinebuffer scrst
    let lines = map (drawLine scrst) (reverse $ take (h-3) $ drop (scrScroll scrst) all_lines)
    let img_lines = if length lines < (h-3)
                        then mconcat $ (V.background_fill w (h - 3 - length lines)) : lines
                        else mconcat lines
    let img_prompt = drawPrompt scrst
    let img_status = drawStatus scrst (fromMaybe "" statusLines)

    let cursor = case (scrMode scrst) of
            NormalMode ->
                V.Cursor (length (scrMarkedPrompt scrst) + length (scrPrompt scrst) + bufferCursor (scrNormalBuffer scrst)) (V.image_height img_lines)
            PromptMode buf title _ ->
                V.Cursor (fromIntegral $ min (bufferCursor buf + length title + 2) 20) (V.image_height img_lines + 2)
            _ -> V.NoCursor

    {-
    let (a,b,c) = Z.toParts $ bufferCompletionList $ scrNormalBuffer scrst
        compList = case b of
            Nothing -> a ++ [""] ++ c
            Just b' -> a ++ [b'] ++ c
        completion_h = min 10 $ length compList
        completion_w = min 5 $ maximum $ map length compList
        compAttr = V.def_attr `V.with_style` V.reverse_video
        drawCompNormal str = V.string compAttr $ str ++ take (completion_w - length str) (repeat ' ')
        drawCompFocus s = V.string (compAttr `V.with_fore_color` V.green) $ case s of
            Just str -> str ++ take (completion_w - length str) (repeat ' ')
            Nothing -> take completion_w (repeat ' ')
        completion_lines = (map drawCompNormal $ take 4 a) ++ [(drawCompFocus b)] ++ (map drawCompNormal $ take 5 c)
        img_completion = mconcat completion_lines
        img_completion_layer =
            if bufferCompletionState (scrNormalBuffer scrst)
                then [V.translate_y (h - completion_h - 3) img_completion]
                else []
    -}
    let img_completion_layer = if bufferCompletionState (scrNormalBuffer scrst)
                then let (a,b,c) = Z.toParts $ bufferCompletionList $ scrNormalBuffer scrst
                         list = drawList a b c
                     in [V.translate_y (h - V.image_height list - 3) list]
                else []

    let img_menu = case (scrMenu scrst) of
            Nothing -> []
            Just (desc, menu) ->
                let drawn_menu = drawList [] Nothing $ map (\(k,d) -> (show k) ++ ": " ++ d) (showMenu menu)
                in [V.translate_y (h - V.image_height drawn_menu - 3) drawn_menu]

    m <- mb $ getMap

    let sidebar_w = 35

    let img_map = case m of
            Just m -> V.translate_x (w - sidebar_w) $ drawMap sidebar_w 25 m
            Nothing -> V.background_fill sidebar_w 25

    widgets <- gets scrWidgets >>= mb

    let img_widgets = V.translate (w - sidebar_w) 30 $ fromMaybe (V.background_fill 30 1) $ fmap drawWidgets widgets

    liftIO $ V.update (scrVty scrst) (V.Picture
        cursor
        (img_menu ++ img_completion_layer ++ [img_map, img_widgets, (img_lines V.<-> img_prompt V.<-> (V.char_fill V.def_attr '-' w 1) V.<-> img_status)])
        (V.Background ' ' V.def_attr)
        )

padRight :: Int -> a -> [a] -> [a]
padRight n c l =
    if length l < n
        then l ++ (take (n - length l) $ repeat c)
        else take n l

drawList :: [String] -> Maybe String -> [String] -> V.Image
drawList a b c =
    let fullList = a ++ maybeToList b ++ c
        maximum' l = if l == [] then 0 else maximum l
        w = maximum' (map length fullList)
        h = length fullList
        attr = V.def_attr `V.with_style` V.reverse_video
        selectedAttr = V.def_attr `V.with_style` V.reverse_video `V.with_fore_color` V.blue
        innerList = case b of
            Nothing ->
               (V.vert_cat $ map (V.string attr . (\s -> "  " ++ padRight w ' ' s ++ "  ")) (a ++ c))
            Just b' ->
               (V.vert_cat $ map (V.string attr . (\s -> "  " ++ padRight w ' ' s ++ "  ")) a)
               V.<->
               (V.string selectedAttr $ "  " ++ padRight w ' ' b' ++ "  ")
               V.<->
               (V.vert_cat $ map (V.string attr . (\s -> "  " ++ padRight w ' ' s ++ "  ")) c)
    in (V.string attr $ take (w + 4) $ repeat ' ')
       V.<->
       innerList
       V.<->
       (V.string attr $ take (w + 4) $ repeat ' ')

minImage :: V.Image -> V.Image
minImage img = if (V.image_width img) == 0 || (V.image_height img) == 0 then V.background_fill 1 1 else img

a <!> b
    | validImg a && validImg b = a V.<|> b
    | validImg a = a
    | validImg b = b
    | otherwise = V.empty_image
  where
    validImg a = V.image_width a > 0 && V.image_height a > 0

drawMap :: Int -> Int -> Map -> V.Image
drawMap w h m = mconcat $ map (V.string V.def_attr) $ mapDrawAscii w h ["base"] m

drawLine :: ScreenState u -> AttrString -> V.Image
drawLine s l = minImage $ V.horiz_cat $ map drawChunk (groupAS $ untabAS 8 l)
    where drawChunk (c, a) = V.string (V.Attr (mapStyle $ attrStyle a) (mapColor $ attrFg a) (mapColor $ attrBg a)) c

drawPrompt :: ScreenState u -> V.Image
drawPrompt s = V.string V.def_attr (scrMarkedPrompt s)
               <!>
               V.string V.def_attr (scrPrompt s)
               <!>
               V.string (V.with_fore_color V.def_attr V.yellow) (bufferContent $ scrNormalBuffer s)

drawStatus :: ScreenState u -> String -> V.Image
drawStatus s status =
    case (scrMode s) of
        PromptMode buf title _ ->
            (V.resize_width 20 $ V.crop_left 20 $ V.string V.def_attr (title ++ ": " ++ (bufferContent buf)))
        _ ->
            (V.background_fill 20 1)
    <!>
    V.string V.def_attr status

drawWidgets :: [UIWidget] -> V.Image
drawWidgets = mconcat . map ((V.<|> V.string V.def_attr " ") . drawWidget)

drawWidget :: UIWidget -> V.Image
drawWidget (UIWidgetText str) = V.string V.def_attr str
drawWidget (UIWidgetTable tab) = mconcat $ map drawLine $ rectify " " (maximum $ map length tab) tab
  where
    drawLine l = V.horiz_cat $ map (V.string V.def_attr . (++ " ")) l
    rectify elem len mat = map (fill elem len) mat
    fill elem len l = l ++ (take (max 0 (len - length l)) $ repeat elem)

createWidgetWindow :: String -> MB u [UIWidget] -> Screen u ()
--createWidgetWindow name widgets = screenEcho "This screen does not support windows"
createWidgetWindow name action = modify $ \s -> s { scrWidgets = action }

createTextWindow :: String -> String -> Screen u ()
createTextWindow winname name = screenEcho "This screen does not support windows"

prompt :: String -> (String -> Screen u ()) -> Screen u ()
prompt title f = modify $ \st -> st { scrMode = PromptMode bufferEmpty title f }

setStatus :: MB u String -> Screen u ()
setStatus action = modify $ \s -> s { scrStatus = action }

-----------------------------------------------------------------------------
-- INPUT BUFFER
-----------------------------------------------------------------------------

data Buffer = Buffer
    { bufferContent :: String
    , bufferCursor :: Int
    , bufferCompletionList :: Z.PrefixZipper Char
    , bufferCompletionState :: Bool
    }

bufferEmpty :: Buffer
bufferEmpty = Buffer
    { bufferContent = ""
    , bufferCursor = 0
    , bufferCompletionList = Z.empty
    , bufferCompletionState = False
    }

bufferModifyCursor :: (Int -> Int) -> Buffer -> Buffer
bufferModifyCursor f buf = buf
    { bufferCursor = f (bufferCursor buf) }

bufferSetContent :: String -> Buffer -> Buffer
bufferSetContent str buf = bufferUpdateCompletion $ buf
    { bufferContent = str
    , bufferCursor = min (length str) (bufferCursor buf)
    , bufferCompletionState = False
    }

bufferInsert :: Char -> Buffer -> Buffer
bufferInsert char buf =
    let cursor = bufferCursor buf
        content = bufferContent buf
    in bufferUpdateCompletion $ buf
        { bufferContent = take cursor content ++ [char] ++ drop cursor content
        , bufferCursor = cursor + 1
        }

bufferDeleteLeft :: Buffer -> Buffer
bufferDeleteLeft buf =
    let cursor = bufferCursor buf
        content = bufferContent buf
    in bufferUpdateCompletion $ buf
        { bufferContent = take (cursor - 1) content ++ drop cursor content
        , bufferCursor = max 0 (cursor - 1)
        }

bufferDeleteRight :: Buffer -> Buffer
bufferDeleteRight buf =
    let cursor = bufferCursor buf
        content = bufferContent buf
    in bufferUpdateCompletion $ buf
        { bufferContent = take cursor content ++ drop (cursor + 1) content
        , bufferCursor = min (length content) (cursor + 1)
        }

bufferUpdateCompletion :: Buffer -> Buffer
bufferUpdateCompletion buf = bufferModifyCompletion (Z.setPrefix (bufferContent buf)) buf

bufferUpdateContent :: Buffer -> Buffer
bufferUpdateContent buf = case Z.cursor (bufferCompletionList buf) of
    Nothing -> buf
    Just c  -> buf { bufferContent = c, bufferCursor = (length c) }

bufferSetCompletion :: [String] -> Buffer -> Buffer
bufferSetCompletion comp buf = bufferUpdateCompletion $ buf
    { bufferCompletionList = Z.fromList comp
    , bufferCompletionState = False
    }

bufferSetCompletionState :: Bool -> Buffer -> Buffer
bufferSetCompletionState v buf = buf { bufferCompletionState = v }

bufferModifyCompletion :: (Z.PrefixZipper Char -> Z.PrefixZipper Char) -> Buffer -> Buffer
bufferModifyCompletion f buf = buf { bufferCompletionList = f (bufferCompletionList buf) }

-----------------------------------------------------------------------------
-- DIALOGS
-----------------------------------------------------------------------------

runDialog :: UIDialogDescription -> (UIDialogResult -> MB u ()) -> Screen u ()
runDialog (UITextDialogDescription content) handler = do
    chan <- gets scrEventChan
    void $ liftIO $ forkIO $ do
        writeFile "/tmp/mbtmp" content
        (_, _, _, p) <- createProcess $ shell "gvim --nofork /tmp/mbtmp"
        code <- waitForProcess p
        if code == ExitSuccess
            then do
                 newcontent <- readFile "/tmp/mbtmp"
                 atomically $ writeTChan chan $ MBEvent $ handler $ UITextDialogResult newcontent
            else return ()
runDialog _ _ = screenEcho "Unsupported dialog type"
