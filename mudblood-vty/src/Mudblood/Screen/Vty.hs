{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
 , DeriveDataTypeable, ExistentialQuantification, ScopedTypeVariables
 , TemplateHaskell
 , TypeOperators #-}

module Mudblood.Screen.Vty
    ( VtyScreen
    , run
    , runEditor, runCmd
    , updateStatus, updateSidebar
    , select
    , setCompletion
    , pasteMode
    , widgetMode

    , Layout (..)

    , module Mudblood.Screen.Vty.UserWidget
    , module Mudblood.Screen.Vty.Monad
    , module Control.Interactive

    ) where

import           Data.Bifunctor
import           Data.Char
import           Data.Carte
import           Data.Word
import           Data.Monoid
import           Data.Dynamic
import           Data.Maybe
import qualified Data.Map as M
import           Data.Foldable (toList)
import qualified Data.ListZipper as LZ
import           Data.Buffer
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Menu
import qualified Data.Sequence as Seq
import qualified Data.History as Hist
import qualified Data.PrefixZipper as Z
import qualified Data.Trie as Trie

import           Text.Printf

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Error
import           Control.Exception (try)
import           Control.Concurrent
import           Control.Concurrent.STM

import           System.IO
import           System.Locale
import           System.Process
import           System.Exit

import qualified Graphics.Vty as V
import           Graphics.Vty.Widget

import           Mudblood.Monad
import           Mudblood.Class
import           Mudblood.Component
import           Mudblood.Core
import           Mudblood.Screen
import           Mudblood.Encoding
import           Mudblood.Telnet
import           Mudblood.Keys
import           Mudblood.Text
import           Mudblood.Error
import           Mudblood.Trigger
import           Mudblood.Trigger.Event

import           Mudblood.Screen.Vty.Monad
import           Mudblood.Screen.Vty.Keys
import           Mudblood.Screen.Vty.Widgets
import           Mudblood.Screen.Vty.Layout
import           Mudblood.Screen.Vty.Draw
import           Mudblood.Screen.Vty.Select
import           Mudblood.Screen.Vty.Lines
import           Mudblood.Screen.Vty.UserWidget

import           Control.UnsafeCallback
import           Control.Interactive


import qualified Codec.Binary.UTF8.String as UTF8

-----------------------------------------------------------------------------

type VMBR e u = MBR (Maybe (UserWidget (MB u VtyScreen))) e u VtyScreen

instance Screen VtyScreen where
    outputS o    = case o of
        OutputLine s  -> mapM_ (appendLine 0) (linesAS s)
        OutputError e -> appendLine 0 $ toAS $ "!! " ++ show e
        OutputLog l   -> appendLine 1 $ toAS $ "-- " ++ l
        OutputInfo l  -> appendInfoLine l
    sendS s      = sendSocket $ Communication s
    setPromptS p = scrMarkedPrompt .= p
    connectS h p = connectToHost h p
    timeS        = use scrTime
    setStatusS s = updateStatus s
    {-
    prompt p f  = lift $ scrMode .= (PromptMode bufferEmpty p $ unsafeCallback f)
    bind k a    = lift $ scrBindings %= Trie.insert k (Last $ Just $ unsafeCallback (\(x :: ()) -> a))
    menu m      = lift $ scrMenu .= (Just $ fmap (unsafeCallback . const) m)
    -}

prompt p f  = lift $ scrMode .= (PromptMode bufferEmpty p $ unsafeCallback f)

select :: String -> ([String] -> MB u VtyScreen ()) -> MB u VtyScreen ()
select p f = lift $ do
    debugLevel <- use scrDebugLevel
    linebuffer <- use scrLinebuffer

    let savedLines = filterDebug debugLevel $
                     prepareLines $ LZ.listRight linebuffer
    scrMode .= SelectMode bufferEmpty p savedLines (unsafeCallback f)

setCompletion :: [String] -> MB u VtyScreen ()
setCompletion comp = lift $ do
    oldBuffer <- use scrNormalBuffer
    scrNormalBuffer .= bufferSetCompletion comp oldBuffer
    needUpdate

pasteMode :: VtyScreen ()
pasteMode = scrMode .= NormalMode True

widgetMode :: VtyScreen ()
widgetMode = scrMode .= WidgetMode

{-
setUserWidget :: UserWidget (MB u VtyScreen) -> MBX e u VtyScreen ()
setUserWidget w = lift $ scrUserWidget .= Just (toUnsafeUserWidget w)
-}

-----------------------------------------------------------------------------

inputLoop :: V.Vty -> TChan Event -> IO ()
inputLoop v chan = do
    forever $ do
        vev <- V.nextEvent v
        case vev of
            V.EvKey k mod -> do
                case mapKey k of
                    Just k' -> do
                        liftIO $ atomically $ writeTChan chan $
                            SKeyEvent k' (map mapKeyMod mod)
                    Nothing -> return ()
            V.EvResize _ _ -> do
                liftIO $ atomically $ writeTChan chan $ SResizeEvent

timerLoop :: TChan Event -> IO ()
timerLoop chan = forever $ do
    t <- getPOSIXTime
    liftIO $ atomically $ writeTChan chan $ STimeEvent $ floor $ toRational t
    threadDelay 1000000

-----------------------------------------------------------------------------

initScreen :: IO VtyScreenState
initScreen = do
    cfg <- V.standardIOConfig
    v <- V.mkVty cfg
    chan <- newTChanIO
    forkIO $ inputLoop v chan
    forkIO $ timerLoop chan
    t <- getZonedTime
    return $ VtyScreenState
        { _scrPrompt = []
        , _scrMarkedPrompt = ""
        , _scrNormalKeybuffer = []
        , _scrNormalBuffer = bufferEmpty
        , _scrNormalHistory = Hist.empty
        , _scrBindings = Trie.empty
        , _scrMenu = Nothing
        , _scrMode = NormalMode False
        , _scrVty = v
        , _scrDebugLevel = 0
        , _scrSidebar = LayoutText $ toAS ""
        , _scrStatus = ""
        , _scrSocket = Nothing
        , _scrEventChan = chan
        , _scrQuit = False
        , _scrLinebuffer = LZ.fromList $ repeat ((0, t), Right mempty) -- TODO: Use Stream instead of List
        , _scrCurrentAttr = defaultAttr
        , _scrTime = 0
        , _scrUpdate = True
    }

needUpdate :: VtyScreen ()
needUpdate = scrUpdate .= True

run :: (MBEvent e) => Maybe (UserWidget (MB u VtyScreen)) -> MBComponent VtyScreen e () u -> IO ()
run widget component = do
    st <- initScreen
    void $ evalVtyScreen (runWithComponent component widget runner) st

showError :: (Show a) => a -> VtyScreen ()
showError = appendLine 0 . toAS . ("ERROR: " ++) . show

runner :: (MBEvent e) => VMBR e u ()
runner = do
    chan <- lift $ use scrEventChan
    ev   <- lift $ liftIO $ atomically $ readTChan chan
    handleEvent ev
    handleAllEvents chan

    renderUserWidget

    status <- getStatus
    lift $ scrStatus .= status

    needUpdate <- lift $ use scrUpdate
    when needUpdate $ lift drawScreen

    wantsToQuit <- lift $ use scrQuit
    if wantsToQuit
        then do
            vty <- lift $ use scrVty
            lift $ liftIO $ V.shutdown vty
        else do
            runner
  where
    handleAllEvents chan = do
        nextEv <- lift $ liftIO $ atomically $ do
            empty <- isEmptyTChan chan
            if empty
                then return Nothing
                else readTChan chan >>= return . Just
        case nextEv of
            Nothing -> return ()
            Just ev -> handleEvent ev >> handleAllEvents chan

    handleEvent ev = handleEvent' ev `catchError` (lift . showError)
    handleEvent' ev = do
        case ev of
            SReceiveEvent chars  -> do
                oldPrompt <- lift $ use scrPrompt
                oldAttr   <- lift $ use scrCurrentAttr

                let (lines, newPrompt, newAttr) = decode oldPrompt chars oldAttr

                mapM_ (triggerWithDefault defaultHandler . mkEv . LineEvent) lines

                lift $ do
                    scrPrompt .= newPrompt
                    scrCurrentAttr .= newAttr
                    needUpdate
            SCloseEvent -> do
                liftMBR $ echo $ toAS "Connection closed"
            SKeyEvent k mod -> do
                handleKey k mod
            STelnetEvent neg -> do
                handleTelneg neg
            STimeEvent t -> do
                lift $ scrTime .= t
                triggerWithDefault defaultHandler $ mkEv $ TimeEvent t
            SInteractiveEvent s -> do
                lift $ appendLine 0 $ toAS s
            SEndInteractiveEvent -> do
                mode <- lift $ use scrMode
                case mode of
                    InteractiveMode buf h cb -> do
                        lift $ scrMode .= NormalMode False
                        liftMBR $ runUnsafeCallback cb ()
                    _ -> throwError (stackTrace "screen" "Received EndInteractiveEvent without being in InteractiveMode")
            SResizeEvent -> do
                lift needUpdate
            _ -> return ()

renderUserWidget :: VMBR e u ()
renderUserWidget = do
    widget <- mbrGetExtra
    mode   <- lift $ use scrMode
    case widget of
        Nothing -> lift $ scrSidebar .= LayoutEmpty
        Just widget' -> do
            layout  <- case mode of
                WidgetMode -> liftMBR $ current widget' True
                _          -> liftMBR $ current widget' False
            lift $ scrSidebar .= layout

handleUserWidgetKey :: Key -> VMBR e u Bool
handleUserWidgetKey k = do
    widget <- mbrGetExtra
    case widget of
        Nothing -> return False
        Just widget' -> do
            case transition widget' k of
                Nothing -> return False
                Just t -> do
                    newWidget <- liftMBR t
                    mbrPutExtra (Just newWidget)
                    return True

connectToHost :: String -> String -> VtyScreen ()
connectToHost host port = do
    oldSocket <- use scrSocket
    chan      <- use scrEventChan
    case oldSocket of
        Nothing        -> return ()
        Just oldSocket -> liftIO $ telnetClose oldSocket
    newSocket <- liftIO $ telnetConnect host port $ telnetRecvHandler $ telnetProc chan
    case newSocket of
        Right newSocket -> scrSocket .= Just newSocket
        Left err        -> showError err
  where
    telnetProc chan ev = case ev of
        TelnetRawEvent s -> liftIO $ telnetReceiveProc chan $ SReceiveEvent s
        TelnetNegEvent n -> liftIO $ telnetReceiveProc chan $ STelnetEvent n
        TelnetCloseEvent reason -> liftIO $ telnetReceiveProc chan $ SCloseEvent

    telnetReceiveProc chan ev = atomically $ writeTChan chan ev

sendSocket :: Communication -> VtyScreen ()
sendSocket dat = do
    socket <- use scrSocket
    case socket of
         Just socket -> liftIO $ telnetSend socket dat
         Nothing     -> return ()

linebufferBound = 10000

appendLine :: Int -> AttrString -> VtyScreen ()
appendLine debug line = do
    t <- liftIO $ getZonedTime
    scrLinebuffer %= LZ.cons' ((debug, t), Right line)
    needUpdate

appendInfoLine :: String -> VtyScreen ()
appendInfoLine line = do
    t <- liftIO $ getZonedTime
    scrLinebuffer %= LZ.cons' ((0, t), Left line)
    needUpdate

handleTelneg :: (MBEvent e) => TelnetNeg -> VMBR e u ()
handleTelneg neg = do
    lift $ appendLine 3 $ setFg Magenta $ toAS $ case telnetToGMCP neg of
        Nothing -> show neg
        Just gmcp -> show gmcp
    case neg of
        TelnetNeg (Just CMD_EOR) Nothing [] -> do
            p <- lift $ use scrPrompt
            lift $ scrPrompt .= []
            triggerWithDefault defaultHandler $ mkEv $ PromptEvent (escapeAll p)
        TelnetNeg (Just CMD_WILL) (Just OPT_EOR) [] ->
            liftMBR $ send $ TelnetNeg (Just CMD_DO) (Just OPT_EOR) []
        _ -> return ()
    case telnetToGMCP neg of
        Nothing   -> triggerWithDefault defaultHandler $ mkEv $ TelnetEvent neg
        Just gmcp -> triggerWithDefault defaultHandler $ mkEv $ GMCPEvent gmcp

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

handleKey :: (MBEvent e) => Key -> [KeyMod] -> VMBR e u ()
handleKey k m = do
    when ((k, m) == (KAscii 'c', [MCtrl])) $ lift $ scrQuit .= True

    lift needUpdate

    mode <- lift $ use scrMode
    case mode of
        NormalMode pasteMode -> do
            wasBinding <- if pasteMode then return False else handleKeybinding k
            if wasBinding
                then return ()
                else case k of
                    KEsc -> do
                        lift $ scrMode .= NormalMode False
                    KEnter -> do
                        buffer       <- lift $ use scrNormalBuffer
                        markedPrompt <- lift $ use scrMarkedPrompt
                        curPrompt    <- lift $ use scrPrompt

                        lift $ appendLine 0 $
                            (toAS $ markedPrompt ++ escapeAll curPrompt) <> (setFg Yellow $ toAS $ bufferContent buffer)

                        case bufferContent buffer of
                            ('/':cmd) -> do
                                command' defaultHandler cmd `catchError` (lift . showError)
                            _ -> do
                              triggerWithDefault defaultHandler $ mkEv $ SendEvent $ bufferContent buffer

                        lift $ do
                            scrNormalBuffer %= bufferSetContent ""
                            scrPrompt .= []
                            scrNormalHistory %= Hist.insert (bufferContent buffer)
                    KPgUp -> do
                        lift $ scrLinebuffer %= LZ.rights 20
                    KPgDn -> do
                        lift $ scrLinebuffer %= LZ.lefts 20
                    KHome -> do
                        lift $ scrLinebuffer %= LZ.start
                    KUp -> do
                        lift $ scrNormalHistory %= Hist.up
                        newHistory <- lift $ use scrNormalHistory
                        lift $ scrNormalBuffer %= bufferSetContent (fromMaybe "" (Hist.cursor newHistory))
                    KDown -> do
                        lift $ scrNormalHistory %= Hist.down
                        newHistory <- lift $ use scrNormalHistory
                        lift $ scrNormalBuffer %= bufferSetContent (fromMaybe "" (Hist.cursor newHistory))
                    _ -> do
                        buffer <- lift $ use scrNormalBuffer
                        case handleBufferKey buffer k of
                            Just buffer' -> lift $ scrNormalBuffer .= buffer'
                            Nothing      -> return ()
        PromptMode buffer title callback -> do
            case k of
                KEsc -> do
                    lift $ scrMode .= NormalMode False
                KEnter -> do
                    lift $ scrMode .= NormalMode False
                    runUnsafeCallback callback $ bufferContent buffer
                _ -> do
                    case handleBufferKey buffer k of
                        Just buffer' -> lift $ scrMode .= PromptMode buffer' title callback
                        Nothing      -> return ()
        InteractiveMode buffer handle callback -> do
            case k of
                KEnter -> do
                    lift $ sendInteractive handle $ bufferContent buffer
                    lift $ scrMode .= InteractiveMode bufferEmpty handle callback
                _ -> do
                    case handleBufferKey buffer k of
                        Just buffer' -> lift $ scrMode .= InteractiveMode buffer' handle callback
                        Nothing      -> return ()
        SelectMode buffer title lines callback -> do
            case k of
                KEsc -> do
                    lift $ scrMode .= NormalMode False
                KEnter -> do
                    lift $ scrMode .= NormalMode False
                    (w, h) <- lift getScreenSize
                    liftMBR $ runUnsafeCallback callback $ reverse $ selectWithFormula (bufferContent buffer) $ map (\(a,(b,c)) -> fromAS c) lines
                _ -> case handleBufferKey buffer k of
                    Just buffer' -> lift $ scrMode .= SelectMode buffer' title lines callback
                    Nothing      -> return ()
        WidgetMode -> do
            case k of
                KEsc -> lift $ scrMode .= NormalMode False
                _    -> void $ handleUserWidgetKey k

getScreenSize :: VtyScreen (Int, Int)
getScreenSize = do
    vty <- use scrVty
    displayRegion <- V.displayBounds (V.outputIface vty)
    return ((fromIntegral $ fst displayRegion), (fromIntegral $ snd displayRegion))


runInteractive :: CreateProcess -> UnsafeCallback () -> VtyScreen ()
runInteractive process callback = do
    chan <- use scrEventChan
    (Just hin, Just hout, Just herr, hproc) <- liftIO $
        createProcess (process { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe })

    scrMode .= InteractiveMode bufferEmpty (InteractiveHandle hin hout herr hproc) callback

    void $ liftIO $ forkIO $ fix $ \loop -> do
        l <- try (hGetLine hout) :: IO (Either IOError String)
        case l of
            Left err -> return ()
            Right l -> do
                atomically $ writeTChan chan $ SInteractiveEvent l
                loop
        atomically $ writeTChan chan $ SEndInteractiveEvent

sendInteractive :: InteractiveHandle -> String -> VtyScreen ()
sendInteractive (InteractiveHandle h _ _ _) s = do
    appendLine 0 $ (toAS "] ") <> (setFg Yellow $ toAS s)
    liftIO $ hPutStrLn h s
    liftIO $ hFlush h

handleTopMenu :: (MBEvent e) => Key -> VMBR e u (Bool, Bool)
handleTopMenu k = do
    menu <- do
        m <- lift $ use scrMenu
        case m of
            Nothing -> fmap (unsafeCallback . const) <$> getBindings
            Just m' -> return m'

    case stepMenu k menu of
        Just m -> case matchMenu m of
            Left action -> do
                lift $ scrMenu .= Nothing
                case matchAction (runUnsafeCallback action ()) of
                    Left action -> do
                        mbx' defaultHandler action
                    Right actionFun -> do
                        prompt (menuTitle m) (mbx' defaultHandler . actionFun)
                return (True, False)
            Right xs -> do
                lift $ scrMenu .= Just m
                return (True, False)
        Nothing -> do
            lift $ scrMenu .= Nothing
            return (False, True)

{-
handleMenu :: Key -> MBRunner e u VtyScreen (Bool, Bool)
handleMenu k = do
    menu <- lift $ use scrMenu
    case menu of
        Nothing -> do
            return (False, False)
        Just (_, menu) -> do
            case stepMenu k menu of
                Just (desc, MenuLeaf action) -> do
                    lift $ scrMenu .= Nothing
                    liftMBRunner $ runUnsafeCallback action ()
                    return (True, False)
                Just (desc, MenuNode m) -> do
                    lift $ scrMenu .= Just (desc, MenuNode m)
                    return (True, False)
                Nothing -> do
                    lift $ scrMenu .= Nothing
                    return (False, True)
-}

handleKeybinding :: (MBEvent e) => Key -> VMBR e u Bool
handleKeybinding k = do
    (didMenu, menuClosed) <- handleTopMenu k
    case (didMenu, menuClosed) of
        (True, False) -> do
            return True
        (False, True) -> do
            return False
        _ -> return False
        {-
        _ -> do
            buffer <- lift $ use scrNormalKeybuffer
            bindings <- lift $ use scrBindings
            let newBuffer = buffer ++ [k]
            if Trie.isPrefix bindings newBuffer
                then do
                    case Trie.lookup bindings newBuffer of
                        Last Nothing -> do
                            lift $ scrNormalKeybuffer .= newBuffer
                        Last (Just ac) -> do
                            lift $ scrNormalKeybuffer .= []
                            liftMBRunner $ runUnsafeCallback ac ()
                    return True
                else do
                    lift $ scrNormalKeybuffer .= []
                    return False
                    -}

drawScreen :: VtyScreen ()
drawScreen = do
    let timestampW = 10
        infoW      = 10

    vty           <- use scrVty
    displayRegion <- V.displayBounds (V.outputIface vty)
    let (w, h)    = ((fromIntegral $ fst displayRegion), (fromIntegral $ snd displayRegion))

    pic <- draw [ bottomleft $ padded 2 0 0 2 wCompletion
                , bottomleft $ padded 2 0 0 2 wMenu
                , makeLayout (w - timestampW - infoW - 80)
                ] w h
    liftIO $ V.update vty pic
    scrUpdate .= False

makeLayout :: Int -> FlowWidget VtyScreen
makeLayout sidebarW = vert wTop wBottom
  where
    wTop :: FlowWidget VtyScreen
    wTop    = horiz wMain' (fixw sidebarW (horiz vertSep wSidebar))
    wMain' :: FlowWidget VtyScreen
    wMain'  = vert wMain'' wPrompt
    wMain'' :: FlowWidget VtyScreen
    wMain'' = vert wMain wRecentLines
    wBottom :: FixedHeightWidget VtyScreen
    wBottom = vert horizSep wStatus

{-
makeLayout :: Int -> FlowWidget (MB u VtyScreen)
makeLayout sidebarW = vert wTop wBottom
  where
    wTop = horiz (vert (vert (hoistWidgetS wMain) (hoistWidgetS wRecentLines)) (hoistWidgetS wPrompt))
                 (fixw sidebarW (horiz vertSep (hoistWidgetS wSidebar)))
    wBottom = vert horizSep (hoistWidgetS wStatus)
-}

-----------------------------------------------------------------------------

updateStatus :: String -> VtyScreen ()
updateStatus x = do
    scrStatus .= x
    needUpdate

updateSidebar :: Layout -> VtyScreen ()
updateSidebar w = do
    scrSidebar .= w
    needUpdate

-----------------------------------------------------------------------------

runEditor :: String -> (String -> MB u VtyScreen ()) -> MB u VtyScreen ()
runEditor content handler = do
    chan <- lift $ use scrEventChan
    lift $ liftIO $ writeFile "/tmp/mbtmp" content
    lift $ runInteractive (shell "ed /tmp/mbtmp") $ unsafeCallback $ \_ -> do
        newcontent <- lift $ liftIO $ readFile "/tmp/mbtmp"
        handler newcontent

runCmd :: String -> MB u VtyScreen () -> MB u VtyScreen ()
runCmd cmd cb = do
    lift $ runInteractive (shell cmd) $ unsafeCallback $ \_ -> cb
