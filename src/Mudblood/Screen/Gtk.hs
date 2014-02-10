{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, BangPatterns #-}

module Mudblood.Screen.Gtk
    ( execScreen
    , Screen (..), ScreenState (..)
    , screen
    , mb
    , bind
    , menu
    , prompt
    , setStatus
    , askControls
    , ScreenControls (..)
    ) where

import Data.Char
import Data.Word
import Data.Monoid
import Data.Dynamic
import Data.IORef
import Data.List
import qualified Data.Map as M
import qualified Data.Trie as Trie

import Text.Printf (printf)

import Control.Concurrent
import Data.Time.Clock.POSIX

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Free (Free (Free, Pure))

import System.IO
import qualified Graphics.UI.Gtk as G
import qualified Graphics.UI.Gtk.Builder as GB
import Graphics.UI.Gtk (AttrOp (..), on, after)
import qualified Graphics.Rendering.Pango as P
import qualified Graphics.Rendering.Cairo as C

import qualified Codec.Binary.UTF8.String as UTF8

import Mudblood.Core
import Mudblood.Telnet
import Mudblood.Keys
import Mudblood.Text
import Mudblood.Colour
import Mudblood.UI
import Mudblood.Mapper

import Paths_mudblood

-- | Socket Events
data SocketEvent = DataEvent String
                 | TelnetEvent TelnetNeg
                 | CloseEvent

------------------------------------------------------------------------------

data Mode u = NormalMode
            | PromptMode (String -> Screen u ())

data ScreenState u = ScreenState {
    scrPrompt :: String,
    scrMarkedPrompt :: String,

    scrNormalKeybuffer :: [Key],

    scrBuffers :: M.Map String G.TextBuffer,
    scrStatus :: MB u String,

    scrBindings :: Trie.Trie Key (Last (Screen u ())),
    scrMenu :: Maybe (String, KeyMenu Key (Screen u ())),

    scrMBState :: MBState u,
    scrMBConfig :: MBConfig u,

    scrSocket :: Maybe TelnetSocket,

    scrTime :: Int,
    scrMode :: Mode u,

    scrQuit :: Bool,

    scrWidgetCache :: [UIWidget],

    scrUpdateHandler :: Screen u ()
    }

mkScreenState conf initMBState = ScreenState {
    scrPrompt = "",
    scrMarkedPrompt = "",
    scrNormalKeybuffer = [],
    scrBindings = Trie.empty,
    scrMenu = Nothing,
    scrMBConfig = conf,
    scrMBState = initMBState,
    scrSocket = Nothing,
    scrQuit = False,
    scrTime = 0,
    scrBuffers = M.empty,
    scrStatus = return "",
    scrMode = NormalMode,
    scrWidgetCache = [],
    scrUpdateHandler = return ()
}

data ScreenControls = ScreenControls
    { ctlBuilder        :: GB.Builder
    , ctlMainView       :: G.TextView
    , ctlMainScroll     :: G.ScrolledWindow
    , ctlMainBuffer     :: G.TextBuffer
    , ctlMainInput      :: G.Entry
    , ctlStatusKeyseq   :: G.Label
    , ctlStatusUser     :: G.Label
    , ctlStatusSystem   :: G.Label
    , ctlPromptLabel    :: G.Label
    , ctlListMenu       :: G.ListStore String
    , ctlTreeMenu       :: G.TreeView
    }

newtype Screen u a = Screen (ReaderT (IORef (ScreenState u), ScreenControls) IO a)
    deriving (Monad, MonadIO, MonadReader (IORef (ScreenState u), ScreenControls))

instance MonadState (ScreenState u) (Screen u) where
    get   = Screen (ask >>= liftIO . readIORef . fst)
    put s = Screen (ask >>= liftIO . flip writeIORef s . fst)

askControls :: Screen u ScreenControls
askControls = ask >>= return . snd

runScreen ctrls stref (Screen s) = runReaderT s (stref, ctrls)

------------------------------------------------------------------------------

mb :: MB u a -> Screen u a
mb mb = do
    st <- get
    (a, s) <- interpMB $ runMB (scrMBConfig st) (scrMBState st) mb
    modify $ \st -> st { scrMBState = s }
    return a
  where
    interpMB (Pure r) = return r
    interpMB (Free (MBFIO action g)) = liftIO action >>= interpMB . g
    interpMB (Free (MBFConnect h p x)) = connectScreen h p >> interpMB x
    interpMB (Free (MBFLine Nothing l x)) = appendToMainBuffer l >> interpMB x
    interpMB (Free (MBFLine (Just name) s x)) = appendToBuffer name s >> interpMB x
    interpMB (Free (MBFSend d x)) = sendSocket d >> interpMB x
    interpMB (Free (MBFQuit x)) = liftIO (G.mainQuit) >> interpMB x
    interpMB (Free (MBFUI a x)) = execUIAction a >> interpMB x
    --interpMB (Free (MBFDialog desc handler x)) = createDialogWindow desc handler >> interpMB x
    interpMB (Free (MBFDialog desc handler x)) = interpMB x
    interpMB (Free (MBFGetTime g)) = gets scrTime >>= interpMB . g

------------------------------------------------------------------------------

-- | Open a connection
connectScreen :: String -> String -> Screen u ()
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
        Left err -> mb $ echoE err
  where
    telnetProc state ev = case ev of
        TelnetRawEvent s -> liftIO $ telnetReceiveProc state $ DataEvent $ UTF8.decode s
        TelnetNegEvent n -> liftIO $ telnetReceiveProc state $ TelnetEvent n
        TelnetCloseEvent reason -> liftIO $ telnetReceiveProc state $ CloseEvent

    telnetReceiveProc st ev = let (Screen s) = telnetReceive ev
                              in G.postGUIAsync $ runReaderT s st

    telnetReceive ev = do
        st <- get
        case ev of
            DataEvent chars  -> do (p, _) <- mb $ process (scrPrompt st) chars defaultAttr -- TODO: where to save current attr?
                                   modify $ \st -> st { scrPrompt = p }
                                   updatePrompt
            CloseEvent          -> mb $ echo "Connection closed"
            TelnetEvent neg     -> do
                                   handleTelneg neg
        updateWidgets

-- | Send a byte array to the socket (if existent)
sendSocket :: Communication -> Screen u ()
sendSocket dat =
    do
    state <- get
    case (scrSocket state) of
         Just sock -> liftIO $ telnetSend sock dat
         Nothing -> appendToMainBuffer (toAS "No socket")

-- | The screen main loop
screen :: Screen u () -> Screen u ()
screen updateHandler = do
    modify $ \s -> s { scrUpdateHandler = updateHandler }
    liftIO G.mainGUI

setStatus :: MB u String -> Screen u ()
setStatus action = modify $ \s -> s { scrStatus = action }

-- | Append a line to the main line buffer
appendToMainBuffer :: AttrString -> Screen u ()
appendToMainBuffer astr = do
    ctrls <- askControls
    let mainbuf = ctlMainBuffer ctrls

    -- Remove old prompt
    endIter <- liftIO $ G.textBufferGetEndIter mainbuf
    promptMark <- liftIO $ G.textBufferGetMark mainbuf "prompt"
    promptIter <- case promptMark of
        Nothing -> do
                   liftIO $ G.textBufferCreateMark mainbuf (Just "prompt") endIter True
                   return endIter
        Just promptMark' -> liftIO $ G.textBufferGetIterAtMark mainbuf promptMark'
    liftIO $ G.textBufferDelete mainbuf promptIter endIter

    mapM_ (appendChunk mainbuf) (groupAS $ untabAS 8 astr)

    endIter <- liftIO $ G.textBufferGetEndIter mainbuf
    liftIO $ G.textBufferInsert mainbuf endIter $ "\n"
    endIter <- liftIO $ G.textBufferGetEndIter mainbuf
    liftIO $ G.textBufferMoveMarkByName mainbuf "prompt" endIter

    scrollToEnd

appendChunk :: G.TextBuffer -> (String, Attr) -> Screen u ()
appendChunk mainbuf (s, a) = do
  liftIO $ do startIter <- G.textBufferGetEndIter mainbuf
              startOffset <- G.textIterGetOffset startIter
              G.textBufferInsert mainbuf startIter s
              endOffset <- G.textBufferGetEndIter mainbuf >>= G.textIterGetOffset
              when (tagNameForAttr a /= "") $ do
                  startIter' <- G.textBufferGetIterAtOffset mainbuf startOffset
                  endIter' <- G.textBufferGetIterAtOffset mainbuf endOffset
                  G.textBufferApplyTagByName mainbuf (tagNameForAttr a) startIter' endIter'
                  return ()

getNamedBuffer :: String -> Screen u G.TextBuffer
getNamedBuffer name = do
    buffers <- gets scrBuffers
    case M.lookup name buffers of
        Nothing -> do
            ctrls <- askControls
            tagTable <- liftIO $ G.textBufferGetTagTable (ctlMainBuffer ctrls)
            newbuf <- liftIO $ G.textBufferNew (Just tagTable)
            modify $ \s -> s { scrBuffers = M.insert name newbuf (scrBuffers s) }
            return newbuf
        Just b -> return b

appendToBuffer :: String -> AttrString -> Screen u ()
appendToBuffer name str = do
    buf <- getNamedBuffer name
    endIter <- liftIO $ G.textBufferGetEndIter buf
    mapM_ (appendChunk buf) (groupAS $ untabAS 8 str)
    endIter <- liftIO $ G.textBufferGetEndIter buf
    liftIO $ G.textBufferInsert buf endIter $ "\n"

scrollToEnd :: Screen u ()
scrollToEnd = do
    ctrls <- askControls
    let mainbuf = ctlMainBuffer ctrls

    endIter <- liftIO $ G.textBufferGetEndIter mainbuf
    endMark <- do
        m <- liftIO $ G.textBufferGetMark mainbuf "end"
        case m of
            Nothing -> liftIO $ G.textBufferCreateMark mainbuf (Just "end") endIter True
            Just mark -> do
                liftIO $ G.textBufferMoveMark mainbuf mark endIter
                return mark
    liftIO $ G.textViewScrollMarkOnscreen (ctlMainView ctrls) endMark

updatePrompt :: Screen u ()
updatePrompt = do
    ctrls <- askControls
    let mainbuf = ctlMainBuffer ctrls

    -- Remove old prompt
    endIter <- liftIO $ G.textBufferGetEndIter mainbuf
    promptMark <- liftIO $ G.textBufferGetMark mainbuf "prompt"
    promptIter <- case promptMark of
        Nothing -> do
                   liftIO $ G.textBufferCreateMark mainbuf (Just "prompt") endIter True
                   return endIter
        Just promptMark' -> liftIO $ G.textBufferGetIterAtMark mainbuf promptMark'
    liftIO $ G.textBufferDelete mainbuf promptIter endIter

    prompt <- gets scrPrompt
    markedPrompt <- gets scrMarkedPrompt

    endIter <- liftIO $ G.textBufferGetEndIter mainbuf
    liftIO $ G.textBufferInsert mainbuf endIter $ markedPrompt ++ prompt

    scrollToEnd

execUIAction :: UIAction (MB u) -> Screen u ()
execUIAction action = case action of
    UIPrompt title handler -> prompt title (mb . handler)
    UISetCompletion comp -> return () -- TODO
    UIBind keystring action -> bind keystring (mb $ action)
    UIUpdateWindow name -> return () --updateWindow name
    UIUpdateMap map -> do
        ctls <- askControls
        liftIO $ G.labelSetText (ctlStatusSystem ctls) $
            printf "Room: %d" (mapCurrentId map)
    UISetBgColor val -> do
        ctls <- askControls
        case parseColour val of
            Nothing -> return ()
            Just c -> do
                liftIO $ G.widgetModifyBase (ctlMainView ctls) G.StateNormal (colourToGdk c)
    UISetColor c val -> do
        ctls <- askControls
        tagTable <- liftIO $ G.textBufferGetTagTable (ctlMainBuffer ctls)
        case c of
            DefaultColor -> case parseColour val of
                Nothing -> return ()
                Just c' -> do
                    liftIO $ G.widgetModifyText (ctlMainView ctls) G.StateNormal (colourToGdk c')
            c' -> case colorToName c of
                    "" -> return ()
                    colname -> case parseColour val of
                        Nothing -> return ()
                        Just val' -> do
                               tag <- liftIO $ G.textTagTableLookup tagTable colname
                               case tag of
                                    Nothing -> return ()
                                    Just tag' -> liftIO $ G.set tag' [ G.textTagForegroundGdk := colourToGdk val' ]
    _ -> return ()

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

pangoColorForString :: String -> Maybe P.Color
pangoColorForString x = case x of
    "black" -> Just $ P.Color 0 0 0
    "white" -> Just $ P.Color 65000 65000 65000
    _ -> Nothing

mapKey :: Maybe Char -> String -> Maybe Key
mapKey chr name = case name of
    "Return"    -> Just KEnter
    "Escape"    -> Just KEsc
    "BackSpace" -> Just KBS
    "Tab"       -> Just KTab

    "F1"        -> Just KF1
    "F2"        -> Just KF2
    "F3"        -> Just KF3
    "F4"        -> Just KF4
    "F5"        -> Just KF5
    "F6"        -> Just KF6
    "F7"        -> Just KF7
    "F8"        -> Just KF8
    "F9"        -> Just KF9
    "F10"       -> Just KF10
    "F11"       -> Just KF11
    "F12"       -> Just KF12

    _           -> fmap KAscii chr

initUI :: String -> IORef (ScreenState u) -> IO ScreenControls
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
    promptLabel     <- GB.builderGetObject builder G.castToLabel        "promptLabel"
    treeMenu        <- GB.builderGetObject builder G.castToTreeView     "treeMenu"

    listMenu <- G.listStoreNew []
    G.treeViewSetModel treeMenu listMenu
    colText <- G.treeViewColumnNew
    cellText <- G.cellRendererTextNew
    G.treeViewColumnPackStart colText cellText True
    G.cellLayoutSetAttributes colText cellText listMenu (\row -> [ G.cellText := row ])
    G.treeViewAppendColumn treeMenu colText

    let controls = ScreenControls
            { ctlBuilder        = builder
            , ctlMainView       = mainView
            , ctlMainScroll     = mainScroll
            , ctlMainBuffer     = mainBuffer
            , ctlMainInput      = mainInput
            , ctlStatusKeyseq   = statusKeyseq
            , ctlStatusUser     = statusUser
            , ctlStatusSystem   = statusSystem
            , ctlPromptLabel    = promptLabel
            , ctlListMenu       = listMenu
            , ctlTreeMenu       = treeMenu
            }

    monoFont <- P.fontDescriptionFromString "monospace"

    G.widgetModifyFont mainView $ Just monoFont
    --G.widgetModifyText mainView G.StateNormal $ P.Color 60000 60000 60000
    --G.widgetModifyBase mainView G.StateNormal $ P.Color 0 0 0

    G.widgetModifyFont mainInput $ Just monoFont

    G.widgetModifyFont statusKeyseq $ Just monoFont
    G.widgetModifyFont promptLabel $ Just monoFont
    G.widgetModifyFont statusSystem $ Just monoFont

    tagTable <- G.textBufferGetTagTable mainBuffer

    forM_ ["black", "white", "cyan", "magenta", "blue", "yellow", "green", "red"] $ \x -> do
        tag <- G.textTagNew $ Just x
        G.set tag [ G.textTagForeground := x ]
        G.textTagTableAdd tagTable tag

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

    -- Quit when the window is closed
    window `on` G.deleteEvent $ liftIO G.mainQuit >> return False

    G.set window [ G.windowRole := "mudblood_main" ]

    G.widgetShowAll window

    return controls

runTimer :: ScreenControls -> IORef (ScreenState u) -> IO ()
runTimer ctrls stref = forever $ do
    t <- getPOSIXTime
    G.postGUIAsync $ runScreen ctrls stref $ updateTimer (floor $ toRational t)
    threadDelay 1000000

updateTimer :: Int -> Screen u ()
updateTimer n = do
    modify $ \s -> s { scrTime = n }
    mb $ processTime n
    updateWidgets

-- | Run the screen
execScreen :: String -> MBConfig u -> MBState u -> Screen u () -> IO ()
execScreen gladepath conf initMBState action =
    do
    G.initGUI
    stref <- newIORef $ mkScreenState conf initMBState
    ctrls <- initUI gladepath stref
    forkIO $ runTimer ctrls stref
    runScreen ctrls stref action
    st <- readIORef stref
    case scrSocket st of
        Just sock -> telnetClose sock
        Nothing -> return ()

------------------------------------------------------------------------------

handleKey :: Key -> Screen u Bool
handleKey key = do
    ctrls <- askControls
    mode <- gets scrMode
    case mode of
        PromptMode tgt -> do
            case key of
                KEnter -> do
                    text <- liftIO $ G.get (ctlMainInput ctrls) G.entryText
                    liftIO $ G.set (ctlMainInput ctrls) [ G.entryText := "" ]
                    modify $ \s -> s { scrMode = NormalMode }
                    liftIO $ G.set (ctlPromptLabel ctrls) [ G.widgetVisible := False, G.labelText := "" ]
                    tgt text
                    return True
                _ -> return False
        NormalMode -> do
            wasBinding <- handleKeybinding key
            st <- get
            liftIO $ G.labelSetText (ctlStatusKeyseq ctrls) $ concat [show x | x <- scrNormalKeybuffer st]
            if wasBinding then return True
                          else case key of
                                   KEnter -> do
                                             text <- liftIO $ G.get (ctlMainInput ctrls) G.entryText
                                             case text of
                                                cmd@('(':_) -> mb $ command cmd
                                                _ -> do
                                                     mb $ echoA $ (toAS $ (scrMarkedPrompt st) ++ (scrPrompt st))
                                                                  `mappend`
                                                                  (setFg Yellow (toAS text))
                                                     mb $ processSend text
                                             --liftIO $ G.set (ctlMainInput ctrls) [ G.entryText := "" ]
                                             liftIO $ G.editableSelectRegion (ctlMainInput ctrls) 0 100
                                             updateWidgets
                                             return True
                                   _ -> return False

handleMenu :: Key -> Screen u Bool
handleMenu k = do
    st <- get
    ctls <- askControls
    case (scrMenu st) of
        Nothing -> return False
        Just (_, menu) ->
            case stepMenu k menu of
                Just (desc, KeyAction action) -> do
                    liftIO $ G.widgetHide (ctlTreeMenu ctls)
                    action
                    modify (\s -> s { scrMenu = Nothing })
                    return True
                Just (desc, KeyMenu m) -> do
                    liftIO $ G.listStoreClear (ctlListMenu ctls)
                    liftIO $ forM_ (showMenu (KeyMenu m)) $ \(k,d) -> G.listStoreAppend (ctlListMenu ctls) $ show k ++ ": " ++ d
                    modify (\s -> s { scrMenu = Just (desc, KeyMenu m) })
                    return True
                Nothing -> do
                    liftIO $ G.widgetHide (ctlTreeMenu ctls)
                    modify (\s -> s { scrMenu = Nothing })
                    return False

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
menu desc m = do
    ctls <- askControls
    liftIO $ G.listStoreClear (ctlListMenu ctls)
    liftIO $ forM_ (showMenu m) $ \(k,d) -> G.listStoreAppend (ctlListMenu ctls) $ show k ++ ": " ++ d
    liftIO $ G.widgetShow (ctlTreeMenu ctls)
    modify $ \s -> s { scrMenu = Just (desc, m) }

------------------------------------------------------------------------------

prompt :: String -> (String -> Screen u ()) -> Screen u ()
prompt title f = do
    ctls <- askControls
    modify $ \s -> s { scrMode = PromptMode f }
    liftIO $ G.set (ctlPromptLabel ctls) [ G.widgetVisible := True, G.labelText := title ]
    liftIO $ liftIO $ G.widgetGrabFocus (ctlMainInput ctls)

------------------------------------------------------------------------------

-- | Handle telnet negotiations
handleTelneg :: TelnetNeg -> Screen u ()
handleTelneg neg = case neg of
    -- WILL EOR
    TelnetNeg (Just CMD_WILL) (Just OPT_EOR) [] ->
        mb $ send $ TelnetNeg (Just CMD_DO) (Just OPT_EOR) []        
    -- EOR
    TelnetNeg (Just CMD_EOR) Nothing [] -> do
        modify $ \st -> st { scrPrompt = "", scrMarkedPrompt = scrPrompt st }
        updatePrompt
    -- All other Telnegs are handled by the Core module
    _ ->
        mb $ processTelnet neg

------------------------------------------------------------------------------

updateWidgets :: Screen u ()
updateWidgets = do
    updateHandler <- gets scrUpdateHandler
    updateHandler
