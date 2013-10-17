{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, BangPatterns #-}

module Mudblood.Screen.Gtk
    ( execScreen
    , Screen (Screen)
    , screen
    , mb
    , bind
    , prompt
    , createWidgetWindow
    , createTextWindow
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

-- data Mode = NormalMode | CommandMode

-- | Socket Events
data SocketEvent = DataEvent String
                 | TelnetEvent TelnetNeg
                 | CloseEvent

------------------------------------------------------------------------------

data Window u = WidgetWindow G.Window (MB u [UIWidget])
              | MapWindow G.Window
              | TextWindow G.Window G.TextView String

data Mode u = NormalMode
            | PromptMode (String -> Screen u ())

data ScreenState u = ScreenState {
    scrPrompt :: String,
    scrMarkedPrompt :: String,

    scrNormalKeybuffer :: [Key],

    scrWindows :: [Window u],
    scrBuffers :: M.Map String G.TextBuffer,

    scrBindings :: Trie.Trie Key (Last (Screen u ())),

    scrMBState :: MBState u,
    scrMBConfig :: MBConfig,

    scrSocket :: Maybe TelnetSocket,

    scrTime :: Int,
    scrMode :: Mode u,

    scrQuit :: Bool
    }

mkScreenState conf initMBState = ScreenState {
    scrPrompt = "",
    scrMarkedPrompt = "",
    scrNormalKeybuffer = [],
    scrBindings = Trie.empty,
    scrMBConfig = conf,
    scrMBState = initMBState,
    scrSocket = Nothing,
    scrQuit = False,
    scrTime = 0,
    scrWindows = [],
    scrBuffers = M.empty,
    scrMode = NormalMode
}

data ScreenControls = ScreenControls
    { ctlMainView       :: G.TextView
    , ctlMainScroll     :: G.ScrolledWindow
    , ctlMainBuffer     :: G.TextBuffer
    , ctlMainInput      :: G.Entry
    , ctlStatusKeyseq   :: G.Label
    , ctlStatusSystem   :: G.Label
    , ctlHboxPrompt     :: G.HBox
    , ctlPromptLabel    :: G.Label
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
    interpMB (Free (MBFLine l x)) = appendToMainBuffer l >> interpMB x
    interpMB (Free (MBFSend d x)) = sendSocket d >> interpMB x
    interpMB (Free (MBFQuit x)) = liftIO (G.mainQuit) >> interpMB x
    interpMB (Free (MBFUI a x)) = execUIAction a >> interpMB x
    interpMB (Free (MBFGetTime g)) = gets scrTime >>= interpMB . g
    interpMB (Free (MBFToBuffer name s x)) = appendToBuffer name s >> interpMB x

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
        Left err -> mb $ mbError err
  where
    telnetProc state ev = case ev of
        TelnetRawEvent s -> liftIO $ telnetReceiveProc state $ DataEvent $ UTF8.decode s
        TelnetNegEvent n -> liftIO $ telnetReceiveProc state $ TelnetEvent n
        TelnetCloseEvent reason -> liftIO $ telnetReceiveProc state $ CloseEvent

    telnetReceiveProc st ev = let (Screen s) = telnetReceive ev
                              in G.postGUIAsync $ runReaderT s st

-- | Handle a SocketEvent
telnetReceive :: SocketEvent -> Screen u ()
telnetReceive ev = do
    st <- get
    case ev of
        DataEvent chars  -> do (p, _) <- mb $ process (scrPrompt st) chars defaultAttr -- TODO: where to save current attr?
                               modify $ \st -> st { scrPrompt = p }
                               updatePrompt
        CloseEvent          -> mb $ echo "Connection closed"
        TelnetEvent neg     -> do
                               handleTelneg neg
                               mb $ logger LogDebug $ show neg
    updateWidgets

-- | Send a byte array to the socket (if existent)
sendSocket :: Communication -> Screen u ()
sendSocket dat =
    do
    state <- get
    case (scrSocket state) of
         Just sock -> liftIO $ telnetSend sock dat
         Nothing -> appendToMainBuffer (toAttrString "No socket")

-- | The screen main loop
screen :: Screen u ()
screen = liftIO G.mainGUI

createWidgetWindow :: MB u [UIWidget] -> Screen u ()
createWidgetWindow widgets = do
    (stref, controls) <- ask

    newwin <- liftIO $ G.windowNew
    liftIO $ G.set newwin [ G.windowTypeHint := G.WindowTypeHintUtility ]
    liftIO $ G.set newwin [ G.windowRole := "mudblood_widgets" ]

    liftIO $ do
        drawingArea <- G.drawingAreaNew
        G.containerAdd newwin drawingArea
        drawingArea `on` G.exposeEvent $ do
            drawwin <- G.eventWindow
            style <- liftIO $ G.rcGetStyle drawingArea
            liftIO $ runScreen controls stref $ drawWidgets drawwin style widgets
            return False
    return ()

    modify $ \s -> s { scrWindows = (WidgetWindow newwin widgets) : (scrWindows s) }

    liftIO $ G.widgetShowAll newwin

createTextWindow :: String -> Screen u ()
createTextWindow name = do
    (stref, controls) <- ask

    newwin <- liftIO $ G.windowNew
    liftIO $ G.set newwin [ G.windowTypeHint := G.WindowTypeHintUtility ]
    liftIO $ G.set newwin [ G.windowRole := "mudblood_text" ]

    buf <- getNamedBuffer name
    view <- liftIO $ do
        scroller <- G.scrolledWindowNew Nothing Nothing
        view <- G.textViewNewWithBuffer buf
        G.set view [ G.textViewEditable := False ]
        monoFont <- P.fontDescriptionFromString "monospace"
        G.widgetModifyFont view $ Just monoFont
        G.containerAdd scroller view
        G.containerAdd newwin scroller
        return view

    modify $ \s -> s { scrWindows = (TextWindow newwin view name) : (scrWindows s) }

    liftIO $ G.widgetShowAll newwin

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

    mapM_ (appendChunk mainbuf) (groupAttrString $ untab 8 astr)

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
    mapM_ (appendChunk buf) (groupAttrString $ untab 8 str)
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
    UIBind keystring action -> bind keystring (mb $ action)
    UIUpdateMap map -> do
        ctls <- askControls
        liftIO $ G.labelSetText (ctlStatusSystem ctls) $
            printf "Room: %d" (mapCurrentId map)
    UISetBgColor val -> do
        ctls <- askControls
        windows <- gets scrWindows
        case parseColour val of
            Nothing -> return ()
            Just c -> do
                liftIO $ G.widgetModifyBase (ctlMainView ctls) G.StateNormal (colourToGdk c)
                forM_ windows $ \w ->
                    case w of
                        TextWindow win view name ->
                            liftIO $ G.widgetModifyBase view G.StateNormal (colourToGdk c)
                        _ -> return ()
    UISetColor c val -> do
        ctls <- askControls
        tagTable <- liftIO $ G.textBufferGetTagTable (ctlMainBuffer ctls)
        case c of
            DefaultColor -> case parseColour val of
                Nothing -> return ()
                Just c' -> do
                    windows <- gets scrWindows
                    liftIO $ G.widgetModifyText (ctlMainView ctls) G.StateNormal (colourToGdk c')
                    forM_ windows $ \w ->
                        case w of
                            TextWindow win view name ->
                                liftIO $ G.widgetModifyText view G.StateNormal (colourToGdk c')
                            _ -> return ()
            c' -> case colorToName c of
                    "" -> return ()
                    colname -> case parseColour val of
                        Nothing -> return ()
                        Just val' -> do
                               tag <- liftIO $ G.textTagTableLookup tagTable colname
                               case tag of
                                    Nothing -> return ()
                                    Just tag' -> liftIO $ G.set tag' [ G.textTagForegroundGdk := colourToGdk val' ]

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
    statusSystem    <- GB.builderGetObject builder G.castToLabel        "statusSystem"
    hboxPrompt      <- GB.builderGetObject builder G.castToHBox         "hboxPrompt"
    promptLabel     <- GB.builderGetObject builder G.castToLabel        "promptLabel"

{-
    widgetWindow <- G.windowNew
    widgetArea <- G.drawingAreaNew

    G.set widgetWindow [ G.windowTypeHint := G.WindowTypeHintUtility ]
    G.containerAdd widgetWindow widgetArea

    G.widgetShowAll widgetWindow

    widgetArea `on` G.sizeRequest $ return $ G.Requisition 200 200
    -}

    let controls = ScreenControls
            { ctlMainView       = mainView
            , ctlMainScroll     = mainScroll
            , ctlMainBuffer     = mainBuffer
            , ctlMainInput      = mainInput
            , ctlStatusKeyseq   = statusKeyseq
            , ctlStatusSystem   = statusSystem
            , ctlHboxPrompt     = hboxPrompt
            , ctlPromptLabel    = promptLabel
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

    {-
    widgetArea `on` G.exposeEvent $ do
        drawwin <- G.eventWindow
        liftIO $ runScreen controls stref $ drawWidgets drawwin
        return False
        -}

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
execScreen :: MBConfig -> (MBState u) -> Screen u () -> IO ()
execScreen conf initMBState action =
    do
    G.initGUI
    stref <- newIORef $ mkScreenState conf initMBState
    --gladepath <- getDataFileName "gui.glade"
    --ctrls <- initUI gladepath stref
    ctrls <- initUI "gui.glade" stref
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
                                                     mb $ echoA $ (toAttrString $ (scrMarkedPrompt st) ++ (scrPrompt st))
                                                                  `mappend`
                                                                  (setFg Yellow (toAttrString text))
                                                     mb $ processSend text
                                             --liftIO $ G.set (ctlMainInput ctrls) [ G.entryText := "" ]
                                             liftIO $ G.editableSelectRegion (ctlMainInput ctrls) 0 100
                                             updateWidgets
                                             return True
                                   _ -> return False

handleKeybinding :: Key -> Screen u Bool
handleKeybinding k = do
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

------------------------------------------------------------------------------

prompt :: String -> (String -> Screen u ()) -> Screen u ()
prompt title f = do
    ctls <- askControls
    modify $ \s -> s { scrMode = PromptMode f }
    liftIO $ G.set (ctlPromptLabel ctls) [ G.widgetVisible := True, G.labelText := title ]
    liftIO $ liftIO $ G.widgetGrabFocus (ctlMainInput ctls)

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
handleTelneg :: TelnetNeg -> Screen u ()
handleTelneg neg = case neg of
    -- WILL EOR
    TelnetNeg (Just CMD_WILL) (Just OPT_EOR) [] ->
        mb $ send $ TelnetNeg (Just CMD_DO) (Just OPT_EOR) []        
    -- EOR
    TelnetNeg (Just CMD_EOR) Nothing [] -> do
        modify $ \st -> st { scrPrompt = "", scrMarkedPrompt = scrPrompt st }
        updatePrompt
    {-
    -- NAWS (defunct)
    TelnetNeg (Just CMD_DO) (Just OPT_NAWS) [] -> do
        sendSocket $ telnegToBytes $ TelnetNeg (Just CMD_WILL) (Just OPT_NAWS) []        
        (w, h) <- getViewSize
        sendSocket $ telnegToBytes $ telnetNegNaws w h
    -}
    -- All other Telnegs are handled by the Core module
    _ ->
        mb $ processTelnet neg

------------------------------------------------------------------------------

updateWidgets :: Screen u ()
updateWidgets = do
    windows <- gets scrWindows
    forM_ windows $ \w ->
        case w of
            WidgetWindow win action -> liftIO $ G.widgetQueueDraw win
            _ -> return ()

drawWidgets :: G.DrawWindow -> G.Style -> MB u [UIWidget] -> Screen u ()
drawWidgets win style widgets = do
    widgets <- mb widgets

    actions <- forM widgets $ \w -> do
        case w of
            UIWidgetText str -> return $ renderTextWidget style str
            UIWidgetTable tab -> return $ renderTableWidget style tab

    liftIO $ G.renderWithDrawable win $ do
        C.translate 8.0 14.0
        forM_ actions $ \a -> do
            C.save
            height <- a
            C.restore
            C.translate 0 $ height + 10.0

renderTextWidget :: G.Style -> String -> C.Render Double
renderTextWidget style str = do
    fgColor <- liftIO $ G.styleGetForeground style G.StateNormal
    G.setSourceColor fgColor
    C.selectFontFace "monospace" C.FontSlantNormal C.FontWeightNormal
    C.setFontSize 12.0

    C.textPath str
    C.fill

    ext <- C.textExtents str
    return $ C.textExtentsHeight ext

renderTableWidget :: G.Style -> [[String]] -> C.Render Double
renderTableWidget style tab = do
    fgColor <- liftIO $ G.styleGetForeground style G.StateNormal
    G.setSourceColor fgColor
    C.selectFontFace "monospace" C.FontSlantNormal C.FontWeightNormal
    C.setFontSize 12.0

    forM_ (transpose $ rectify " " (maximum $ map length tab) tab) $ \col -> do
        C.save
        widths <- forM col $ \cell -> do
            C.save
            C.textPath cell
            C.fill
            C.restore
            C.translate 0.0 12.0
            ext <- C.textExtents cell
            return $ C.textExtentsWidth ext
        C.restore
        C.translate (maximum widths + 5.0) 0.0

    return $ (fromIntegral $ length tab) * 12.0
  where
    rectify elem len mat = map (fill elem len) mat
    fill elem len l = l ++ (take (max 0 (len - length l)) $ repeat elem)

drawMap :: G.DrawWindow -> Screen u ()
drawMap win = return ()
