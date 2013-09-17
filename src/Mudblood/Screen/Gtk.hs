{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, BangPatterns #-}

module Mudblood.Screen.Gtk
    ( execScreen
    , Screen (Screen)
    , screen
    , mb
    , bind
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

data ScreenState = ScreenState {
    scrPrompt :: String,
    scrMarkedPrompt :: String,

    scrNormalBuffer :: String,
    scrCommandBuffer :: String,
    scrNormalKeybuffer :: [Key],

    scrWidgets :: [UIWidget MB],

    scrBindings :: Trie.Trie Key (Screen ()),

    scrMBState :: MBState,
    scrMBConfig :: MBConfig,

    scrSocket :: Maybe TelnetSocket,

    scrTime :: Int,

    --scrMode :: Mode,
    
    scrQuit :: Bool
    }

mkScreenState conf initMBState = ScreenState {
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
    scrQuit = False,
    scrTime = 0,
    scrWidgets = []
}

data ScreenControls = ScreenControls
    { ctlMainView       :: G.TextView
    , ctlMainScroll     :: G.ScrolledWindow
    , ctlMainBuffer     :: G.TextBuffer
    , ctlMainInput      :: G.Entry
    , ctlStatusKeyseq   :: G.Label
    , ctlStatusUser     :: G.Label
    , ctlStatusSystem   :: G.Label
    , ctlWidgetArea     :: G.DrawingArea
    , ctlMapArea        :: G.DrawingArea
    , ctlSidebar        :: G.VBox
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
    interpMB (Free (MBFGetTime g)) = gets scrTime >>= interpMB . g

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
        Left err -> mb $ mbError err
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
                               updatePrompt
        CloseEvent          -> mb $ echo "Connection closed"
        TelnetEvent neg     -> do
                               handleTelneg neg
                               mb $ logger LogDebug $ show neg
    updateWidgets

-- | Send a byte array to the socket (if existent)
sendSocket :: Communication -> Screen ()
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

    mapM_ appendChunk (groupAttrString $ untab 8 astr)

    endIter <- liftIO $ G.textBufferGetEndIter mainbuf
    liftIO $ G.textBufferInsert mainbuf endIter $ "\n"
    endIter <- liftIO $ G.textBufferGetEndIter mainbuf
    liftIO $ G.textBufferMoveMarkByName mainbuf "prompt" endIter

    scrollToEnd

  where
    appendChunk :: (String, Attr) -> Screen ()
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

scrollToEnd :: Screen ()
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

updatePrompt :: Screen ()
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

execUIAction :: UIAction MB -> Screen ()
execUIAction action = case action of
    UIBind keystring action -> bind keystring (mb $ action)
    UIStatus str -> askControls >>= (\l -> liftIO $ G.labelSetText l str) . ctlStatusUser
    UIShowSidebar state -> askControls >>= liftIO . (if state then G.widgetShow else G.widgetHide) . ctlSidebar
    UIUpdateMap map -> do
        ctls <- askControls
        liftIO $ G.labelSetText (ctlStatusSystem ctls) $
            printf "Room: %d" (mapCurrentId map)
    UIUpdateWidgets w -> modify $ \s -> s { scrWidgets = w }
    UISetBgColor val -> do
        ctls <- askControls
        case parseColour val of
            Nothing -> return ()
            Just c -> liftIO $ G.widgetModifyBase (ctlMainView ctls) G.StateNormal (colourToGdk c)
    UISetColor c val -> do
        ctls <- askControls
        tagTable <- liftIO $ G.textBufferGetTagTable (ctlMainBuffer ctls)
        case c of
            DefaultColor -> case parseColour val of
                Nothing -> return ()
                Just c' -> liftIO $ G.widgetModifyText (ctlMainView ctls) G.StateNormal (colourToGdk c')
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
    widgetArea      <- GB.builderGetObject builder G.castToDrawingArea  "widgetArea"
    mapArea         <- GB.builderGetObject builder G.castToDrawingArea  "mapArea"
    sidebar         <- GB.builderGetObject builder G.castToVBox         "sidebar"

    let controls = ScreenControls
            { ctlMainView       = mainView
            , ctlMainScroll     = mainScroll
            , ctlMainBuffer     = mainBuffer
            , ctlMainInput      = mainInput
            , ctlStatusKeyseq   = statusKeyseq
            , ctlStatusUser     = statusUser
            , ctlStatusSystem   = statusSystem
            , ctlWidgetArea     = widgetArea
            , ctlMapArea        = mapArea
            , ctlSidebar        = sidebar
            }

    monoFont <- P.fontDescriptionFromString "monospace"

    G.widgetModifyFont mainView $ Just monoFont
    --G.widgetModifyText mainView G.StateNormal $ P.Color 60000 60000 60000
    --G.widgetModifyBase mainView G.StateNormal $ P.Color 0 0 0

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

    widgetArea `on` G.exposeEvent $ do
        drawwin <- G.eventWindow
        liftIO $ runScreen controls stref $ drawWidgets drawwin
        return False

    mapArea `on` G.exposeEvent $ do
        drawwin <- G.eventWindow
        liftIO $ runScreen controls stref $ drawMap drawwin
        return False

    -- Quit when the window is closed
    window `on` G.deleteEvent $ liftIO G.mainQuit >> return False

    G.widgetShowAll window

    return controls

runTimer :: ScreenControls -> IORef ScreenState -> IO ()
runTimer ctrls stref = forever $ do
    t <- getPOSIXTime
    G.postGUIAsync $ runScreen ctrls stref $ updateTimer (floor $ toRational t)
    threadDelay 1000000

updateTimer :: Int -> Screen ()
updateTimer n = do
    modify $ \s -> s { scrTime = n }
    mb $ processTime n
    updateWidgets

-- | Run the screen
execScreen :: MBConfig -> MBState -> Screen () -> IO ()
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
                                        cmd@('(':_) -> mb $ command cmd
                                        _ -> do
                                             mb $ echoA $ (toAttrString $ (scrMarkedPrompt st) ++ (scrPrompt st))
                                                          `mappend`
                                                          (setFg Yellow (toAttrString text))
                                             mb $ processSend text
                                     liftIO $ G.set (ctlMainInput ctrls) [ G.entryText := "" ]
                                     updateWidgets
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

updateWidgets :: Screen ()
updateWidgets = do
    ctls <- askControls
    liftIO $ G.widgetQueueDraw (ctlWidgetArea ctls)

drawWidgets :: G.DrawWindow -> Screen ()
drawWidgets win = do
    widgets <- gets scrWidgets
    ctls <- askControls
    style <- liftIO $ G.rcGetStyle (ctlWidgetArea ctls)

    actions <- forM widgets $ \w -> do
        case w of
            UIWidgetText poll -> mb poll >>= return . (renderTextWidget style)
            UIWidgetTable poll -> mb poll >>= return . (renderTableWidget style)

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

drawMap :: G.DrawWindow -> Screen ()
drawMap win = return ()
