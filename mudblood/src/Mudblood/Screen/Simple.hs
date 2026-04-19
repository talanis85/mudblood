{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Mudblood.Screen.Simple
    ( SimpleScreen
    , run
    ) where

import Mudblood.Class
import Mudblood.Core
import Mudblood.Text
import Mudblood.Telnet
import Mudblood.Trigger
import Mudblood.Error
import Mudblood.Monad
import Mudblood.Component
import Mudblood.Encoding
import Mudblood.Keys
import Mudblood.Screen

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

import Control.Concurrent
import Control.Concurrent.STM

import Control.UnsafeCallback

import Data.Menu
import Data.Time.Clock.POSIX

import qualified Codec.Binary.UTF8.String as UTF8

import System.IO
import Text.Printf
import Data.Word

-----------------------------------------------------------------------------

newtype SimpleScreen a = SimpleScreen (ExceptT StackTrace (StateT ScreenState IO) a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState ScreenState, MonadError StackTrace)

execSimpleScreen :: SimpleScreen a -> ScreenState -> IO ()
execSimpleScreen (SimpleScreen s) state = void $ runStateT (void $ runExceptT s) state

-----------------------------------------------------------------------------

type SMBR e u = MBR () e u SimpleScreen

data Event = SReceiveEvent [Word8]
           | SSendEvent String
           | STelnetEvent TelnetNeg
           | SCloseEvent
           | SFifoEvent String
           | STimeEvent Int

data Mode = NormalMode
          | PromptMode String (UnsafeCallback String)

data ScreenState = ScreenState
    { _scrEventChan      :: TChan Event
    , _scrSocket         :: Maybe TelnetSocket
    , _scrQuit           :: Bool
    , _scrPrompt         :: [Word8]
    , _scrAttr           :: Attr
    , _scrMode           :: Mode
    , _scrTime           :: Ticks
    }

makeLenses ''ScreenState

-----------------------------------------------------------------------------

instance Screen SimpleScreen where
  outputS o    = case o of
                     OutputLine x  -> outputLine x
                     OutputError x -> outputMessage "ERROR" $ show x
                     OutputInfo x  -> return ()
                     OutputLog x   -> outputMessage "LOG" x
  sendS s      = sendToCurrentSocket s
  setPromptS p = return ()
  connectS h p = connectScreen h p
  timeS        = use scrTime
  setStatusS s = return ()
  {-
  prompt p f  = lift $ do
                       outputMessage "PROMPT" p
                       assign scrMode $ PromptMode p $ unsafeCallback f
  bind k a    = return ()
  menu m      = return ()
  -}

-----------------------------------------------------------------------------

initSimpleScreen :: IO ScreenState
initSimpleScreen = do
    chan <- newTChanIO
    t <- getPOSIXTime
    forkIO $ inputLoop chan
    forkIO $ timerLoop chan
    return $ ScreenState
        { _scrPrompt = []
        , _scrAttr = defaultAttr
        , _scrSocket = Nothing
        , _scrEventChan = chan
        , _scrQuit = False
        , _scrMode = NormalMode
        , _scrTime = floor $ toRational t
        }

-----------------------------------------------------------------------------

run :: (MBEvent e) => MBComponent SimpleScreen e () u -> IO ()
run component = do
    st <- initSimpleScreen
    execSimpleScreen (runWithComponent component () runner) st

runner :: (MBEvent e) => SMBR e u ()
runner = do
    ev <- lift $ do
        chan <- use scrEventChan
        liftIO $ atomically $ readTChan chan

    catchError (handleEvent ev) (lift . outputMessage "ERROR" . show)

    wantsToQuit <- lift $ use scrQuit
    when (not wantsToQuit) runner
  where
    handleEvent ev = do
        case ev of
            SReceiveEvent chars -> do
                oldPrompt <- lift $ use scrPrompt
                oldAttr   <- lift $ use scrAttr
                let (ls, newPrompt, newAttr) = decode oldPrompt chars oldAttr
                mapM_ (triggerWithDefault defaultHandler . mkEv . LineEvent) ls
                lift $ do
                    scrPrompt .= newPrompt
                    scrAttr   .= newAttr
                    outputPrompt newPrompt
            SSendEvent str -> do
                curMode <- lift $ use scrMode
                case curMode of
                    NormalMode -> do
                        case str of
                            ('/':rest) -> do
                                let cmd = takeWhile (/= ' ') rest
                                    arg = case dropWhile (/= ' ') rest of
                                            []     -> Nothing
                                            (x:xs) -> Just xs
                                handleCmd cmd arg
                            _ -> do
                                lift $ scrPrompt .= []
                                triggerWithDefault defaultHandler $ mkEv $ SendEvent str
                    {-
                    PromptMode _ f -> do
                        lift $ scrMode .= NormalMode
                        mbrR' defaultHandler $ runUnsafeCallback f str
                    -}
            SCloseEvent -> do
                lift $ outputMessage "NETWORK" "Connection closed - shutting down."
                lift $ scrQuit .= True
            STelnetEvent neg -> do
                triggerWithDefault defaultHandler $ mkEv $ TelnetEvent neg
            STimeEvent t -> do
                lift $ scrTime .= t
                triggerWithDefault defaultHandler $ mkEv $ TimeEvent t
            _ -> do
                return ()

-----------------------------------------------------------------------------

handleCmd :: (MBEvent e) => String -> Maybe String -> SMBR e u ()
handleCmd cmd arg = do
    bindings <- getBindings
    case walkMenu (map KAscii cmd) bindings of
        Nothing -> return () -- binding does not exist
        Just m  -> case matchMenu m of
            Right _ -> return () -- is submenu
            Left a  -> case matchAction a of
                Left action -> mbx' defaultHandler action
                Right actionFun -> case arg of
                    Nothing -> return () -- missing arg
                    Just arg -> mbx' defaultHandler $ actionFun arg

-----------------------------------------------------------------------------

connectScreen :: String -> String -> SimpleScreen ()
connectScreen host port = do
    oldSocket <- use scrSocket
    chan      <- use scrEventChan
    case oldSocket of
        Nothing         -> return ()
        Just oldSocket  -> liftIO $ telnetClose oldSocket
    newSocket <- liftIO . telnetConnect host port $ telnetRecvHandler $ telnetProc chan
    case newSocket of
        Right newSocket -> do
            scrSocket .= Just newSocket
            outputMessage "NETWORK" ("Connected to " ++ host ++ ":" ++ port)
        Left err -> do
            outputMessage "ERROR" err
  where
    telnetProc chan ev = case ev of
        TelnetRawEvent s        -> liftIO $ telnetReceiveProc chan $ SReceiveEvent s
        TelnetNegEvent n        -> liftIO $ telnetReceiveProc chan $ STelnetEvent n
        TelnetCloseEvent reason -> liftIO $ telnetReceiveProc chan $ SCloseEvent

    telnetReceiveProc chan ev = atomically $ writeTChan chan ev

outputPrompt :: [Word8] -> SimpleScreen ()
outputPrompt p = liftIO $ do
    putStr "\r                                                                                   \r"
    putStr $ escapeAll p
    hFlush stdout

outputMessage :: String -> String -> SimpleScreen ()
outputMessage t msg = do
    p <- use scrPrompt
    liftIO $ do
        putStr "\r                                                                                   \r"
        putStrLn $ printf "[%-8s] %s" t msg
        putStr $ "[PROMPT  ] " ++ escapeAll p
        hFlush stdout

outputLine :: AttrString -> SimpleScreen ()
outputLine l = do
    p <- use scrPrompt
    liftIO $ do
        putStr "\r                                                                                   \r"
        putStrLn $ fromAS l
        putStr $ escapeAll p
        hFlush stdout

sendToCurrentSocket :: (Sendable a) => a -> SimpleScreen ()
sendToCurrentSocket dat = do
    socket <- use scrSocket
    case socket of
        Just socket -> liftIO $ telnetSend socket $ Communication dat
        Nothing     -> return ()

timerLoop :: TChan Event -> IO ()
timerLoop chan = forever $ do
    t <- getPOSIXTime
    atomically $ writeTChan chan $ STimeEvent $ floor $ toRational t
    threadDelay 1000000

inputLoop :: TChan Event -> IO ()
inputLoop chan = forever $ do
    l <- getLine
    atomically $ writeTChan chan $ SSendEvent l
