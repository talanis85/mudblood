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

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

import Control.Concurrent
import Control.Concurrent.STM

import Control.UnsafeCallback

import Data.Time.Clock.POSIX

import qualified Codec.Binary.UTF8.String as UTF8

import System.IO
import Text.Printf
import Data.Word

-----------------------------------------------------------------------------

newtype SimpleScreen a = SimpleScreen (ExceptT StackTrace (StateT ScreenState IO) a)
    deriving (Functor, Monad, MonadIO, MonadState ScreenState, MonadError StackTrace)

execSimpleScreen :: SimpleScreen a -> ScreenState -> IO ()
execSimpleScreen (SimpleScreen s) state = void $ runStateT (void $ runExceptT s) state

-----------------------------------------------------------------------------

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
  output o    = lift $ outputMessage "ECHO" $ show o
  send s      = lift $ outputMessage "SEND" (show (Communication s)) >> sendToCurrentSocket s
  prompt p f  = lift $ do
                       outputMessage "PROMPT" p
                       assign scrMode $ PromptMode p $ unsafeCallback f
  bind k a    = return ()
  menu m      = return ()
  setPrompt p = return ()
  connect h p = lift $ connectScreen h p
  time        = lift $ use scrTime
  setStatus s = return ()

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

run :: (MBEvent e) => MBComponent SimpleScreen e EmptyRec u -> IO ()
run component = do
    st <- initSimpleScreen
    execSimpleScreen (runWithComponent component runner) st

runner :: (MBEvent e) => MBRunner e u SimpleScreen ()
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
                r <- fmap concat $ mapM (trigger . mkEv . LineEvent) ls
                mapM_ (liftMBRunner . defaultHandler) r
                lift $ do
                    scrPrompt .= newPrompt
                    scrAttr   .= newAttr
                    outputPrompt newPrompt
            SSendEvent str -> do
                curMode <- lift $ use scrMode
                case curMode of
                    NormalMode -> do
                        lift $ scrPrompt .= []
                        r <- trigger $ mkEv $ SendEvent str
                        mapM_ (liftMBRunner . defaultHandler) r
                    PromptMode _ f -> do
                        lift $ scrMode .= NormalMode
                        liftMBRunner $ runUnsafeCallback f str
            SCloseEvent -> do
                return ()
            STelnetEvent neg -> do
                r <- trigger $ mkEv $ TelnetEvent neg
                mapM_ (liftMBRunner . defaultHandler) r
            STimeEvent t -> do
                lift $ scrTime .= t
                r <- trigger $ mkEv $ TimeEvent t
                mapM_ (liftMBRunner . defaultHandler) r
            _ -> do
                return ()

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
        Right newSocket -> scrSocket .= Just newSocket
        Left err        -> outputMessage "ERROR" err
  where
    telnetProc chan ev = case ev of
        TelnetRawEvent s        -> liftIO $ telnetReceiveProc chan $ SReceiveEvent s
        TelnetNegEvent n        -> liftIO $ telnetReceiveProc chan $ STelnetEvent n
        TelnetCloseEvent reason -> liftIO $ telnetReceiveProc chan $ SCloseEvent

    telnetReceiveProc chan ev = atomically $ writeTChan chan ev

outputPrompt :: [Word8] -> SimpleScreen ()
outputPrompt p = liftIO $ do
    putStr "\r                                                                                   \r"
    putStr $ "[PROMPT  ] " ++ escapeAll p
    hFlush stdout

outputMessage :: String -> String -> SimpleScreen ()
outputMessage t msg = do
    p <- use scrPrompt
    liftIO $ do
        putStr "\r                                                                                   \r"
        putStrLn $ printf "[%-8s] %s" t msg
        putStr $ "[PROMPT  ] " ++ escapeAll p
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
