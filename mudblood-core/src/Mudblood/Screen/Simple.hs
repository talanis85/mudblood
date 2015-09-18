{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts, DeriveDataTypeable #-}

module Mudblood.Screen.Simple
    ( SimpleScreen
    , evalSimpleScreen, execSimpleScreen, initSimpleScreen, loopSimpleScreen
    ) where

import Mudblood.Class
import Mudblood.Core
import Mudblood.Text
import Mudblood.Telnet
import Mudblood.Trigger
import Mudblood.Error

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

import Control.Concurrent
import Control.Concurrent.STM

import Data.Time.Clock.POSIX

import Control.UnsafeCallback

import qualified Codec.Binary.UTF8.String as UTF8

import System.IO
import Text.Printf
import Data.Word

-----------------------------------------------------------------------------

newtype SimpleScreen a = SimpleScreen (ExceptT StackTrace (StateT ScreenState IO) a)
    deriving (Functor, Monad, MonadIO, MonadState ScreenState, MonadError StackTrace)

execSimpleScreen :: SimpleScreen a -> ScreenState -> IO ()
execSimpleScreen (SimpleScreen s) state = void $ runStateT (void $ runExceptT s) state

evalSimpleScreen :: SimpleScreen a -> ScreenState -> IO (Either StackTrace a)
evalSimpleScreen (SimpleScreen s) state = evalStateT (runExceptT s) state

-----------------------------------------------------------------------------

instance MB SimpleScreen SimpleScreen where
    liftScreen = id
    promptDyn p f = do
        outputMessage "PROMPT" p
        modify $ \s -> s { scrMode = PromptMode p f }
    bindDyn p f = return () -- unsupported
    menuDyn m = return () -- unsupported
    connect h p = connectScreen h p
    send s = do
        outputMessage "SEND" $ show (Communication s)
        sendToCurrentSocket s
    setPrompt p = return () -- unsupported
    output o = outputMessage "ECHO" $ show o
    time = gets scrTime

-----------------------------------------------------------------------------

data ScreenState = ScreenState
    { scrEventChan      :: TChan Event
    , scrSocket         :: Maybe TelnetSocket
    , scrQuit           :: Bool
    , scrPrompt         :: [Word8]
    , scrAttr           :: Attr
    , scrMode           :: Mode
    , scrTime           :: Ticks
    }

data Mode = NormalMode
          | PromptMode String (UnsafeCallback String)

initSimpleScreen :: IO ScreenState
initSimpleScreen = do
    chan <- newTChanIO
    t <- getPOSIXTime
    forkIO $ inputLoop chan
    forkIO $ timerLoop chan
    return $ ScreenState
        { scrPrompt = []
        , scrAttr = defaultAttr
        , scrSocket = Nothing
        , scrEventChan = chan
        , scrQuit = False
        , scrMode = NormalMode
        , scrTime = floor $ toRational t
        }

-----------------------------------------------------------------------------

data Event = SReceiveEvent [Word8]
           | SSendEvent String
           | STelnetEvent TelnetNeg
           | SCloseEvent
           | SFifoEvent String
           | STimeEvent Int

loopSimpleScreen :: (Game SimpleScreen m) => m ()
loopSimpleScreen = do
    ev <- liftScreen $ get >>= liftIO . atomically . readTChan . scrEventChan
    catchError (handleEvent ev)
               (liftScreen . outputMessage "ERROR" . show)
    liftScreen (gets scrQuit) >>= (flip when loopSimpleScreen) . not
  where
    handleEvent ev = do
        st <- liftScreen get
        case ev of
            SReceiveEvent chars  -> do
                                   let (ls, newprompt, newAttr) = decode (scrPrompt st) chars (scrAttr st)
                                   mapM_ triggerLine ls
                                   liftScreen $ do
                                       modify $ \st -> st { scrPrompt = newprompt, scrAttr = newAttr }
                                       outputPrompt newprompt
            SSendEvent str       -> case scrMode st of
                NormalMode -> do
                    liftScreen $ modify $ \st -> st { scrPrompt = [] }
                    triggerSend str
                PromptMode _ f -> do
                                  liftScreen $ modify $ \s -> s { scrMode = NormalMode }
                                  g <- hoistMaybe
                                       (stackTrace "screen" "runUnsafeCallback failed. This is a bug, please report")
                                       (runUnsafeCallback f)
                                  g str
            SCloseEvent          -> return ()
            STelnetEvent neg     -> triggerTelnet neg
            STimeEvent t         -> do
                                    liftScreen $ modify $ \st -> st { scrTime = t }
                                    triggerTime t
            _                   -> return ()

-----------------------------------------------------------------------------

connectScreen :: String -> String -> SimpleScreen ()
connectScreen host port =
    do
    state <- get
    case scrSocket state of
        Nothing -> return ()
        Just oldsock -> liftIO $ telnetClose oldsock
    sock <- liftIO . telnetConnect host port $ telnetRecvHandler $ telnetProc (scrEventChan state)
    case sock of
        Right sock' -> modify $ \s -> s { scrSocket = Just sock' }
        Left err -> outputMessage "ERROR" err
  where
    telnetProc chan ev = case ev of
        TelnetRawEvent s -> liftIO $ telnetReceiveProc chan $ SReceiveEvent s
        TelnetNegEvent n -> liftIO $ telnetReceiveProc chan $ STelnetEvent n
        TelnetCloseEvent reason -> liftIO $ telnetReceiveProc chan $ SCloseEvent

    telnetReceiveProc chan ev = atomically $ writeTChan chan ev

outputPrompt :: [Word8] -> SimpleScreen ()
outputPrompt p = liftIO $ do
    putStr "\r                                                                                   \r"
    putStr $ "[PROMPT  ] " ++ escapeAll p
    hFlush stdout

outputMessage :: String -> String -> SimpleScreen ()
outputMessage t msg = do
    pr <- gets scrPrompt
    liftIO $ do
        putStr "\r                                                                                   \r"
        putStrLn $ printf "[%-8s] %s" t msg
        putStr $ "[PROMPT  ] " ++ escapeAll pr
        hFlush stdout

sendToCurrentSocket :: (Sendable a) => a -> SimpleScreen ()
sendToCurrentSocket dat =
    do
    state <- get
    case (scrSocket state) of
         Just sock -> liftIO $ telnetSend sock $ Communication dat
         Nothing -> return ()

timerLoop :: TChan Event -> IO ()
timerLoop chan = forever $ do
    t <- getPOSIXTime
    atomically $ writeTChan chan $ STimeEvent $ floor $ toRational t
    threadDelay 1000000

inputLoop :: TChan Event -> IO ()
inputLoop chan = forever $ do
    l <- getLine
    atomically $ writeTChan chan $ SSendEvent l
