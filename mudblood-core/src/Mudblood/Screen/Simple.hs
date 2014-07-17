{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts, DeriveDataTypeable #-}

module Mudblood.Screen.Simple
    ( SimpleScreen
    , execSimpleScreen, initSimpleScreen, loopSimpleScreen
    ) where

import Mudblood.Class
import Mudblood.Core
import Mudblood.Text
import Mudblood.Telnet
import Mudblood.Error

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Error

import Control.Concurrent
import Control.Concurrent.STM

import Data.Time.Clock.POSIX

import Control.DynCallback

import qualified Codec.Binary.UTF8.String as UTF8

import System.IO
import Text.Printf

-----------------------------------------------------------------------------

newtype SimpleScreen a = SimpleScreen (ErrorT StackTrace (StateT ScreenState IO) a)
    deriving (Functor, Monad, MonadIO, MonadState ScreenState, Typeable, MonadError StackTrace)

execSimpleScreen :: SimpleScreen a -> ScreenState -> IO ()
execSimpleScreen (SimpleScreen s) state = void $ runStateT (void $ runErrorT s) state

-----------------------------------------------------------------------------

instance MB SimpleScreen SimpleScreen where
    liftScreen = id
    promptDyn p f = do
        outputMessage "PROMPT" p
        modify $ \s -> s { scrMode = PromptMode p f }
    bindDyn p f = return () -- unsupported
    connect h p = connectScreen h p
    send s = do
        outputMessage "SEND" $ show (Communication s)
        sendToCurrentSocket s
    echo s = outputMessage "ECHO" $ fromAS s

-----------------------------------------------------------------------------

data ScreenState = ScreenState
    { scrEventChan      :: TChan Event
    , scrSocket         :: Maybe TelnetSocket
    , scrQuit           :: Bool
    , scrPrompt         :: String
    , scrAttr           :: Attr
    , scrMode           :: Mode
    }

data Mode = NormalMode
          | PromptMode String (DynCallback String)

initSimpleScreen :: IO ScreenState
initSimpleScreen = do
    chan <- newTChanIO
    forkIO $ inputLoop chan
    forkIO $ timerLoop chan
    return $ ScreenState
        { scrPrompt = ""
        , scrAttr = defaultAttr
        , scrSocket = Nothing
        , scrEventChan = chan
        , scrQuit = False
        , scrMode = NormalMode
        }

-----------------------------------------------------------------------------

data Event = SReceiveEvent String
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
                                   (newprompt, newAttr) <- triggerReceive (scrPrompt st) chars (scrAttr st)
                                   liftScreen $ do
                                       modify $ \st -> st { scrPrompt = newprompt, scrAttr = newAttr }
                                       outputPrompt newprompt
            SSendEvent str       -> case scrMode st of
                NormalMode -> do
                    liftScreen $ modify $ \st -> st { scrPrompt = "" }
                    triggerSend str
                PromptMode _ f -> do
                                  liftScreen $ modify $ \s -> s { scrMode = NormalMode }
                                  g <- maybeError (stackTrace "screen" "runDynCallback failed. This is a bug, please report") $ runDynCallback f
                                  g str
            SCloseEvent          -> return ()
            STelnetEvent neg     -> triggerTelnet neg
            STimeEvent t         -> triggerTime t
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
        TelnetRawEvent s -> liftIO $ telnetReceiveProc chan $ SReceiveEvent $ UTF8.decode s
        TelnetNegEvent n -> liftIO $ telnetReceiveProc chan $ STelnetEvent n
        TelnetCloseEvent reason -> liftIO $ telnetReceiveProc chan $ SCloseEvent

    telnetReceiveProc chan ev = atomically $ writeTChan chan ev

outputPrompt :: String -> SimpleScreen ()
outputPrompt p = liftIO $ do
    putStr "\r                                                                                   \r"
    putStr $ "[PROMPT  ] " ++ p
    hFlush stdout

outputMessage :: String -> String -> SimpleScreen ()
outputMessage t msg = do
    pr <- gets scrPrompt
    liftIO $ do
        putStr "\r                                                                                   \r"
        putStrLn $ printf "[%-8s] %s" t msg
        putStr $ "[PROMPT  ] " ++ pr
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
