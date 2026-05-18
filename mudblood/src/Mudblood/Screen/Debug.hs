{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Mudblood.Screen.Debug
    ( DebugScreen
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
import Mudblood.Screen

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

newtype DebugScreen a = DebugScreen (ExceptT StackTrace (StateT ScreenState IO) a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState ScreenState, MonadError StackTrace)

runDebugScreen :: DebugScreen a -> ScreenState -> IO (Either StackTrace a)
runDebugScreen (DebugScreen s) state = evalStateT (runExceptT s) state

-----------------------------------------------------------------------------

data Event = SReceiveEvent [Word8]
           | SSendEvent String
           | STelnetEvent TelnetNeg
           | SCloseEvent
           | SFifoEvent String
           | STimeEvent Int

data ScreenState = ScreenState
    { _scrEventChan      :: TChan Event
    , _scrSocket         :: Maybe TelnetSocket
    , _scrQuit           :: Bool
    , _scrPrompt         :: [Word8]
    , _scrAttr           :: Attr
    }

makeLenses ''ScreenState

type SMBR e u = MBR () e u DebugScreen

-----------------------------------------------------------------------------

instance Screen DebugScreen where
  outputS o    = case o of
                     OutputLine x  -> outputLine x
                     OutputError x -> outputMessage "ERROR" $ show x
                     OutputInfo x  -> return ()
                     OutputLog x   -> outputMessage "LOG" x
  sendS s      = do
    liftIO $ putStrLn $ "*** REAL SEND ***"
    liftIO $ putStrLn $ UTF8.decode (toBinary s)
    sendToCurrentSocket s
  setPromptS p = outputMessage "PROMPT" p
  connectS h p = connectScreen h p
  timeS        = return 0
  setStatusS s = return ()

-----------------------------------------------------------------------------

initDebugScreen :: IO ScreenState
initDebugScreen = do
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
        }

-----------------------------------------------------------------------------

run :: (MBEvent e) => MBComponent DebugScreen e () u -> IO ()
run component = do
    st <- initDebugScreen
    r <- runDebugScreen (runWithComponent component () runner) st
    case r of
        Left st -> putStrLn $ "PANIC: " ++ show st
        Right (Left st) -> putStrLn $ "PANIC: " ++ show st
        Right (Right ()) -> return ()

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
                liftIO $ putStrLn "*** RECEIVE ***"
                oldPrompt <- lift $ use scrPrompt
                oldAttr   <- lift $ use scrAttr
                let (ls, newPrompt, newAttr) = decode oldPrompt chars oldAttr
                lift $ scrPrompt .= newPrompt
                lift $ scrAttr   .= newAttr
                liftIO $ mapM_ (putStrLn . fromAS) ls
                lift $ outputPrompt newPrompt

                {-
                r <- fmap concat $ mapM (trigger . mkEv . LineEvent) ls
                mapM_ (liftMBRunner . defaultHandler) r
                lift $ do
                    scrPrompt .= newPrompt
                    scrAttr   .= newAttr
                    outputPrompt newPrompt
                -}
            SSendEvent str -> do
                liftIO $ putStrLn $ "*** SEND: " ++ str
                lift $ scrPrompt .= []
                triggerWithDefault defaultHandler $ mkEv $ SendEvent str
            SCloseEvent -> do
                liftIO $ putStrLn "*** CLOSE ***"
                return ()
            STelnetEvent neg -> do
                liftIO $ putStrLn "*** TELNET ***"
                liftIO $ putStrLn $ case telnetToGMCP neg of
                    Nothing -> show neg
                    Just gmcp -> show gmcp
                handleTelneg neg
            _ -> do
                return ()

-----------------------------------------------------------------------------

connectScreen :: String -> String -> DebugScreen ()
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

-----------------------------------------------------------------------------

handleTelneg :: (MBEvent e) => TelnetNeg -> SMBR e u ()
handleTelneg neg = do
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

outputLine :: AttrString -> DebugScreen ()
outputLine l = liftIO $ do
    putStr "\r                                                                                   \r"
    putStrLn $ fromAS l
    hFlush stdout

outputPrompt :: [Word8] -> DebugScreen ()
outputPrompt p = liftIO $ do
    putStr "\r                                                                                   \r"
    putStr $ "[PROMPT  ] " ++ escapeAll p
    hFlush stdout

outputMessage :: String -> String -> DebugScreen ()
outputMessage t msg = do
    p <- use scrPrompt
    liftIO $ do
        putStr "\r                                                                                   \r"
        putStrLn $ printf "[%-8s] %s" t msg
        putStr $ "[PROMPT  ] " ++ escapeAll p
        hFlush stdout

sendToCurrentSocket :: (Sendable a) => a -> DebugScreen ()
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
