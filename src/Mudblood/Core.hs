{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Mudblood.Core
    ( 
    -- * The MB monad
      MB, runMB
    , MBState (mbLinebuffer, mbUserData), mkMBState
    , MBConfig (MBConfig)
    , LogSeverity (LogDebug, LogInfo, LogWarning, LogError)
    -- * The MBF Functor
    , MBF (MBFIO, MBFLine, MBFSend, MBFConnect, MBFQuit, MBFUI)
    -- * MB primitives
    , command, quit, logger, process, error
    , connect, sendBinary, modifyTriggers
    , MBMonad (echo, echoA, send, ui, io, getUserData, putUserData, modifyUserData)
    -- * Events
    -- ** The trigger event type
    , TriggerEvent (LineTEvent, SendTEvent, TelnetTEvent)
    -- ** Convenient type aliases
    , MBTrigger, MBTriggerFlow
    ) where

import Prelude hiding (catch, error)

import Data.Word
import Data.List.Split
import Data.Dynamic

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Free

import qualified Data.Map as M

import Control.Exception

import qualified Codec.Binary.UTF8.String as UTF8

import Mudblood.UI
import Mudblood.Telnet
import Mudblood.Text
import Mudblood.Trigger
import Mudblood.Command

import Debug.Trace

data LogSeverity = LogDebug
                 | LogInfo
                 | LogWarning
                 | LogError
    deriving (Show)

--------------------------------------------------------------------------------------------------

-- | Type class that contains common functions of triggers and MB actions.
class (Monad m) => MBMonad m where
    echoA :: AttrString -> m ()
    echoA = echo . fromAttrString

    echo :: String -> m ()
    echo = echoA . toAttrString

    send :: String -> m ()

    ui :: UIAction -> m ()

    io :: IO a -> m a

    getUserDataDynamic :: m Dynamic

    modifyUserDataDynamic :: (Dynamic -> Dynamic) -> m ()
    modifyUserDataDynamic f = getUserDataDynamic >>= putUserDataDynamic . f

    putUserDataDynamic :: Dynamic -> m ()
    putUserDataDynamic d = modifyUserDataDynamic (\_ -> d)

    getUserData :: (Typeable a) => m a
    getUserData = do
        ud <- getUserDataDynamic
        case fromDynamic ud of
            Just ud' -> return ud'
            Nothing  -> fail "THIS IS A BUG: User data corrupted."

    putUserData :: (Typeable a) => a -> m ()
    putUserData d = putUserDataDynamic (toDyn d)

    modifyUserData :: (Typeable a) => (a -> a) -> m ()
    modifyUserData f = getUserData >>= putUserData . f

--------------------------------------------------------------------------------------------------

data MBState = MBState {
    mbLinebuffer :: [AttrString],
    mbLog :: [String],
    mbTrigger :: Maybe (TriggerFlow TriggerEvent),
    --mbUiValues :: M.Map String UiValue,
    mbUserData :: Dynamic
}

-- | Create a new MBState.
mkMBState :: (Typeable a) =>
             Maybe (TriggerFlow TriggerEvent)   -- ^ Triggers
          -> a                                  -- ^ User data
          -> MBState

mkMBState triggers user = MBState {
    mbLinebuffer = [],
    mbLog = [],
    mbTrigger = triggers,
    --mbUiValues = M.empty,
    mbUserData = toDyn user
    }

data MBConfig = MBConfig {
    confCommands :: M.Map String (Command MB)
}

data MBF o = forall a. MBFIO (IO a) (a -> o)
           | MBFLine AttrString o
           | MBFSend [Word8] o
           | MBFConnect String String o
           | MBFQuit o
           | MBFUI UIAction o

instance Functor MBF where
    fmap f (MBFIO action g) = MBFIO action $ f . g
    fmap f (MBFUI action x) = MBFUI action $ f x
    fmap f (MBFLine l x) = MBFLine l $ f x
    fmap f (MBFSend dat x) = MBFSend dat $ f x
    fmap f (MBFConnect host port x) = MBFConnect host port $ f x
    fmap f (MBFQuit x) = MBFQuit $ f x

--------------------------------------------------------------------------------------------------

newtype MB a = MB (ReaderT MBConfig (StateT MBState (Free MBF)) a)
    deriving (Monad, MonadState MBState, MonadReader MBConfig, MonadFree MBF)

-- | Run the MB monad.
runMB :: MBConfig -> MBState -> MB a -> Free MBF (a, MBState)
runMB conf st (MB mb) = runStateT (runReaderT mb conf) st

dispatchIO :: IO a -> MB a
dispatchIO action = liftF $ MBFIO action id

dispatchLine :: AttrString -> MB ()
dispatchLine x = liftF $ MBFLine x ()

dispatchSend :: [Word8] -> MB ()
dispatchSend x = liftF $ MBFSend x ()

dispatchConnect :: String -> String -> MB ()
dispatchConnect h p = liftF $ MBFConnect h p ()

dispatchQuit :: MB ()
dispatchQuit = liftF $ MBFQuit ()

dispatchUI :: UIAction -> MB ()
dispatchUI a = liftF $ MBFUI a ()

--------------------------------------------------------------------------------------------------

type MBTrigger a = Trigger TriggerEvent [TriggerEvent] a
type MBTriggerFlow = TriggerFlow TriggerEvent

data TriggerEvent = LineTEvent AttrString   -- ^ Emitted when a line was received from the host
                  | SendTEvent String       -- ^ Emitted when the user wants to send a line of input
                  | TelnetTEvent TelnetNeg  -- ^ Emitted when a telnet negotiation is received

--------------------------------------------------------------------------------------------------

-- | Parse and execute a command
command :: String -> MB ()
command c = case parseCommand c of
    Right (c', args) -> do
               conf <- ask
               case M.lookup c' (confCommands conf) of
                   Just c''    -> do
                                  res <- runCommand (cmdAction c'') args
                                  case res of
                                    Left s -> error s
                                    Right _ -> return ()
                   Nothing     -> error "Invalid command"
    Left e -> error $ "Parse error in '" ++ c ++ "': " ++ e

-- | Quit mudblood
quit :: MB ()
quit = dispatchQuit

-- | General logging function
logger :: LogSeverity -> String -> MB ()
logger sev str = modify $ \s -> s { mbLog = ((show sev) ++ str):(mbLog s) }

-- | Process a chunk of incoming data, i.e. pass the trigger chain and add the
--   result to the main linebugger.
process :: String -> String -> Attr -> MB (String, Attr)
process pr str a =
    case lines' str of
        (l:[])      -> return (pr ++ l, a)
        (l:ls)      -> do
                       let (ls', attr) = foldr decodeFold ([], a) ((pr ++ l):(take (length ls - 1) ls))
                       mapM_ trigger (map LineTEvent ls')
                       return (last ls, attr)
  where
    lines' str = splitWhen (== '\n') str
    decodeFold cur (l, a) = let (line, attr) = decode cur a
                            in (line:l, attr)

-- | Feed a trigger event to the global trigger chain.
trigger :: TriggerEvent -> MB ()
trigger ev =
    do
    s <- get
    case (mbTrigger s) of
        Nothing -> finishTEvent ev
        Just t -> do
                  (res, t') <- runTriggerFlow runTriggerMB t ev
                  modify $ \x -> x { mbTrigger = t' }
                  mapM_ finishTEvent res
  where
    finishTEvent :: TriggerEvent -> MB ()
    finishTEvent ev = case ev of
        LineTEvent line -> echoA line
        SendTEvent line -> send line
        TelnetTEvent t -> return ()

-- | Output an error.
error :: String -> MB ()
error str = echo $ "Error: " ++ str

connect :: String -> String -> MB ()
connect = dispatchConnect

-- | Send binary data to the socket.
sendBinary :: [Word8] -> MB ()
sendBinary = dispatchSend

-- | Modify the global TriggerFlow
modifyTriggers :: (Maybe MBTriggerFlow -> Maybe MBTriggerFlow) -> MB ()
modifyTriggers f = modify $ \s -> s { mbTrigger = f (mbTrigger s) }

{-
-- | Send a string to the socket. UTF8 will be assumed as encoding.
-- | Set a UI value
setUiValue :: String -> UiValue -> MB ()
setUiValue k v = case (k, v) of
    ("status", UiStringValue str) -> liftF $ MBFUI (MBUIStatus str) ()
    (k, v) -> liftF $ MBFUI (MBUISetValue k v) ()
-}

instance MBMonad MB where
    send str = sendBinary $ UTF8.encode $ str ++ "\n"
    echoA = dispatchLine
    ui = dispatchUI
    io = dispatchIO

    getUserDataDynamic = gets mbUserData

    putUserDataDynamic d = do
        modify $ \st -> st { mbUserData = d }

--------------------------------------------------------------------------------------------------

instance MBMonad (Trigger i y) where
    send s = liftF $ Send s ()
    echo s = liftF $ Echo s ()
    ui action = liftF $ PutUI action ()
    io action = liftF $ RunIO action id

    getUserDataDynamic = liftF $ GetUserData id
    putUserDataDynamic d = liftF $ PutUserData d ()

-- | Interpreter for the Trigger Monad
runTriggerMB :: Trigger i o o -> MB (TriggerResult i o o)
runTriggerMB (Pure r) = return $ TResult r
runTriggerMB (Free (Yield y f)) = return $ TYield y f
runTriggerMB (Free (Fail)) = return $ TFail
runTriggerMB (Free (Echo s x)) = echo s >> runTriggerMB x
runTriggerMB (Free (Send s x)) = send (s ++ "\n") >> runTriggerMB x
runTriggerMB (Free (GetUserData g)) = getUserDataDynamic >>= runTriggerMB . g
runTriggerMB (Free (PutUserData d x)) = putUserDataDynamic d >> runTriggerMB x
runTriggerMB (Free (PutUI a x)) = ui a >> runTriggerMB x
runTriggerMB (Free (RunIO action f)) = io action >>= runTriggerMB . f


