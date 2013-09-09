{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Mudblood.Core
    ( 
    -- * The MB monad
      MB, runMB
    , MBState (mbLinebuffer, mbUserData), mkMBState
    , MBConfig (..), mkMBConfig
    , LogSeverity (LogDebug, LogInfo, LogWarning, LogError)
    , MBCommand
    -- * The MBF Functor
    , MBF (MBFIO, MBFLine, MBFSend, MBFConnect, MBFQuit, MBFUI)
    -- * MB primitives
    , command, commands, quit, logger, process, processSend, processTelnet, mbError
    , connect, modifyTriggers
    , initGMCP
    , MBMonad (echo, echoA, send, ui, io, getUserData, putUserData, modifyUserData, getMap, putMap, modifyMap)
    -- * Widgets
    , UIWidget (..)
    , getWidgets, modifyWidgets
    ) where

import Data.Word
import Data.List.Split
import Data.Dynamic

import Control.Applicative
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
import Mudblood.Mapper.Map
import Data.GMCP

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

    send :: (Sendable a) => a -> m ()

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

    getMap :: m Map

    putMap :: Map -> m ()
    putMap d = modifyMap (\_ -> d)

    modifyMap :: (Map -> Map) -> m ()
    modifyMap f = getMap >>= putMap . f

--------------------------------------------------------------------------------------------------

data MBState = MBState {
    mbLinebuffer :: [AttrString],
    mbLog :: [String],
    mbTrigger :: Maybe MBTriggerFlow,
    mbUserData :: Dynamic,
    mbMap :: Map,
    mbWidgets :: [UIWidget]
}

-- | Create a new MBState.
mkMBState :: (Typeable a) =>
             Maybe MBTriggerFlow                -- ^ Triggers
          -> a                                  -- ^ User data
          -> MBState

mkMBState triggers user = MBState {
    mbLinebuffer = [],
    mbLog = [],
    mbTrigger = triggers,
    mbUserData = toDyn user,
    mbMap = mapEmpty,
    mbWidgets = []
    }

data MBConfig = MBConfig {
    confCommands :: M.Map String MBCommand,
    confGMCPSupports :: [String]
}

mkMBConfig = MBConfig
    { confCommands = M.empty
    , confGMCPSupports = []
    }

data MBF o = forall a. MBFIO (IO a) (a -> o)
           | MBFLine AttrString o
           | MBFSend Communication o
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
    deriving (Monad, MonadState MBState, MonadReader MBConfig, MonadFree MBF, Functor, Applicative)

-- | Run the MB monad.
runMB :: MBConfig -> MBState -> MB a -> Free MBF (a, MBState)
runMB conf st (MB mb) = runStateT (runReaderT mb conf) st

dispatchIO :: IO a -> MB a
dispatchIO action = liftF $ MBFIO action id

dispatchLine :: AttrString -> MB ()
dispatchLine x = liftF $ MBFLine x ()

dispatchSend :: Communication -> MB ()
dispatchSend x = liftF $ MBFSend x ()

dispatchConnect :: String -> String -> MB ()
dispatchConnect h p = liftF $ MBFConnect h p ()

dispatchQuit :: MB ()
dispatchQuit = liftF $ MBFQuit ()

dispatchUI :: UIAction -> MB ()
dispatchUI a = liftF $ MBFUI a ()

--------------------------------------------------------------------------------------------------

type MBCommand = Command MB

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
                                    Left s -> mbError s
                                    Right _ -> return ()
                   Nothing     -> mbError "Invalid command"
    Left e -> mbError $ "Parse error in '" ++ c ++ "': " ++ e

-- | Run commands from a string
commands :: String -> MB ()
commands s = forM_ (filter (/= "") (lines s)) command

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

processSend :: String -> MB ()
processSend str = do
    trigger $ SendTEvent str

initGMCP :: MB ()
initGMCP = do
    send $ TelnetNeg (Just CMD_DO) (Just OPT_GMCP) []        
    send $ GMCP "Core.Hello" $
        JSObject $ toJSObject [ ("client", JSString $ toJSString "mudblood"),
                                ("version", JSString $ toJSString "0.1")
                              ]
    send $ GMCP "Core.Supports.Set" $
        JSArray $ [ JSString $ toJSString "MG.char 1"
                  , JSString $ toJSString "comm.channel 1"
                  , JSString $ toJSString "MG.room 1"
                  ]

processTelnet :: TelnetNeg -> MB ()
processTelnet neg = case neg of
    -- GMCP
    TelnetNeg (Just CMD_WILL) (Just OPT_GMCP) _ -> initGMCP
    TelnetNeg (Just CMD_SB) (Just OPT_GMCP) dat ->
        case parseGMCP $ UTF8.decode dat of
            Nothing -> mbError "Received invalid GMCP"
            Just gmcp -> trigger $ GMCPTEvent gmcp
    -- Else: output telneg
    x -> echoA (setFg Magenta (toAttrString $ show x))

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
        GMCPTEvent g -> return ()

-- | Output an error.
mbError :: String -> MB ()
mbError str = echo $ "Error: " ++ str

connect :: String -> String -> MB ()
connect = dispatchConnect

-- | Modify the global TriggerFlow
modifyTriggers :: (Maybe MBTriggerFlow -> Maybe MBTriggerFlow) -> MB ()
modifyTriggers f = modify $ \s -> s { mbTrigger = f (mbTrigger s) }

instance MBMonad MB where
    send = dispatchSend . Communication
    echoA = dispatchLine
    ui = dispatchUI
    io = dispatchIO

    getUserDataDynamic = gets mbUserData

    putUserDataDynamic d = do
        modify $ \st -> st { mbUserData = d }

    getMap = gets mbMap
    putMap d = do modify $ \s -> s { mbMap = d }
                  dispatchUI $ UIUpdateMap d

--------------------------------------------------------------------------------------------------

instance MBMonad (Trigger (MBTriggerF i) i y) where
    send s = liftF $ Action $ Send (Communication s) ()
    echo s = liftF $ Action $ Echo s ()
    ui action = liftF $ Action $ PutUI action ()
    io action = liftF $ Action $ RunIO action id

    getUserDataDynamic = liftF $ Action $ GetUserData id
    putUserDataDynamic d = liftF $ Action $ PutUserData d ()

    getMap = liftF $ Action $ GetMap id
    putMap d = liftF $ Action $ PutMap d ()

-- | Interpreter for the Trigger Monad
runTriggerMB :: Trigger (MBTriggerF i) i o o -> MB (TriggerResult (MBTriggerF i) i o o)
runTriggerMB (Pure r) = return $ TResult r
runTriggerMB (Free (Yield y f)) = return $ TYield y f
runTriggerMB (Free (Fail)) = return $ TFail
runTriggerMB (Free (Action (Echo s x))) = echo s >> runTriggerMB x
runTriggerMB (Free (Action (Send s x))) = dispatchSend s >> runTriggerMB x
runTriggerMB (Free (Action (GetUserData g))) = getUserDataDynamic >>= runTriggerMB . g
runTriggerMB (Free (Action (PutUserData d x))) = putUserDataDynamic d >> runTriggerMB x
runTriggerMB (Free (Action (PutUI a x))) = ui a >> runTriggerMB x
runTriggerMB (Free (Action (RunIO action f))) = io action >>= runTriggerMB . f
runTriggerMB (Free (Action (GetMap g))) = getMap >>= runTriggerMB . g
runTriggerMB (Free (Action (PutMap d x))) = putMap d >> runTriggerMB x

--------------------------------------------------------------------------------------------------

-- | Widgets are small pieces of information to be displayed by the screen.
data UIWidget = UIWidgetText (MB String)        -- ^ A singe line of text
              | UIWidgetTable (MB [[String]])   -- ^ A table of textual cells

-- | Get the widget list
getWidgets :: MB [UIWidget]
getWidgets = gets mbWidgets >>= return

-- | Modify the widget list
modifyWidgets :: ([UIWidget] -> [UIWidget]) -> MB ()
modifyWidgets f = modify $ \s -> s { mbWidgets = f (mbWidgets s) }

