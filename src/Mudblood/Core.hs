{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Mudblood.Core
    ( 
    -- * The MB monad
      MB, runMB
    , MBState (mbLinebuffer, mbUserData), mkMBState
    , MBConfig (..), mkMBConfig
    , CommandHandler (CommandHandler)
    , LogSeverity (LogDebug, LogInfo, LogWarning, LogError)
    , StackTrace
    -- * The MBF Functor
    , MBF (..)
    -- * MB primitives
    , command, commands, setCommandHandler, quit
    , process, processSend, processTelnet, processTime
    , connect, modifyTriggers, trigger, runTrigger, feedTrigger
    , gmcpHello
    , MBMonad ( echo, echoA, echoAux, echoE, send, ui, io, sound, dialog, getUserData
              , putUserData, modifyUserData, getMap, putMap, modifyMap, getTime
              )
    -- * Triggers
    , MBTrigger, MBTriggerFlow
    -- * Error handling
    , catchError, throwError, strMsg, noMsg, stackError, justError, maybeError, eitherError, stackTrace, mapLeft
    ) where

import Data.Word
import Data.Maybe
import Data.List.Split

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Free
import Control.Monad.Error

import qualified Data.Map as M

import Control.Exception

import qualified Codec.Binary.UTF8.String as UTF8

import Mudblood.UI
import Mudblood.Telnet
import Mudblood.Text
import Mudblood.Trigger
import Mudblood.Mapper.Map
import Mudblood.Keys
import Mudblood.Sound
import Mudblood.Error
import Data.GMCP

import Debug.Trace

data LogSeverity = LogDebug
                 | LogInfo
                 | LogWarning
                 | LogError
    deriving (Show)

--------------------------------------------------------------------------------------------------

class (Monad m) => MBMonad m u | m -> u where
    echoA :: AttrString -> m ()

    echo :: String -> m ()
    echo = echoA . toAS

    echoAux :: String -> AttrString -> m ()

    echoE :: String -> m ()
    echoE = echoA . (setFg Red) . toAS

    send :: (Sendable a) => a -> m ()

    ui :: UIAction (MB u) -> m ()

    io :: IO a -> m a

    sound :: Sound a -> m a

    dialog :: UIDialogDescription -> (UIDialogResult -> MB u ()) -> m ()

    getUserData :: m u

    putUserData :: u -> m ()
    putUserData x = modifyUserData $ const x

    modifyUserData :: (u -> u) -> m ()
    modifyUserData f = getUserData >>= putUserData . f

    getMap :: m Map

    putMap :: Map -> m ()
    putMap m = modifyMap $ const m

    modifyMap :: (Map -> Map) -> m ()
    modifyMap f = getMap >>= putMap . f

    getTime :: m Int

instance MBMonad (MB u) u where
    echoA x = liftF $ MBFLine Nothing x ()
    echoAux name s = liftF $ MBFLine (Just name) s ()
    send x = liftF $ MBFSend (Communication x) ()
    ui action = liftF $ MBFUI action ()
    io action = liftF $ MBFIO action id
    sound action = liftF (MBFSound action id) >>= eitherError
    dialog desc handler = liftF $ MBFDialog desc handler ()
    getUserData = gets mbUserData
    modifyUserData f = modify $ \s -> s { mbUserData = f (mbUserData s) }
    getMap = gets mbMap
    putMap d = do modify $ \s -> s { mbMap = d }
                  liftF $ MBFUI (UIUpdateMap d) ()
    getTime = liftF $ MBFGetTime id

instance MBMonad (TriggerM (MB u) y r) u where
    echoA = liftT . echoA
    echoAux name s = liftT $ echoAux name s
    send = liftT . send
    ui = liftT . ui
    io = liftT . io
    sound = liftT . sound
    dialog desc handler = liftT $ dialog desc handler
    getUserData = liftT getUserData
    modifyUserData = liftT . modifyUserData
    getMap = liftT getMap
    putMap = liftT . putMap
    getTime = liftT getTime

instance MBMonad (StateT s (TriggerM (MB u) y r)) u where
    echoA = lift . echoA
    echoAux name s = lift $ echoAux name s
    send = lift . send
    ui = lift . ui
    io = lift . io
    sound = lift . sound
    dialog desc handler = lift $ dialog desc handler
    getUserData = lift getUserData
    modifyUserData = lift . modifyUserData
    getMap = lift getMap
    putMap = lift . putMap
    getTime = lift getTime

--------------------------------------------------------------------------------------------------

data MBState u = MBState
    { mbLinebuffer      :: [AttrString]
    , mbLog             :: [String]
    , mbTrigger         :: MBTriggerFlow u
    , mbUserData        :: u
    , mbMap             :: Map
    , mbCommandHandler  :: CommandHandler u
}

-- | Create a new MBState.
mkMBState :: MBTriggerFlow u                    -- ^ Triggers
          -> u                                  -- ^ User data
          -> MBState u

newtype CommandHandler u = CommandHandler { runCommandHandler :: String -> MB u (CommandHandler u) }

emptyHandler = CommandHandler $ const $ return emptyHandler

mkMBState triggers user = MBState
    { mbLinebuffer  = []
    , mbLog         = []
    , mbTrigger     = triggers
    , mbUserData    = user
    , mbMap         = mapEmpty
    , mbCommandHandler = emptyHandler
    }

data MBConfig u = MBConfig
    {
    }

mkMBConfig = MBConfig
    {
    }

data MBF u o = forall a. MBFIO (IO a) (a -> o)
             | MBFLine (Maybe String) AttrString o
             | MBFSend Communication o
             | MBFConnect String String o
             | MBFQuit o
             | MBFUI (UIAction (MB u)) o
             | MBFDialog UIDialogDescription (UIDialogResult -> MB u ()) o
             | MBFGetTime (Int -> o)
             | forall a. MBFSound (Sound a) (Either StackTrace a -> o)

instance Functor (MBF u) where
    fmap f (MBFIO action g)             = MBFIO action $ f . g
    fmap f (MBFUI action x)             = MBFUI action $ f x
    fmap f (MBFDialog desc handler x)   = MBFDialog desc handler $ f x
    fmap f (MBFLine buf l x)            = MBFLine buf l $ f x
    fmap f (MBFSend dat x)              = MBFSend dat $ f x
    fmap f (MBFConnect host port x)     = MBFConnect host port $ f x
    fmap f (MBFQuit x)                  = MBFQuit $ f x
    fmap f (MBFGetTime g)               = MBFGetTime $ f . g
    fmap f (MBFSound action g)          = MBFSound action $ f . g

--------------------------------------------------------------------------------------------------

newtype MB u a = MB (ReaderT (MBConfig u) (StateT (MBState u) (ErrorT StackTrace (Free (MBF u)))) a)
    deriving ( Monad, MonadState (MBState u), MonadReader (MBConfig u)
             , MonadError StackTrace, MonadFree (MBF u), Functor, Applicative )

runMB :: MBConfig u -> MBState u -> MB u a -> Free (MBF u) (Either StackTrace (a, MBState u))
runMB conf st (MB mb) = runErrorT (runStateT (runReaderT mb conf) st)

--------------------------------------------------------------------------------------------------

-- | Parse and execute a command
command :: String -> MB u ()
command c = do
    handler <- gets mbCommandHandler
    newHandler <- stackError (stackTrace "core" "Error executing command") $ runCommandHandler handler c
    setCommandHandler newHandler

-- | Run commands from a string
commands :: String -> MB u ()
commands s = forM_ (filter (/= "") (lines s)) command

setCommandHandler :: CommandHandler u -> MB u ()
setCommandHandler h = modify $ \s -> s { mbCommandHandler = h }

-- | Quit mudblood
quit :: MB u ()
quit = liftF $ MBFQuit ()

-- | Process a chunk of incoming line data.
process :: String               -- ^ Current unprocessed data ('prompt')
        -> String               -- ^ New data
        -> Attr                 -- ^ Last known attribute state
        -> MB u (String, Attr)  -- ^ Remaining unprocessed data and resulting attribute state
process pr str a =
    case lines' str of
        (l:[])      -> return (pr ++ l, a)
        (l:ls)      -> do
                       let (ls', attr) = foldr decodeFold ([], a) ((pr ++ l):(take (length ls - 1) ls))
                       mapM_ feedTrigger ls'
                       return (last ls, attr)
  where
    lines' str = splitWhen (== '\n') str
    decodeFold cur (l, a) = let (cur', add) = processChars cur
                                (line, attr) = fromMaybe (toAS ("[ERROR]" ++ cur'), a) (decodeAS cur' a)
                            in ((LineTEvent line):(add ++ l), attr)

-- | Handle certain special characters, like \a.
--   Results in a string of textual data and maybe some TriggerEvents.
processChars :: String -> (String, [TriggerEvent])
processChars s = foldr f ("", []) s
  where
    f c (str, add) =
        case c of
            '\a' -> (str, BellTEvent : add)
            x    -> (x:str, add)

-- | Process a line to send.
processSend :: String -> MB u ()
processSend str = do
    feedTrigger $ SendTEvent str

-- | Process an incoming telnet negotiation.
processTelnet :: TelnetNeg -> MB u ()
processTelnet neg = case neg of
    -- GMCP
    TelnetNeg (Just CMD_SB) (Just OPT_GMCP) dat ->
        case parseGMCP $ UTF8.decode dat of
            Nothing -> throwError $ stackTrace "core" "Received invalid GMCP"
            Just gmcp -> feedTrigger $ GMCPTEvent gmcp
    -- Else: trigger it
    _ -> feedTrigger $ TelnetTEvent neg

-- | Process a timer tick.
processTime :: Int -> MB u ()
processTime n = feedTrigger $ TimeTEvent n

-- | Feed a trigger event to the global trigger chain.
feedTrigger :: TriggerEvent -> MB u ()
feedTrigger ev =
    do
    s <- get
    (res, t') <- runTriggerFlow (mbTrigger s) ev
    modify $ \x -> x { mbTrigger = t' }
    mapM_ finishTEvent res

-- | This is called for each event that comes out of the trigger chain.
finishTEvent :: TriggerEvent -> MB u ()
finishTEvent ev = case ev of
    LineTEvent line -> echoA line
    SendTEvent line -> send line
    BellTEvent -> ui $ UIBell
    TelnetTEvent t -> do
        case t of
            TelnetNeg (Just CMD_DO) (Just OPT_TIMING_MARK) _ ->
                send $ TelnetNeg (Just CMD_WILL) (Just OPT_TIMING_MARK) []
            _ ->
                --echoA (setFg Magenta (toAttrString $ show t))
                return ()
    _ -> return ()

-- | Connect to game server.
connect :: String   -- ^ Host
        -> String   -- ^ Port
        -> MB u ()
connect h p = liftF $ MBFConnect h p ()

-- | Send a GMCP hello.
gmcpHello :: [String]           -- ^ A list of supported GMCP modules
          -> [Communication]
gmcpHello supports =
    [ Communication $ TelnetNeg (Just CMD_DO) (Just OPT_GMCP) []
    , Communication $ GMCP "Core.Hello" $
        JSObject $ toJSObject [ ("client", JSString $ toJSString "mudblood"),
                                ("version", JSString $ toJSString "0.1") -- TODO: Configure this somehow
                              ]
    , Communication $ GMCP "Core.Supports.Set" $ JSArray $ map (JSString . toJSString) supports
    ]

-- | Modify the global TriggerFlow
modifyTriggers :: (MBTriggerFlow u -> MBTriggerFlow u) -> MB u ()
modifyTriggers f = modify $ \s -> s { mbTrigger = f (mbTrigger s) }

-- | Add a one-shot trigger to the trigger chain.
trigger :: (TriggerEvent -> MBTrigger u [TriggerEvent]) -> MB u ()
trigger trig = modifyTriggers $ flip (<>) (Volatile trig)

-- | Add a one-shot trigger and execute it immediately. I.e. switch context from
--   MB to MBTrigger.
runTrigger :: MBTrigger u [TriggerEvent] -> MB u ()
runTrigger trig = trigger (const trig) >> feedTrigger NilTEvent

--------------------------------------------------------------------------------------------------

type MBTrigger u = TriggerM (MB u) [TriggerEvent] TriggerEvent
type MBTriggerFlow u = TriggerFlow (MB u) TriggerEvent

--------------------------------------------------------------------------------------------------

{-
 - TODO: Move to a separate module.
 -
mbCoreBuiltins :: [(String, Exp (MB u) Value)]
mbCoreBuiltins =
    [ ("quit", Function [] $ liftL quit >> return nil)
    , ("echo", Function ["string"] $ do
        x <- getSymbol "string"
        case x of
            Value (AttrStringValue x) -> liftL $ echoA x
            Value (StringValue x) -> liftL $ echo x
            _ -> typeError "string"
        return nil
      )
    , ("connect", Function ["host", "port"] $ do
        h <- getSymbol "host" >>= typeString
        p <- getSymbol "port" >>= typeInt
        liftL $ connect h (show p)
        return nil
      )
    , ("send", Function ["data"] $ do
        d <- getSymbol "data" >>= typeString
        liftL $ send d
        return nil
      )
    , ("cmd", Function ["name", "fun"] $ do
        name <- getSymbol "name" >>= typeString
        fun <- getSymbol "fun"
        liftL $ addCommand name fun
        return nil
      )
    , ("prompt", Function ["title", "function"] $ do
        title <- getSymbol "title" >>= typeString
        (sig, fun) <- getSymbol "function" >>= typeFunction
        case sig of
            [symbol] -> do
                liftL $ ui $ UIPrompt title $ \v -> do
                    ctx <- gets mbCoreContext
                    run (mkContext mbCoreBuiltins `mappend`
                         mkContext mbBuiltins `mappend`
                         ctx) $ List [Function [symbol] fun, Value $ StringValue v]
                    return ()
                return nil
            _ -> throwError "Prompt handler must be a function with exactly one argument"
        )
    , ("bind", Function ["keystring", "code"] $ do
        keystring <- getSymbol "keystring" >>= typeString
        code <- getSymbol "code" >>= typeString
        case parse parseValue code of
            Left e -> throwError $ "Parsing error: " ++ e
            Right exp -> case parseKeys keystring of
                Nothing -> throwError $ "Invalid keystring"
                Just ks -> liftL $ ui $ UIBind ks $ do
                    ctx <- gets mbCoreContext
                    ret <- run (mkContext mbCoreBuiltins `mappend` mkContext mbBuiltins `mappend` ctx) exp
                    case ret of
                        Left e -> echoE $ "Error in binding for " ++ (show ks) ++ ": " ++ e
                        Right r -> return ()
                    return ()
        return nil
      )
    , ("on-send", Function ["code"] $ do
        code <- getSymbol "code" >>= typeString
        case parse parseValue code of
            Left e -> throwError $ "Parsing error: " ++ e
            Right exp -> do
                liftL $ modifyTriggers $ fmap (:>>: (Permanent $ lispSendTrigger exp))
                return nil
      )
    , ("on-line", Function ["code"] $ do
        code <- getSymbol "code" >>= typeString
        case parse parseValue code of
            Left e -> throwError $ "Parsing error: " ++ e
            Right exp -> do
                liftL $ modifyTriggers $ fmap (:>>: (Permanent $ lispLineTrigger exp))
                return nil
      )
    ]

lispSendTrigger :: Exp (MBTrigger u) Value -> TriggerEvent -> MBTrigger u [TriggerEvent]
lispSendTrigger exp = guardSend >=> \x -> do
    res <- run (mkContext mbBuiltins `mappend` ctx x) exp
    case res of
        Right (Value (StringValue x)) -> return [SendTEvent x]
        Right (Value (AttrStringValue x)) -> return [SendTEvent (fromAttrString x)]
        Left err -> liftT (echoE ("Trigger error: " ++ err)) >> return [SendTEvent x]
        _ -> return []
  where
    ctx x = (mkContext [("$", mkStringValue x)])

lispLineTrigger :: Exp (MBTrigger u) Value -> TriggerEvent -> MBTrigger u [TriggerEvent]
lispLineTrigger exp = guardLine >=> \x -> do
    res <- run (mkContext mbBuiltins `mappend` ctx x) exp
    case res of
        Right (Value (StringValue x)) -> return [LineTEvent (toAttrString x)]
        Right (Value (AttrStringValue x)) -> return [LineTEvent x]
        Left err -> liftT (echoE ("Trigger error: " ++ err)) >> return [LineTEvent x]
        _ -> return []
  where
    ctx x = (mkContext [("$", mkAttrStringValue x)])
    -}
