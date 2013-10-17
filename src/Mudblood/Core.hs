{-# LANGUAGE ExistentialQuantification, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Mudblood.Core
    ( 
    -- * The MB monad
      MB, runMB
    , MBState (mbLinebuffer, mbUserData), mkMBState
    , MBConfig (..), mkMBConfig
    , LogSeverity (LogDebug, LogInfo, LogWarning, LogError)
    -- * The MBF Functor
    , MBF (..)
    -- * MB primitives
    , command, commands, quit, logger, process, processSend, processTelnet, processTime, mbError
    , addCommand
    , connect, modifyTriggers
    , gmcpHello
    , MBMonad (echo, echoA, echoAux, echoErr, send, ui, io, getUserData, putUserData, modifyUserData, getMap, putMap, modifyMap, getTime)
    -- * Triggers
    , MBTrigger, MBTriggerM, MBTriggerFlow
    ) where

import Data.Word
import Data.List.Split
--import Data.Dynamic

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Free

import Language.DLisp.Core
import Mudblood.Language

import qualified Data.Map as M

import Control.Exception

import qualified Codec.Binary.UTF8.String as UTF8

import Mudblood.UI
import Mudblood.Telnet
import Mudblood.Text
import Mudblood.Trigger
import Mudblood.Mapper.Map
import Mudblood.Keys
import Data.GMCP

import Debug.Trace

import Paths_mudblood
import Data.Version

data LogSeverity = LogDebug
                 | LogInfo
                 | LogWarning
                 | LogError
    deriving (Show)

--------------------------------------------------------------------------------------------------

class (Monad m) => MBMonad m u | m -> u where
    echoA :: AttrString -> m ()

    echo :: String -> m ()
    echo = echoA . toAttrString

    echoAux :: String -> AttrString -> m ()

    echoErr :: String -> m ()
    echoErr = echoA . (setFg Red) . toAttrString

    send :: (Sendable a) => a -> m ()

    ui :: UIAction (MB u) -> m ()

    io :: IO a -> m a

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
    echoA x = liftF $ MBFLine x ()
    echoAux name s = liftF $ MBFToBuffer name s ()
    send x = liftF $ MBFSend (Communication x) ()
    ui action = liftF $ MBFUI action ()
    io action = liftF $ MBFIO action id
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
    getUserData = liftT getUserData
    modifyUserData = liftT . modifyUserData
    getMap = liftT getMap
    putMap = liftT . putMap
    getTime = liftT getTime

--------------------------------------------------------------------------------------------------

data MBState u = MBState {
    mbLinebuffer :: [AttrString],
    mbLog :: [String],
    mbTrigger :: Maybe (MBTriggerFlow u),
    mbUserData :: u,
    mbMap :: Map,
    mbCoreContext :: Context (MB u) Value,
    mbTriggerContext :: Context (MBTriggerM u) Value
}

-- | Create a new MBState.
mkMBState :: Maybe (MBTriggerFlow u)            -- ^ Triggers
          -> u                                  -- ^ User data
          -> [(String, Exp (MB u) Value)]
          -> MBState u

mkMBState triggers user funcs = MBState {
    mbLinebuffer = [],
    mbLog = [],
    mbTrigger = triggers,
    mbUserData = user,
    mbMap = mapEmpty,
    mbCoreContext = mkContext funcs,
    mbTriggerContext = mkContext []
    }

data MBConfig = MBConfig {
    confGMCPSupports :: [String]
}

mkMBConfig = MBConfig
    { confGMCPSupports = []
    }

data MBF u o = forall a. MBFIO (IO a) (a -> o)
             | MBFLine AttrString o
             | MBFToBuffer String AttrString o
             | MBFSend Communication o
             | MBFConnect String String o
             | MBFQuit o
             | MBFUI (UIAction (MB u)) o
             | MBFGetTime (Int -> o)

instance Functor (MBF u) where
    fmap f (MBFIO action g) = MBFIO action $ f . g
    fmap f (MBFUI action x) = MBFUI action $ f x
    fmap f (MBFLine l x) = MBFLine l $ f x
    fmap f (MBFToBuffer name s x) = MBFToBuffer name s $ f x
    fmap f (MBFSend dat x) = MBFSend dat $ f x
    fmap f (MBFConnect host port x) = MBFConnect host port $ f x
    fmap f (MBFQuit x) = MBFQuit $ f x
    fmap f (MBFGetTime g) = MBFGetTime $ f . g

--------------------------------------------------------------------------------------------------

newtype MB u a = MB (ReaderT MBConfig (StateT (MBState u) (Free (MBF u))) a)
    deriving (Monad, MonadState (MBState u), MonadReader MBConfig, MonadFree (MBF u), Functor, Applicative)

-- | Run the MB monad.
runMB :: MBConfig -> MBState u -> MB u a -> Free (MBF u) (a, MBState u)
runMB conf st (MB mb) = runStateT (runReaderT mb conf) st

--------------------------------------------------------------------------------------------------

-- | Parse and execute a command
command :: String -> MB u ()
command c = do
    ctx <- gets mbCoreContext
    let exp = parse parseValue c
    case exp of
        Left e -> mbError $ "Parsing error: " ++ e
        Right exp -> do
            res <- run (mkContext mbBuiltins `mappend` mkContext mbCoreBuiltins `mappend` ctx) exp
            case res of
                Right r -> case r of
                            List [] -> return ()
                            _       -> echo $ show r
                Left e  -> mbError e

-- | Run commands from a string
commands :: String -> MB u ()
commands s = forM_ (filter (/= "") (lines s)) command

addCommand name fun = modify $ \s -> s { mbCoreContext = M.insert name fun (mbCoreContext s) }

-- | Quit mudblood
quit :: MB u ()
quit = liftF $ MBFQuit ()

-- | General logging function
logger :: LogSeverity -> String -> MB u ()
logger sev str = modify $ \s -> s { mbLog = ((show sev) ++ str):(mbLog s) }

-- | Process a chunk of incoming data, i.e. pass the trigger chain and add the
--   result to the main linebugger.
process :: String -> String -> Attr -> MB u (String, Attr)
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

processSend :: String -> MB u ()
processSend str = do
    trigger $ SendTEvent str

gmcpHello :: [String] -> [Communication]
gmcpHello supports =
    [ Communication $ TelnetNeg (Just CMD_DO) (Just OPT_GMCP) []
    , Communication $ GMCP "Core.Hello" $
        JSObject $ toJSObject [ ("client", JSString $ toJSString "mudblood"),
                                ("version", JSString $ toJSString (showVersion version))
                              ]
    , Communication $ GMCP "Core.Supports.Set" $ JSArray $ map (JSString . toJSString) supports
    ]

processTelnet :: TelnetNeg -> MB u ()
processTelnet neg = case neg of
    -- GMCP
    TelnetNeg (Just CMD_SB) (Just OPT_GMCP) dat ->
        case parseGMCP $ UTF8.decode dat of
            Nothing -> mbError "Received invalid GMCP"
            Just gmcp -> trigger $ GMCPTEvent gmcp
    -- Else: trigger it
    _ -> trigger $ TelnetTEvent neg

processTime :: Int -> MB u ()
processTime n = trigger $ TimeTEvent n

-- | Feed a trigger event to the global trigger chain.
trigger :: TriggerEvent -> MB u ()
trigger ev =
    do
    s <- get
    case (mbTrigger s) of
        Nothing -> finishTEvent ev
        Just t -> do
                  (res, t') <- runTriggerFlow t ev
                  modify $ \x -> x { mbTrigger = t' }
                  mapM_ finishTEvent res
  where
    finishTEvent :: TriggerEvent -> MB u ()
    finishTEvent ev = case ev of
        LineTEvent line -> echoA line
        SendTEvent line -> send line
        TelnetTEvent t -> echoA (setFg Magenta (toAttrString $ show t))
        GMCPTEvent g -> return ()
        TimeTEvent n -> return ()

-- | Output an error.
mbError :: String -> MB u ()
mbError str = echo $ "Error: " ++ str

connect :: String -> String -> MB u ()
connect h p = liftF $ MBFConnect h p ()

-- | Modify the global TriggerFlow
modifyTriggers :: (Maybe (MBTriggerFlow u) -> Maybe (MBTriggerFlow u)) -> MB u ()
modifyTriggers f = modify $ \s -> s { mbTrigger = f (mbTrigger s) }

--------------------------------------------------------------------------------------------------

type MBTrigger u = Trigger (MB u) [TriggerEvent] TriggerEvent
type MBTriggerM u = TriggerM (MB u) [TriggerEvent] TriggerEvent
type MBTriggerFlow u = TriggerFlow (MB u) TriggerEvent

--------------------------------------------------------------------------------------------------

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
                        Left e -> mbError $ "Error in binding for " ++ (show ks) ++ ": " ++ e
                        Right r -> return ()
                    return ()
        return nil
      )
    , ("on-send", Function ["code"] $ do
        code <- getSymbol "code" >>= typeString
        case parse parseValue code of
            Left e -> throwError $ "Parsing error: " ++ e
            Right exp -> do
                liftL $ modifyTriggers $ fmap (:>>: (Permanent $ marr $ lispSendTrigger exp))
                return nil
      )
    , ("on-line", Function ["code"] $ do
        code <- getSymbol "code" >>= typeString
        case parse parseValue code of
            Left e -> throwError $ "Parsing error: " ++ e
            Right exp -> do
                liftL $ modifyTriggers $ fmap (:>>: (Permanent $ marr $ lispLineTrigger exp))
                return nil
      )
    ]

lispSendTrigger :: Exp (MBTriggerM u) Value -> TriggerEvent -> MBTriggerM u [TriggerEvent]
lispSendTrigger exp = guardSend >=> \x -> do
    res <- run (mkContext mbBuiltins `mappend` ctx x) exp
    case res of
        Right (Value (StringValue x)) -> return [SendTEvent x]
        Right (Value (AttrStringValue x)) -> return [SendTEvent (fromAttrString x)]
        Left err -> liftT (echoErr ("Trigger error: " ++ err)) >> return [SendTEvent x]
        _ -> return []
  where
    ctx x = (mkContext [("$", mkStringValue x)])

lispLineTrigger :: Exp (MBTriggerM u) Value -> TriggerEvent -> MBTriggerM u [TriggerEvent]
lispLineTrigger exp = guardLine >=> \x -> do
    res <- run (mkContext mbBuiltins `mappend` ctx x) exp
    case res of
        Right (Value (StringValue x)) -> return [LineTEvent (toAttrString x)]
        Right (Value (AttrStringValue x)) -> return [LineTEvent x]
        Left err -> liftT (echoErr ("Trigger error: " ++ err)) >> return [LineTEvent x]
        _ -> return []
  where
    ctx x = (mkContext [("$", mkAttrStringValue x)])
