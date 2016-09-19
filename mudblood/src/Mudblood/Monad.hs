{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mudblood.Monad
  ( MB (..)
  , MBR (..)
  , MBX (..)
  , MBRState (..)
  , Action, matchAction
  , actionWithoutArg, actionWithArg
  , zoomMB
  , zoomMBX
  , mbx, mbx'
  , dispatch
  , raise
  , liftMBR
  , mbrGetExtra, mbrPutExtra
  , connect
  , trigger, triggerWithDefault
  , getBindings
  , getStatus
  , command, command'
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer
import Control.Monad.Morph
import Control.Lens
import Control.Command

import Control.Trigger

import Data.Maybe
import Data.Menu
import qualified Data.Map as M

import Mudblood.Screen
import Mudblood.Text
import Mudblood.Keys
import Mudblood.Error
import Mudblood.Trigger.Event

data MBRState s e u m = MBRState
  { mbrTrigger   :: Trigger (Ev e) (MB u m) ()
  , mbrBindings  :: Maybe (Menu Key (Action (MBX e u m ())))
  , mbrCommands  :: M.Map String (Command (MBX e u m) ())
  , mbrStatus    :: MB u m String
  , mbrExtra     :: s
  }

newtype MB u m r = MB { runMB :: StateT u (ExceptT StackTrace m) r }
  deriving (Functor, Applicative, Monad, MonadState u, MonadError StackTrace, MonadIO)

instance MonadTrans (MB u) where
  lift = MB . lift . lift

instance MFunctor (MB u) where
  hoist f t = MB $ hoist (hoist f) (runMB t)

zoomMB :: (Monad m) => Lens' b a -> MB a m r -> MB b m r
zoomMB l mb = MB $ zoom l $ runMB mb

--------------------------------------------------------------------------------------------------

newtype Action m = Action { runAction :: Either m (String -> m) }

instance Functor Action where
    fmap f a = Action (bimap f (fmap f) (runAction a))

matchAction :: Action m -> Either m (String -> m)
matchAction = runAction

actionWithoutArg = Action . Left
actionWithArg = Action . Right

--------------------------------------------------------------------------------------------------

newtype MBR s e u m r = MBR { runMBR :: StateT (MBRState s e u m) (MB u m) r }
  deriving (Functor, Applicative, Monad, MonadState (MBRState s e u m), MonadError StackTrace)

liftMBR :: (Monad m) => MB u m r -> MBR s e u m r
liftMBR = MBR . lift

instance MonadTrans (MBR s e u) where
  lift = MBR . lift . lift

mbrGetExtra = gets mbrExtra
mbrPutExtra x = modify $ \s -> s { mbrExtra = x }

--------------------------------------------------------------------------------------------------

newtype MBX e u m r = MBX { runMBX :: WriterT ([Ev e], Maybe (Trigger (Ev e) (MB u m) ())) (MB u m) r }
  deriving (Functor, Applicative, Monad, MonadWriter ([Ev e], Maybe (Trigger (Ev e) (MB u m) ())), MonadError StackTrace, MonadIO, MonadState u)

instance MonadTrans (MBX e u) where
  lift = MBX . lift . lift

dispatch :: (Monad m) => Trigger (Ev a) (MB u m) () -> MBX a u m ()
dispatch t = MBX $ tell ([], Just t)

raise :: (Monad m) => Ev e -> MBX e u m ()
raise ev = MBX $ tell ([ev], Nothing)

connect :: (MBEvent e, Screen m) => String -> String -> MBX e u m ()
connect host port = lift $ connectS host port

zoomMBX :: (Monad m) => Lens' b a -> MBX e a m r -> MBX e b m r
zoomMBX l = MBX . mapWriterT (zoomMB l . (fmap (second (second (fmap (hoist (zoomMB l))))))) . runMBX

mbx :: (Monad m, MBEvent e) => MBX e u m r -> MBR s e u m (r, [Ev e])
mbx action = do
    (r, (evs, t)) <- liftMBR $ runWriterT $ runMBX action
    tr <- case t of
        Nothing -> return []
        Just t  -> do
            modify $ \s -> s { mbrTrigger = t <> mbrTrigger s }
            trigger $ mkEv NilEvent
    tr' <- fmap mconcat (mapM trigger evs)
    return (r, tr ++ tr')

mbx' :: (Monad m, MBEvent e) => (Ev e -> MBR s e u m ()) -> MBX e u m r -> MBR s e u m r
mbx' f action = do
    (r, evs) <- mbx action
    mapM_ f evs
    return r

--------------------------------------------------------------------------------------------------

trigger :: (MBEvent e, Monad m) => Ev e -> MBR s e u m [Ev e]
trigger ev = do
  t <- gets mbrTrigger
  (r, t') <- liftMBR $ execTrigger t [ev]
  modify $ \s -> s { mbrTrigger = t' }
  return r

getBindings :: (Monad m) => MBR s e u m (Menu Key (Action (MBX e u m ())))
getBindings = fromMaybe emptyMenu <$> gets mbrBindings

getStatus :: (Monad m) => MBR s e u m String
getStatus = do
    m <- gets mbrStatus
    liftMBR m

triggerWithDefault :: (Monad m, MBEvent e) => (Ev e -> MBR s e u m ()) -> Ev e -> MBR s e u m ()
triggerWithDefault handler ev = trigger ev >>= mapM_ handler

command :: (MBEvent e, Screen m, Error err, MonadError err m) => String -> MBR s e u m [Ev e]
command s = do
    (cmd, args) <- parseCommand s
    cmds <- gets mbrCommands
    case cmd of
        "commands" -> do
            let showCmd (name, c) = do
                  lift $ outputS $ OutputLine $ setStyle StyleBold $ toAS (getCommandDoc c)
                  lift $ outputS $ OutputLine $ toAS ""
            mapM_ showCmd (M.toList cmds)
            return []
        _ -> case M.lookup cmd cmds of
            Nothing -> throwError $ strMsg "Unknown command"
            Just cmd' -> fmap snd $ mbx (runCommand cmd' args)

command' :: (MBEvent e, Screen m, Error err, MonadError err m) => (Ev e -> MBR s e u m ()) -> String -> MBR s e u m ()
command' handler s = do
    r <- command s
    mapM_ handler r
