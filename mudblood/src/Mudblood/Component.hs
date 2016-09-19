{-# LANGUAGE DataKinds #-}
module Mudblood.Component
  ( MBComponent
  , Priority
  , module Control.Component.Stateful
  , (>>>)
  , runWithComponent
  , insertMonoid

  , nilC, stateC, triggerC, bootC, shutdownC, statusC, commandC
  , submenu, bind, bindArg

  , describe, description

  , module Data.Menu
  ) where

import Prelude hiding ((.), id)

import Control.Category
import Control.Lens
import Control.Component.Stateful
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Morph
import Control.Trigger
import Control.Command

import Data.Carte
import Data.List
import Data.Menu
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M

import Mudblood.Class
import Mudblood.Screen
import Mudblood.Text
import Mudblood.Error
import Mudblood.Monad
import Mudblood.Trigger.Event
import Mudblood.Keys

type Priority = Int

data MBComponentEffect m e u = MBComponentEffect
    { ceffTriggers :: M.Map Priority (Trigger (Ev e) (MB u m) ())
    , ceffBindings :: Maybe (Menu Key (Action (MBX e u m ())))
    , ceffBoot :: MBX e u m ()
    , ceffShutdown :: MBX e u m ()
    , ceffCommands :: M.Map String (Command (MBX e u m) ())
    , ceffStatus :: MB u m String
    , ceffDescription :: String
    }

instance (Monad m) => StateEffect (MBComponentEffect m e) where
    seEmpty = MBComponentEffect
        { ceffTriggers = mempty
        , ceffBindings = mempty
        , ceffBoot = return ()
        , ceffShutdown = return ()
        , ceffCommands = mempty
        , ceffStatus = return ""
        , ceffDescription = ""
        }
    seMap l h = h
        { ceffTriggers = fmap (hoist (zoomMB l)) $ ceffTriggers h
        , ceffBindings = fmap (fmap (fmap (zoomMBX l))) $ ceffBindings h
        , ceffBoot = zoomMBX l $ ceffBoot h
        , ceffShutdown = zoomMBX l $ ceffShutdown h
        , ceffCommands = fmap (mapCommand (hoist (zoomMBX l))) $ ceffCommands h
        , ceffStatus = zoomMB l $ ceffStatus h
        }
    seCat a b = MBComponentEffect
        { ceffTriggers = M.unionWith (<>) (ceffTriggers a) (ceffTriggers b)
        , ceffBindings = ceffBindings a <> ceffBindings b
        , ceffBoot = ceffBoot a >> ceffBoot b
        , ceffShutdown = ceffShutdown a >> ceffShutdown b
        , ceffCommands = M.union (ceffCommands a) (ceffCommands b)
        , ceffStatus = (intercalate " | " . filter (/= "")) <$> sequence [ceffStatus a, ceffStatus b]
        , ceffDescription = intercalate "\n" $ filter (/= "") [ceffDescription a, ceffDescription b]
        }

type MBComponent m e = StatefulComponent (MBComponentEffect m e)

runWithComponent :: (MBEvent e, Screen m) => MBComponent m e () u -> s -> MBR s e u m r -> m (Either StackTrace r)
runWithComponent component extraState act = runExceptT (evalStateT (runMB (evalStateT (runMBR act') initMBRState)) initMBState)
  where
    act' = do
        boot
        r <- act
        shutdown
        return r
    boot = do
        let eff = effect component

        liftMBR $ echo $ toAS $ "Loaded components:"
        mapM_ (liftMBR . echo . toAS) $ lines $ description component

        -- Execute boot actions
        mbx' (const $ return ()) $ ceffBoot $ effect component
    shutdown = do
        mbx' (const $ return ()) $ ceffShutdown $ effect component
    initMBState =
        mkUserState component ()
    initMBRState =
        MBRState
            { mbrTrigger = mconcat (map snd (M.toAscList (ceffTriggers (effect component))))
            , mbrBindings = ceffBindings (effect component)
            , mbrCommands = ceffCommands (effect component)
            , mbrStatus = ceffStatus (effect component)
            , mbrExtra = extraState
            }
    {-
    -- REMOVED: Causes delays in status updates. The screen is responsible for displaying the current status.
    statusTrigger =
        permanent $ do
            x <- await
            s <- lift $ ceffStatus $ effect component
            lift $ setStatus s
            yield x
    -}

insertMonoid :: (Ord k, Monoid v) => k -> v -> M.Map k v -> M.Map k v
insertMonoid = M.insertWith (<>)

nilC :: (Monad m) => MBComponent m e () (Fix Nil)
nilC = statefulComponent (const (Fix Nil)) nilLens seEmpty

stateC :: (Monad m, Functor r, Functor l) => (forall a. r a) -> MBComponent m e (Fix l) (Fix (r :*: l))
stateC v = statefulComponent (compUserState v) decompLens seEmpty

bind :: (Monad m) => Key -> String -> MBX e u m () -> MBComponent m e u u
bind k d a = pureComponent $ seEmpty { ceffBindings = Just (menuItem k d (actionWithoutArg a)) }

bindArg :: (Monad m) => Key -> String -> (String -> MBX e u m ()) -> MBComponent m e u u
bindArg k d a = pureComponent $ seEmpty { ceffBindings = Just (menuItem k d (actionWithArg a)) }

submenu :: (Monad m) => Key -> MBComponent m e u u -> MBComponent m e u u
submenu k c = c
    { effect = (effect c)
        { ceffBindings = fmap (mkSubmenu k) (ceffBindings (effect c))
        , ceffDescription = case ceffDescription (effect c) of
            "" -> "submenu"
            d  -> "submenu (" ++ show k ++ ") of\n" ++ unlines (map ("  " ++) (lines d))
        }
    }

triggerC :: (Monad m) => Priority -> Trigger (Ev e) (MB u m) () -> MBComponent m e u u
triggerC p t = pureComponent $ seEmpty { ceffTriggers = M.singleton p t }

bootC :: (Monad m) => MBX e u m () -> MBComponent m e u u
bootC a = pureComponent $ seEmpty { ceffBoot = a }

shutdownC :: (Monad m) => MBX e u m () -> MBComponent m e u u
shutdownC a = pureComponent $ seEmpty { ceffShutdown = a }

commandC :: (Monad m) => String -> String -> String -> CommandM (MBX e u m) () -> MBComponent m e u u
commandC name usage description cmd =
    let cmd' = mkCommand (name ++ " " ++ usage ++ "\n" ++ description) cmd
    in pureComponent $ seEmpty { ceffCommands = M.singleton name cmd' }

statusC :: (Monad m) => MB u m String -> MBComponent m e u u
statusC s = pureComponent $ seEmpty { ceffStatus = s }

describe :: String -> MBComponent m e u1 u2 -> MBComponent m e u1 u2
describe d c = c
    { effect = (effect c)
        { ceffDescription = case ceffDescription (effect c) of
            "" -> d
            d' -> d ++ "\n" ++ unlines (map ("  " ++) (lines d'))
        , ceffBindings = fmap (describeMenu d) (ceffBindings (effect c))
        }
    }

description :: MBComponent m e u1 u2 -> String
description c = ceffDescription $ effect c
