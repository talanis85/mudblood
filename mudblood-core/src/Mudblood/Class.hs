{-# LANGUAGE FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Mudblood.Class
    ( MB (..)
    , Ticks
    , prompt, throw, bind, menu
    ) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import Control.DynCallback

import Mudblood.Error (StackTrace, throwError, stackTrace)
import Mudblood.Telnet (Sendable)
import Mudblood.Text (AttrString)
import Mudblood.Keys

import Data.Menu

--------------------------------------------------------------------------------------------------

type Ticks = Int

-- | Provides I/O primitives. A 'Game' monad must be an instance of 'MB'
class (Monad m, MonadError StackTrace m, Monad s, Typeable1 m) => MB s m | m -> s where
    -- | Lift an action from the underlying screen monad.
    liftScreen :: s a -> m a
    -- | Install a prompt with an appropriate callback.
    promptDyn :: String -> DynCallback String -> m ()
    -- | Bind a key sequence to a dynamic callback.
    bindDyn :: [Key] -> DynCallback () -> m ()
    -- | Popup a menu with dynamic callbacks.
    menuDyn :: Menu Key (DynCallback ()) -> m ()
    -- | Connect to a server.
    connect :: String -> String -> m ()
    -- | Output an ''AttrString'' to the user.
    echo :: AttrString -> m ()
    -- | Send something to the server.
    send :: (Sendable a) => a -> m ()
    -- | Get the current time in ticks.
    time :: m Ticks

-- | Prompt for user input. Takes a prompt string and a callback to handle the input.
prompt :: (MB s m) => String -> (String -> m ()) -> m ()
prompt s f = promptDyn s $ dynCallback f

-- | Bind a key sequence to an action.
bind :: (MB s m) => [Key] -> m () -> m ()
bind ks action = bindDyn ks $ dynCallback $ const action

menu :: (MB s m) => Menu Key (m ()) -> m ()
menu m = menuDyn $ second (dynCallback . const) m

-- | Throw an error. Errors should be caught in the screen.
throw :: (MB s m) => String -> String -> m a
throw subsys msg = throwError $ stackTrace subsys msg

--------------------------------------------------------------------------------------------------

-- MB instances for mtl transformers (needs 'UndecidableInstances')

instance (MB s m, Typeable x) => MB s (StateT x m) where
    liftScreen = lift . liftScreen
    promptDyn s f = lift $ promptDyn s f
    bindDyn ks f = lift $ bindDyn ks f
    menuDyn m = lift $ menuDyn m
    connect h p = lift $ connect h p
    echo = lift . echo
    send = lift . send
    time = lift $ time
