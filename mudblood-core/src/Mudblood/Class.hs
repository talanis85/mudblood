{-# LANGUAGE FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Mudblood.Class
    ( MB (..)
    , Output (..)
    , Ticks
    , prompt, throw, bind, menu
    , echo, echoError, echoLog, echoInfo
    ) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import Control.UnsafeCallback

import Mudblood.Error (StackTrace, throwError, stackTrace)
import Mudblood.Telnet (Sendable)
import Mudblood.Text (AttrString)
import Mudblood.Keys

import Data.Menu

--------------------------------------------------------------------------------------------------

type Ticks = Int

data Output = OutputLine AttrString
            | OutputError StackTrace
            | OutputLog String
            | OutputInfo String
    deriving (Show)

-- | Provides I/O primitives. A 'Game' monad must be an instance of 'MB'
class (Functor m, Monad m, MonadError StackTrace m, Monad s) => MB s m | m -> s where
    -- | Lift an action from the underlying screen monad.
    liftScreen :: s a -> m a
    -- | Install a prompt with an appropriate callback.
    promptDyn :: String -> UnsafeCallback String -> m ()
    -- | Bind a key sequence to a dynamic callback.
    bindDyn :: [Key] -> UnsafeCallback () -> m ()
    -- | Popup a menu with dynamic callbacks.
    menuDyn :: Menu Key (UnsafeCallback ()) -> m ()
    -- | Connect to a server.
    connect :: String -> String -> m ()
    -- | Output an ''AttrString'' to the user.
    output :: Output -> m ()
    -- | Send something to the server.
    send :: (Sendable a) => a -> m ()
    -- | Set current prompt.
    setPrompt :: String -> m ()
    -- | Get the current time in ticks.
    time :: m Ticks

echo :: (MB s m) => AttrString -> m ()
echo s = output $ OutputLine s

echoError :: (MB s m) => StackTrace -> m ()
echoError s = output $ OutputError s

echoLog :: (MB s m) => String -> m ()
echoLog s = output $ OutputLog s

echoInfo :: (MB s m) => String -> m ()
echoInfo s = output $ OutputInfo s

-- | Prompt for user input. Takes a prompt string and a callback to handle the input.
prompt :: (MB s m) => String -> (String -> m ()) -> m ()
prompt s f = promptDyn s $ unsafeCallback f

-- | Bind a key sequence to an action.
bind :: (MB s m) => [Key] -> m () -> m ()
bind ks action = bindDyn ks $ unsafeCallback $ const action

menu :: (MB s m) => Menu Key (m ()) -> m ()
menu m = menuDyn $ second (unsafeCallback . const) m

-- | Throw an error. Errors should be caught in the screen.
throw :: (MB s m) => String -> String -> m a
throw subsys msg = throwError $ stackTrace subsys msg

--------------------------------------------------------------------------------------------------

-- MB instances for mtl transformers (needs 'UndecidableInstances')

instance (MB s m) => MB s (StateT x m) where
    liftScreen = lift . liftScreen
    promptDyn s f = lift $ promptDyn s f
    bindDyn ks f = lift $ bindDyn ks f
    menuDyn m = lift $ menuDyn m
    connect h p = lift $ connect h p
    output = lift . output
    send = lift . send
    setPrompt = lift . setPrompt
    time = lift $ time
