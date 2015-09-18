{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Mudblood.Screen.Dummy
  ( DummyScreen
  , execDummyScreen
  ) where

import Mudblood
import Mudblood.Error
import Control.Monad
import Control.Monad.Identity

newtype DummyScreen a = DummyScreen (ExceptT StackTrace IO a)
  deriving (Monad, Applicative, Functor, MonadIO, MonadError StackTrace)

instance MB DummyScreen DummyScreen where
  liftScreen = id
  promptDyn p f = return ()
  bindDyn k v = return ()
  menuDyn m = return ()
  connect h p = return ()
  send s = return ()
  setPrompt p = return ()
  output o = return ()
  time = return 0

execDummyScreen :: DummyScreen a -> IO ()
execDummyScreen (DummyScreen s) = void $ runExceptT s
