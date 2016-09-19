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

instance Screen DummyScreen where
  outputS o    = return ()
  sendS s      = return ()
  setPromptS p = return ()
  connectS h p = return ()
  timeS        = return 0
  setStatusS s = return ()
  {-
  prompt p f  = return ()
  bind k a    = return ()
  menu m      = return ()
  -}

execDummyScreen :: DummyScreen a -> IO ()
execDummyScreen (DummyScreen s) = void $ runExceptT s
