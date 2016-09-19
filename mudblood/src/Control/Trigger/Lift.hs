module Control.Trigger.Lift
  ( stateful
  ) where

import Control.Trigger.Core
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Monad.Morph

stateful :: (Monad m) => s -> T a a (StateT s m) () -> T a a m ()
stateful s t = evalStateT (distribute t) s
{-# INLINE stateful #-}
