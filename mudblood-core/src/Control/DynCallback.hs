{-# LANGUAGE ScopedTypeVariables #-}
module Control.DynCallback
    ( DynCallback
    , dynCallback, runDynCallback

    , module Data.Typeable
    ) where

import Data.Dynamic
import Data.Typeable

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Error

-- | DynCallback is a function with one argument and a dynamic result type.
newtype DynCallback a = DynCallback Dynamic

-- | Wrap a function in a 'DynCallback'.
dynCallback :: (Typeable a, Typeable b) => (a -> b) -> DynCallback a
dynCallback f = DynCallback $ toDyn f

-- | Unwrap a 'DynCallback' or return 'Nothing' if the result type does not match.
runDynCallback :: (Typeable a, Typeable b) => DynCallback a -> Maybe (a -> b)
runDynCallback (DynCallback cb) = fromDynamic cb
