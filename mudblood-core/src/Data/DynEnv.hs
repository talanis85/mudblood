module Data.DynEnv
  ( MonadDynEnv (..)
  , lookupEnv, putEnv
  ) where

import Prelude hiding (lookup, insert)

import Data.Maybe
import Data.Default
import Data.Typeable
import Data.Dynamic
import qualified Data.Map as M

import Control.Monad

type DynEnv = M.Map String Dynamic

empty :: DynEnv
empty = M.empty

lookup :: (Typeable a) => String -> DynEnv -> Maybe a
lookup k e = M.lookup k e >>= fromDynamic

lookup' :: (Typeable a, Default a) => String -> DynEnv -> a
lookup' k = fromMaybe def . lookup k

put :: (Typeable a) => String -> a -> DynEnv -> DynEnv
put k v = M.insert k (toDyn v)

class (Monad m) => MonadDynEnv m where
  getEnv :: m DynEnv
  modifyEnv :: (DynEnv -> DynEnv) -> m ()

lookupEnv :: (MonadDynEnv m, Typeable a, Default a) => String -> m a
lookupEnv k = liftM (lookup' k) getEnv

putEnv :: (MonadDynEnv m, Typeable a) => String -> a -> m ()
putEnv k v = modifyEnv (put k v)
