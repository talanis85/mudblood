{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Control.Trigger.Forking
  ( Forking (..), fork_
  ) where

import Data.Bifunctor

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Morph

import Control.Applicative

data Forking m a = Forking { runForking :: m (Either (Forking m (), Forking m a) a) }

fork_ :: (Monad m) => Forking m () -> Forking m ()
fork_ f = Forking $ return $ Left (f, return ())

instance (Monad m) => Functor (Forking m) where
  fmap f (Forking m) = Forking $ liftM (bimap (fmap (fmap f)) f) m

instance (Monad m) => Applicative (Forking m) where
  pure = Forking . return . Right
  f <*> x = Forking $ do
    r <- runForking f
    case r of
      Left (g, c) -> return $ Left (g, c <*> x)
      Right r -> runForking $ fmap (r $) x

instance (Monad m) => Monad (Forking m) where
  return = Forking . return . Right
  m >>= f = Forking $ do
    r <- runForking m
    case r of
      Left (g, c) -> return $ Left (g, c >>= f)
      Right r -> runForking $ f r

instance MonadTrans Forking where
  lift x = Forking $ liftM Right x

instance (MonadIO m) => MonadIO (Forking m) where
  liftIO = lift . liftIO

instance (MonadFree f m, Functor f) => MonadFree f (Forking m) where
  wrap = Forking . wrap . fmap runForking

instance MFunctor Forking where
  hoist h m = Forking $ h (liftM (bimap (bimap (hoist h) (hoist h)) id) (runForking m))
