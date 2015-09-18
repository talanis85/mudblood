{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

-- | We roll our own ExceptT here to avoid the version bump to mtl-2.2.*
--   For documentation about this module, see the docs of transformers-0.4.*

module Control.Monad.Except
  ( ExceptT, runExceptT
  , exceptT
  , fmapL
  , hoistEither
  , hoistMaybe
  , hoistJust
  , ignoreError
  , stackError

  , MonadError (..), Error (..)
  , throwError, catchError
  ) where

import Data.Monoid
import Data.Bifunctor

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance (Monad m) => Functor (ExceptT e m) where
  fmap f = ExceptT . liftM (fmap f) . runExceptT

instance (Monad m) => Applicative (ExceptT e m) where
  pure = ExceptT . return . Right
  f <*> x = ExceptT $ liftM2 (<*>) (runExceptT f) (runExceptT x)

instance (Monad m) => Monad (ExceptT e m) where
  return = pure
  m >>= f = ExceptT $ do
    x <- runExceptT m
    case x of
      Left x' -> return $ Left x'
      Right x' -> runExceptT $ f x'

instance MonadTrans (ExceptT e) where
  lift = ExceptT . liftM Right

instance (Monad m) => MonadError e (ExceptT e m) where
  throwError = ExceptT . return . Left
  catchError m f = ExceptT $ do
    x <- runExceptT m
    case x of
      Left x' -> runExceptT $ f x'
      Right x' -> return $ Right x'

hoistEither :: (MonadError e m) => Either e a -> m a
hoistEither = either throwError return

hoistMaybe :: (MonadError e m) => e -> Maybe a -> m a
hoistMaybe e = maybe (throwError e) return

hoistJust :: (MonadError e m) => Maybe e -> m ()
hoistJust = maybe (return ()) throwError

ignoreError :: (MonadError e m) => m a -> m ()
ignoreError m = catchError (m >> return ()) (const $ return ())

stackError :: (MonadError e m, Monoid e) => e -> m a -> m a
stackError e m = catchError m $ \e' -> throwError $ e <> e'

exceptT :: (Monad m) => m (Either e a) -> ExceptT e m a
exceptT = ExceptT

fmapL :: (Monad m) => (e -> e') -> ExceptT e m a -> ExceptT e' m a
fmapL f = ExceptT . liftM (first f) . runExceptT

mapExceptT :: (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
mapExceptT f = ExceptT . f . runExceptT

-- MTL instances

instance (MonadIO m) => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO

instance (MonadReader r m) => MonadReader r (ExceptT e m) where
  ask = lift ask
  local f = mapExceptT (local f)

instance (MonadWriter w m) => MonadWriter w (ExceptT e m) where
  tell = lift . tell
  listen = liftListen listen
  pass = liftPass pass

instance (MonadState s m) => MonadState s (ExceptT e m) where
  get = lift get
  put = lift . put

liftListen listen = mapExceptT $ \ m -> do
  (a, w) <- listen m
  return $! fmap (\ r -> (r, w)) a

liftPass pass = mapExceptT $ \ m -> pass $ do
  a <- m
  return $! case a of
    Left l -> (Left l, id)
    Right (r, f) -> (Right r, f)
