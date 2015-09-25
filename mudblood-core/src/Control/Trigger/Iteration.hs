{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Control.Trigger.Iteration
  ( Iteration
  , liftIteration
  , pushback, flush
  , permanent, oneshot
  , (<&>)
  , withState
  ) where

import Control.Monad.State

import Control.Trigger.Core

newtype Iteration a m r = Iteration { runIteration :: StateT [a] (Trigger a m) r }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (Iteration a) where
    lift m = Iteration $ lift $ lift m

liftIteration :: (Monad m) => Trigger a m r -> Iteration a m r
liftIteration t = Iteration $ lift t

pushback :: (Monad m) => t -> Iteration t m ()
pushback x = Iteration $ modify (++ [x])

flush :: (Monad m) => Iteration t m ()
flush = Iteration $ do
  pb <- get
  mapM_ (lift . yield) pb
  put []

instance (Monad m) => Triggering a a (Iteration a m) where
    yield x = Iteration $ lift $ yield x
    await = Iteration $ lift await
    feedback x = Iteration $ lift $ feedback x

permanent :: (Monad m) => Iteration a m r -> T a a m r'
permanent p = let x = oneshot p >> x in x

oneshot :: (Monad m) => Iteration a m r -> T a a m r
oneshot p = runStateT (runIteration p) [] >>= \(r, l) -> mapM_ yield l >> return r

(<&>) :: (Monad m) => (r -> a) -> Iteration a m r -> T a a m ()
f <&> t = permanent $ t >>= yield . f
