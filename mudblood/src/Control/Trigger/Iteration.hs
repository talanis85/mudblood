{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Control.Trigger.Iteration
  ( Iteration, Iteration'
  , liftI
  , pushback, flush
  , permanent, oneshot, oneshotIgnorePB
  , chainIteration
  , (<&>)
  , withState
  ) where

import Data.Semigroup
import Control.Monad.State
import Control.Monad.Morph

import Control.Trigger.Core

newtype Iteration' a b m r = Iteration { runIteration :: StateT [a] (T a b m) r }
  deriving (Functor, Applicative, Monad, MonadIO)

type Iteration a = Iteration' a a

instance MonadTrans (Iteration a) where
    lift m = Iteration $ lift $ lift m

instance MFunctor (Iteration a) where
  hoist f t = Iteration $ hoist (hoist f) (runIteration t)

liftI :: (Monad m) => T a b m r -> Iteration' a b m r
liftI t = Iteration $ lift t

pushback :: (Monad m) => a -> Iteration' a b m ()
pushback x = Iteration $ modify (++ [x])

flush :: (Monad m) => Iteration t m ()
flush = Iteration $ do
  pb <- get
  mapM_ (lift . yield) pb
  put []

instance (Monad m) => Triggering a b (Iteration' a b m) where
    yield x = Iteration $ lift $ yield x
    await = Iteration $ lift await
    -- feedback x = Iteration $ lift $ feedback x

instance (Monad m) => Semigroup (Iteration' a a m r) where
    a <> b = liftI $ oneshot a <> oneshot b

instance (Monad m, Monoid r) => Monoid (Iteration' a a m r) where
    mempty = return mempty
    mappend = (<>)

chainIteration a b = liftI $ oneshot a >-> oneshot b

permanent :: (Monad m) => Iteration a m r -> T a a m r'
permanent p = let x = oneshot p >> x in x

oneshot :: (Monad m) => Iteration a m r -> T a a m r
oneshot p = runStateT (runIteration p) [] >>= \(r, l) -> mapM_ yield l >> return r

oneshotIgnorePB :: (Monad m) => Iteration' a b m r -> T a b m r
oneshotIgnorePB p = runStateT (runIteration p) [] >>= return . fst

(<&>) :: (Monad m) => (r -> a) -> Iteration a m r -> T a a m ()
f <&> t = permanent $ t >>= yield . f
