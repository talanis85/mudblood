{-# LANGUAGE FlexibleContexts #-}

module Control.Trigger.Prelude
    (
      succeed, pass
    , passback
    , (>-@->), (>-%->)
    , (>!==>)
    , ftk
    , Injection, wrap, with
    , check, detect, detectFirst
    , stack, unstack

    ) where

import Control.Monad
import Control.Trigger.Monad

import Data.Bifunctor

-----------------------------------------------------------------------------

succeed :: (MonadTrigger a (Either a b) m) => b -> m ()
succeed = yield . Right

pass :: (MonadTrigger a (Either a b) m) => a -> m ()
pass = yield . Left

passback :: (Monad m) => (b -> d) -> Trigger a (Either b c) m r -> Trigger a (Either d c) m r
passback f t = mapYield (first f) t

infixl 1 >-@->, >-%->

(>-@->) :: (Monad m) => FailingEndoTrigger a m r -> FailingEndoTrigger a m r -> FailingEndoTrigger a m r
a >-@-> b = collate a >--?> b

(>-%->) :: (Monad m) => FailingEndoTrigger a m r -> FailingEndoTrigger a m r -> FailingEndoTrigger a m r
a >-%-> b = filterLeft a >--?> b

(>!==>) :: (Monad m) => FailingTrigger a b m () -> FailingEndoHandler b m () -> FailingTrigger a b m ()
a >!==> b = a >?==> (await >>= \x -> (mapYield (const undefined) $ mapAwait (const undefined) (b x)) >> yield x)

ftk :: (Monad m) => (a -> Fallible (EndoTrigger a m) ()) -> FailingEndoTrigger a m r
ftk = forever . try . keep

type Injection a b = ((a -> b), (b -> Maybe a))

wrap :: (Monad m) => Injection a b -> FailingEndoTrigger a m () -> FailingEndoTrigger b m ()
wrap (f, g) t = (forever $ await >>= \x -> maybe (pass x) succeed (g x)) >?=?> mapYield (either (Left . f) (Right . f)) t

with :: (Monad m) => Injection a b -> FailingTrigger a b m () -> FailingEndoTrigger b m ()
with (f, g) t = (forever $ await >>= \x -> maybe (pass x) succeed (g x)) >?=?> passback f t

stack :: (Monad m) => (a -> m b) -> a -> m (a, b)
stack f x = f x >>= (\y -> return (x, y))

unstack :: (Monad m) => (a -> m r) -> (a, b) -> m r
unstack f (a,b) = f a

check :: (MonadPlus m) => (a -> Bool) -> a -> m a
check f x = if f x then return x else mzero

detect :: (MonadPlus m) => (a -> Maybe b) -> a -> m b
detect f x = maybe mzero return (f x)

detectFirst :: (MonadPlus m) => [a -> Maybe b] -> a -> m b
detectFirst m x = msum $ map (\f -> detect f x) m
