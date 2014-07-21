{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Trigger.Monad
    ( TriggerM, TriggerR, TriggerF (..)
    , yield, flop, chain, (<||>)
    , runTriggerM
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Coroutine

import Data.Monoid
import Control.Applicative

-- | Monad transformer for trigger functions.
newtype TriggerM i o m r = TriggerM (Coroutine (TriggerF i o) m r)
    deriving (Monad, Functor)

-- | Continuation functor for TriggerM
data TriggerF i o r = Yield o (i -> r)
                    | Flop

-- | Result type for runTriggerM
type TriggerR i o m r = Either (TriggerF i o (TriggerM i o m r)) r

instance Functor (TriggerF x f) where
    fmap f (Yield x g) = Yield x $ f . g
    fmap f Flop        = Flop

instance MonadTrans (TriggerM i o) where
    lift = TriggerM . lift

-- | Yield a value, suspend the trigger and wait for the next input.
yield :: (Monad m) => o -> TriggerM i o m i
yield x = TriggerM $ suspend $ Yield x return

-- | Signal failure of a trigger.
flop :: (Monad m) => TriggerM i o m r
flop = TriggerM $ suspend $ Flop

-- | Repeat one trigger ad infinitum.
chain :: (Monad m) => (i -> TriggerM i o m i) -> (i -> TriggerM i o m r2)
chain t = t >=> chain t

-- | Run the trigger monad.
runTriggerM :: (Monad m) => TriggerM i o m r -> m (TriggerR i o m r)
runTriggerM (TriggerM cr) = mmap (mapLeft (fmap TriggerM)) (resume cr)
    where mmap f x = x >>= return . f
          mapLeft f e = case e of
                Left v  -> Left $ f v
                Right v -> Right v

instance (Monad m) => MonadPlus (TriggerM i o m) where
    mzero = flop
    a `mplus` b = do
        r1 <- lift $ runTriggerM a
        case r1 of
            Right v          -> return v
            Left (Yield x g) -> yield x >>= g
            Left Flop        -> b

-- | Choice for Kleisli functionn. Try the first function; if it fails, try
--   the second.
(<||>) :: (MonadPlus m) => (a -> m r) -> (a -> m r) -> (a -> m r)
a <||> b = \x -> a x `mplus` b x
