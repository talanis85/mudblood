{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Trigger.Monad
    ( TriggerM, TriggerSuspension (..), TriggerR
    , yield, flop, check, chain, (<||>)
    , runTriggerM
    ) where

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Trans

data TriggerM a b m r =
    Pure r
  | M (m (TriggerM a b m r))
  | Suspend (TriggerSuspension a b m r)

data TriggerSuspension a b m r =
    Flop
  | Check (a -> TriggerM a b m r)
  | Yield b (a -> TriggerM a b m r)

instance (Monad m) => Functor (TriggerM a b m) where
    fmap f c = case c of
        Pure r               -> Pure $ f r
        M mc                 -> M $ liftM (fmap f) mc
        Suspend Flop         -> Suspend $ Flop
        Suspend (Check fc)   -> Suspend $ Check $ fmap (fmap f) fc
        Suspend (Yield x fc) -> Suspend $ Yield x $ fmap (fmap f) fc

instance (Monad m) => Monad (TriggerM a b m) where
    return = Pure
    m >>= f = case m of
        Pure r               -> f r
        M mc                 -> M $ liftM (>>= f) mc
        Suspend Flop         -> Suspend $ Flop
        Suspend (Check fc)   -> Suspend $ Check $ liftM (>>= f) fc
        Suspend (Yield x fc) -> Suspend $ Yield x $ liftM (>>= f) fc

type TriggerR a b m r = Either (TriggerSuspension a b m r) r

instance MonadTrans (TriggerM a b) where
    lift = M . liftM Pure

-- | Yield a value, suspend the trigger and wait for the next input.
yield :: (Monad m) => o -> TriggerM i o m i
yield x = Suspend $ Yield x (return . id)

-- | Signal failure of a trigger.
flop :: (Monad m) => TriggerM i o m r
flop = Suspend Flop

-- | Resume from here if the trigger flops.
check :: (Monad m) => TriggerM i o m i
check = Suspend $ Check (return . id)

-- | Repeat one trigger ad infinitum.
chain :: (Monad m) => (i -> TriggerM i o m i) -> (i -> TriggerM i o m r2)
chain t = t >=> chain t

-- | Run the trigger monad.
runTriggerM :: (Monad m) => TriggerM i o m r -> m (TriggerR i o m r)
runTriggerM t = case t of
    Pure r    -> return $ Right r
    M m       -> m >>= runTriggerM
    Suspend x -> return $ Left x

instance (Monad m) => MonadPlus (TriggerM i o m) where
    mzero = flop
    a `mplus` b = do
        r1 <- lift $ runTriggerM a
        case r1 of
            Right v          -> return v
            Left (Yield x g) -> yield x >>= g
            Left (Check g)   -> check >>= (\x -> g x `mplus` b)
            Left Flop        -> b

-- | Choice for Kleisli functionn. Try the first function; if it fails, try
--   the second.
(<||>) :: (MonadPlus m) => (a -> m r) -> (a -> m r) -> (a -> m r)
a <||> b = \x -> a x `mplus` b x
