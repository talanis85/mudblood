{-# LANGUAGE FlexibleInstances #-}
module Control.Trigger.Arrow
    ( Trigger (..), trig
    , EndoTrigger, EndoTriggerR
    , runEndoTrigger
    , (>:>)
    , static
    ) where

import Prelude hiding ((.), id)

import Data.Monoid

import Control.Monad
import Control.Monad.Trans
import Control.Arrow
import Control.Category

import Control.Trigger.Monad
import Control.Trigger.Aux

newtype Trigger m a b = Trigger { unTrigger :: a -> TriggerM a b m () }

-- | Construct a trigger from a TriggerM function.
trig f = Trigger $ fmap void f

instance (Monad m) => Category (Trigger m) where
    id = Trigger $ let f = yield >=> f
                   in f
    b . a = Trigger $ \x -> do
        r1 <- lift $ runTriggerM $ (unTrigger a) x
        case r1 of
            Right () -> return ()
            Left (Yield x g) -> do
                r2 <- lift $ runTriggerM $ (unTrigger b) x
                case r2 of
                    Right () -> return ()
                    Left (Yield x g') -> yield x >>= (unTrigger $ Trigger g' . Trigger g)
                    Left (Check g') -> check >>= (unTrigger $ Trigger g' . Trigger g)
                    Left Flop -> flop
            Left (Check g) -> check >>= (unTrigger $ b . Trigger g)
            Left Flop -> flop

{-
instance (Monad m) => Arrow (Trigger m) where
    arr g = Trigger $ let f = yield . singleton . g >=> f
                      in f
    -- TODO
-}

-----------------------------------------------------------------------------

-- | An EndoTrigger is a special type of trigger with output type = [input type].
type EndoTrigger t m = Trigger m t [t]
type EndoTriggerR t m r = TriggerR t [t] m r

instance (Monad m) => Monoid (Trigger m t [t]) where
    mempty = Trigger $ let f = yield . singleton >=> f in f
    a `mappend` b = Trigger $ \x -> do
        (r1, t1) <- lift $ runEndoTrigger a x
        (r2, t2) <- lift $ foldM foldEndoTrigger ([], Just b) r1
        case t1 `mappend` t2 of
            Nothing -> yield r2 >> return ()
            Just t3 -> yield r2 >>= unTrigger t3

foldEndoTrigger :: (Monad m) => ([t], Maybe (EndoTrigger t m)) -> t -> m ([t], Maybe (EndoTrigger t m))
foldEndoTrigger (xs, t) x = do
    case t of
        Nothing -> return (xs, t)
        Just t -> do
            (r, t') <- runEndoTrigger t x
            return (xs ++ r, t')

-- | Run an EndoTrigger. Returns a tuple (result, remaining EndoTrigger)
runEndoTrigger :: (Monad m) => EndoTrigger t m -> t -> m ([t], Maybe (EndoTrigger t m))
runEndoTrigger t x = do
    r <- runTriggerM $ unTrigger t x
    case r of
        Right () -> return ([x], Nothing)
        Left (Yield x g) -> return (x, Just $ Trigger g)
        Left (Check g) -> runEndoTrigger (Trigger g) x
        Left Flop -> return ([x], Just t)

-- | Chain EndoTriggers. Result is an EndoTrigger that runs the left trigger first
--   and then feeds each result value to the second trigger.
(>:>) :: (Monad m) => EndoTrigger a m -> EndoTrigger a m -> EndoTrigger a m
(>:>) = mappend

-- | Make an EndoTrigger repeat itself forever.
static :: (Monad m) => EndoTrigger a m -> EndoTrigger a m
static t' = Trigger $ static' t'
    where static' t' = oneIteration t'    
          oneIteration t = \x -> do
            r <- lift $ runTriggerM $ unTrigger t x
            case r of
                Right () -> static' t' x
                Left (Yield x g) -> yield x >>= oneIteration (Trigger g)
                Left (Check g) -> check >>= g
                Left Flop -> flop
