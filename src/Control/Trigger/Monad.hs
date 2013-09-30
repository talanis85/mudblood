{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Trigger.Monad
    ( TriggerM, runTriggerM
    , MaybeRequest (Yield, Fail)
    , yieldT, failT, whileT
    , liftT
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Coroutine

newtype TriggerM m y r o = TriggerM (Coroutine (MaybeRequest y r) m o)
    deriving (Monad)

instance (Monad m) => MonadPlus (TriggerM m y r) where
    mzero = failT
    (TriggerM a) `mplus` (TriggerM b) = TriggerM $ do
        r1 <- lift $ resume a
        case r1 of
            Left Fail -> b
            Left (Yield x g) -> suspend $ Yield x g
            Right v -> return v

data MaybeRequest req resp x = Yield req (resp -> x)
                             | Fail

instance Functor (MaybeRequest x f) where
    fmap f (Yield x g) = Yield x (f . g)
    fmap f Fail = Fail

runTriggerM :: (Functor m) => TriggerM m y r o -> m (Either (MaybeRequest y r (TriggerM m y r o)) o)
runTriggerM (TriggerM cr) = fmap (mapLeft (fmap TriggerM)) (resume cr)
    where mapLeft f e = case e of
                Left v  -> Left $ f v
                Right v -> Right v

liftT :: (Monad m) => m a -> TriggerM m y r a
liftT = TriggerM . lift

yieldT :: (Monad m) => y -> TriggerM m y r r
yieldT a = TriggerM $ suspend $ Yield a return

failT :: (Monad m) => TriggerM m y r o
failT = TriggerM $ suspend $ Fail

whileT :: (Monad m) => (r -> TriggerM m y r o) -- ^ Guard trigger to execute before
                      -> (o -> Bool)            -- ^ The condition
                      -> (o -> TriggerM m y r y) -- ^ Trigger to run while condition = True
                      -> (o -> TriggerM m y r y) -- ^ Trigger to run when condition = False
                      -> (r -> TriggerM m y r y)
whileT guard condition combi end x = do
    x' <- guard x
    if condition x'
        then combi x' >>= yieldT >>= whileT guard condition combi end
        else end x' >>= return
