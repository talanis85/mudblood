module Control.Trigger.Flow
    ( TriggerFlow (Permanent, Volatile, (:>>:), NoTrigger)
    , runTriggerFlow
    ) where

import Control.Trigger.Monad
import Control.Monad.Coroutine
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Monoid

-- A TriggerFlow is a flow chart of triggers. Triggers can either be Permanent or
-- Volatile. They can be combined in sequence or parallel.

data TriggerFlow m t = Permanent (t -> TriggerM m [t] t [t])
                     | Volatile (t -> TriggerM m [t] t [t])
                     | TriggerFlow m t :>>: TriggerFlow m t
                     | NoTrigger

infixr 9 :>>:

instance Show (TriggerFlow m t) where
    show (Permanent t) = "Permanent"
    show (Volatile t) = "Volatile"
    show (a :>>: b) = show a ++ " :>>: " ++ show b

instance Monoid (TriggerFlow m t) where
    mempty = NoTrigger
    NoTrigger `mappend` b = b
    a `mappend` NoTrigger = a
    a `mappend` b     = a :>>: b

runTriggerFlow :: (Applicative m, Monad m)
               => TriggerFlow m t
               -> t
               -> m ([t], TriggerFlow m t)

runTriggerFlow NoTrigger arg = return ([arg], NoTrigger)
runTriggerFlow (Permanent t) arg = run <$> runTriggerM (t arg)
    where run res = case res of
            Right v -> (v, Permanent t)
            Left (Yield v g) -> (v, Volatile g `mappend` Permanent t)
            Left (Replace v g) -> (v, Permanent g)
            Left Fail -> ([arg], Permanent t)

runTriggerFlow (Volatile t) arg = run <$> runTriggerM (t arg)
    where run res = case res of
            Right v -> (v, NoTrigger)
            Left (Yield v g) -> (v, Volatile g)
            Left (Replace v g) -> (v, Volatile g)
            Left Fail -> ([arg], Volatile t)

runTriggerFlow (f1 :>>: f2) arg = do
    (res1, t1) <- runTriggerFlow f1 arg
    (res2, t2) <- foldTriggerFlow f2 res1

    let t3 = t1 `mappend` t2

    return (mconcat res2, t3)

  where
    foldTriggerFlow tf [] = return ([], tf)
    foldTriggerFlow tf (x:xs) = do
        (r, t) <- runTriggerFlow tf x
        (rest, trest) <- foldTriggerFlow t xs
        return (r:rest, trest)
