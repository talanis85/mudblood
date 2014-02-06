module Control.Trigger.Flow
    ( TriggerFlow (Permanent, Volatile, (:>>:))
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

infixr 9 :>>:

instance Show (TriggerFlow m t) where
    show (Permanent t) = "Permanent"
    show (Volatile t) = "Volatile"
    show (a :>>: b) = (show a) ++ " :>>: " ++ (show b)

runTriggerFlow :: (Applicative m, Monad m)
               => TriggerFlow m t
               -> t
               -> m ([t], Maybe (TriggerFlow m t))

runTriggerFlow (Permanent t) arg = run <$> runTriggerM (t arg)
    where run res = case res of
            Right v -> (v, Just (Permanent t))
            Left (Yield v g) -> (v, Just (Volatile g :>>: Permanent t))
            Left (Replace v g) -> (v, Just (Permanent g))
            Left Fail -> ([arg], Just (Permanent t))

runTriggerFlow (Volatile t) arg = run <$> runTriggerM (t arg)
    where run res = case res of
            Right v -> (v, Nothing)
            Left (Yield v g) -> (v, Just (Volatile g))
            Left (Replace v g) -> (v, Just (Volatile g))
            Left Fail -> ([arg], Just (Volatile t))

runTriggerFlow (f1 :>>: f2) arg = do
    (res1, t1) <- runTriggerFlow f1 arg
    (res2, t2) <- foldTriggerFlow f2 res1

    let t3 = case (t1, t2) of
                (Just a, Just b) -> Just (a :>>: b)
                (a, b)           -> a `mplus` b

    return (mconcat res2, t3)

  where
    foldTriggerFlow tf [] = return ([], Just tf)
    foldTriggerFlow tf (x:xs) = do
        (r, t) <- runTriggerFlow tf x
        (rest, trest) <- case t of
            Just t' -> foldTriggerFlow t' xs
            Nothing -> return ([], Nothing)
        return (r:rest, trest)
