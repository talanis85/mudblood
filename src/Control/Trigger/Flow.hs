module Control.Trigger.Flow
    ( Trigger
    , TriggerFlow (Permanent, Volatile, (:||:), (:>>:))
    , runTriggerFlow
    -- * Combinators
    --, whileT
    ) where

import Control.Trigger.Arrow
import Control.Trigger.Monad
import Control.Monad.Coroutine
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Monoid

-- A TriggerFlow is a flow chart of triggers. Triggers can either be Permanent or
-- Volatile. They can be combined in sequence or parallel.

data TriggerFlow m t = Permanent (Trigger m [t] t t [t])
                     | Volatile (Trigger m [t] t t [t])
                     | TriggerFlow m t :||: TriggerFlow m t
                     | TriggerFlow m t :>>: TriggerFlow m t

infixr 9 :||:
infixr 9 :>>:

instance Show (TriggerFlow m t) where
    show (Permanent t) = "Permanent"
    show (Volatile t) = "Volatile"
    show (a :>>: b) = (show a) ++ " :>>: " ++ (show b)
    show (a :||: b) = (show a) ++ " :||: " ++ (show b)

runTriggerFlow :: (Applicative m, Monad m)
               => TriggerFlow m t
               -> t
               -> m ([t], Maybe (TriggerFlow m t))

runTriggerFlow (Permanent t) arg = run <$> runTriggerM (runKleisli t arg)
    where run res = case res of
            Right v -> (v, Just (Permanent t))
            Left (Yield v g) -> (v, Just (Volatile (marr g) :>>: Permanent t))
            Left Fail -> ([arg], Just (Permanent t))

runTriggerFlow (Volatile t) arg = run <$> runTriggerM (runKleisli t arg)
    where run res = case res of
            Right v -> (v, Nothing)
            Left (Yield v g) -> (v, Just (Volatile $ marr g))
            Left Fail -> ([arg], Just (Volatile t))

runTriggerFlow (f1 :||: f2) arg = do
    (res1, t1) <- runTriggerFlow f1 arg
    (res2, t2) <- runTriggerFlow f2 arg

    let t3 = case (t1, t2) of
                (Just a, Just b) -> Just (a :||: b)
                (a, b)           -> a `mplus` b

    return (res2, t3)

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

{-
(>>>) :: (Monad m) => (a -> m [b]) -> (b -> m [c]) -> (a -> m [c])
a >>> b = a >=> mapM b >=> return . concat

(|||) :: (Applicative f) => (a -> f [b]) -> (a -> f [b]) -> (a -> f [b])
a ||| b = \x -> (++) <$> a x <*> b x
-}

-- Trigger primitives

{-
-- | Run a trigger as long as a condition is true.
whileT :: (Functor f) => (i -> Trigger f i y o) -- ^ Guard trigger to execute before
                      -> (o -> Bool)            -- ^ The condition
                      -> (o -> Trigger f i y y) -- ^ Trigger to run while condition = True
                      -> (o -> Trigger f i y y) -- ^ Trigger to run when condition = False
                      -> (i -> Trigger f i y y)
whileT guard condition combi end x = do
    x' <- guard x
    if condition x'
        then combi x' >>= yield >>= whileT guard condition combi end
        else end x' >>= return
-}

{-
    multiple :: (Functor f)
         => String
         -> (AttrString -> MBTrigger [TriggerEvent])
         -> String
         -> (AttrString -> MBTrigger [TriggerEvent])
         -> (i -> Trigger f i y o)
triggerRegexMultiline start startt next nextt = guardLine >=> \l -> do
    guardT $ l =~ start
    l' <- startt l >>= yield >>= waitForLine
    follow l'
  where
    follow l = do
        if l =~ "^ " then do
                          l' <- nextt l >>= yield >>= waitForLine
                          follow l'
                     else returnLine l

whileT :: (Functor f) => -> (i -> Trigger f i y o)
-}
