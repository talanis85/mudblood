module Control.Trigger
    ( Trigger
    , (>>>), (|||)
    , TriggerF (Result, Yield, Fail, Action)
    , TriggerResult (TResult, TYield, TFail)
    , TriggerFlow (Permanent, Volatile, (:||:), (:>>:))
    , runTriggerFlow
    , Free (Pure, Free)
    , failT, yield
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Data.Monoid

-- A TriggerFlow is a flow chart of triggers. Triggers can either be Permanent or
-- Volatile. They can be combined in sequence or parallel.

data TriggerFlow f t = Permanent (t -> Trigger f t [t] [t])
                     | Volatile (t -> Trigger f t [t] [t])
                     | TriggerFlow f t :||: TriggerFlow f t
                     | TriggerFlow f t :>>: TriggerFlow f t

infixr 9 :||:
infixr 9 :>>:

instance Show (TriggerFlow f t) where
    show (Permanent t) = "Permanent"
    show (Volatile t) = "Volatile"
    show (a :>>: b) = (show a) ++ " :>>: " ++ (show b)
    show (a :||: b) = (show a) ++ " :||: " ++ (show b)

runTriggerFlow :: (Applicative m, Monad m, Functor f)
                            => (Trigger f t [t] [t] -> m (TriggerResult f t [t] [t]))
                            -> TriggerFlow f t
                            -> t
                            -> m ([t], Maybe (TriggerFlow f t))

runTriggerFlow f (Permanent t) arg = run <$> f (t arg)
    where run res = case res of
            TResult v -> (v, Just (Permanent t))
            TYield v g -> (v, Just (Volatile g :>>: Permanent t))
            TFail -> ([arg], Just (Permanent t))

runTriggerFlow f (Volatile t) arg = run <$> f (t arg)
    where run res = case res of
            TResult v -> (v, Nothing)
            TYield v g -> (v, Just (Volatile g))
            TFail -> ([arg], Just (Volatile t))

runTriggerFlow f (f1 :||: f2) arg = do
    (res1, t1) <- runTriggerFlow f f1 arg
    (res2, t2) <- runTriggerFlow f f2 arg

    let t3 = case (t1, t2) of
                (Nothing, Nothing) -> Nothing
                (Just a, Nothing) -> Just a
                (Nothing, Just b) -> Just b
                (Just a, Just b) -> Just (a :||: b)

    return (res2, t3)

runTriggerFlow f (f1 :>>: f2) arg = do
    (res1, t1) <- runTriggerFlow f f1 arg
    (res2, t2) <- foldTriggerFlow f f2 res1

    let t3 = case (t1, t2) of
                (Nothing, Nothing) -> Nothing
                (Just a, Nothing) -> Just a
                (Nothing, Just b) -> Just b
                (Just a, Just b) -> Just (a :>>: b)

    return (mconcat res2, t3)

  where
    foldTriggerFlow f tf [] = return ([], Just tf)
    foldTriggerFlow f tf (x:xs) = do
        (r, t) <- runTriggerFlow f tf x
        (rest, trest) <- case t of
            Just t' -> foldTriggerFlow f t' xs
            Nothing -> return ([], Nothing)
        return (r:rest, trest)


data TriggerResult f i y o = TResult o
                           | TYield y (i -> Trigger f i y o)
                           | TFail

-- Trigger functor

data TriggerF f i y o = Result o
                      | Yield y (i -> o)
                      | Fail
                      | Action (f o)

instance (Functor f) => Functor (TriggerF f i y) where
    fmap f (Result o) = Result $ f o
    fmap f (Yield o g) = Yield o $ f . g
    fmap f Fail = Fail
    fmap f (Action a) = Action (fmap f a)

-- Free monad of trigger functor

type Trigger f i y = Free (TriggerF f i y)

(>>>) :: (Monad m) => (a -> m [b]) -> (b -> m [c]) -> (a -> m [c])
a >>> b = a >=> mapM b >=> return . concat

(|||) :: (Applicative f) => (a -> f [b]) -> (a -> f [b]) -> (a -> f [b])
a ||| b = \x -> (++) <$> a x <*> b x

-- Trigger primitives

failT :: (Functor f) => Trigger f i y o
failT = liftF $ Fail

yield :: (Functor f) => y -> Trigger f i y i
yield x = liftF $ Yield x id
