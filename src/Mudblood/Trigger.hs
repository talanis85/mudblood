{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}

module Mudblood.Trigger
    ( Trigger
    , (>>>), (|||)
    , TriggerF (Result, Yield, Fail, RunIO, Echo, Send, GetUserData, PutUserData, PutUI, GetMap, PutMap)
    , TriggerResult (TResult, TYield, TFail)
    , TriggerFlow (Permanent, Volatile, (:||:), (:>>:))
    , runTriggerFlow
    , Free (Pure, Free)
    , failT, yield, --echo, send, getUserData, putUserData, putUIString, runIO
    ) where

import Prelude hiding (fail)

import Control.Monad hiding (fail)
import Control.Monad.Free
import Data.Monoid
import Data.Dynamic (Dynamic)
import Data.Typeable

import Mudblood.UI
import Mudblood.Mapper.Map
import Mudblood.Telnet

-- A TriggerFlow is a flow chart of triggers. Triggers can either be Permanent or
-- Volatile. They can be combined in sequence or parallel.

data TriggerFlow t = Permanent (t -> Trigger t [t] [t])
                   | Volatile (t -> Trigger t [t] [t])
                   | TriggerFlow t :||: TriggerFlow t
                   | TriggerFlow t :>>: TriggerFlow t

infixr 9 :||:
infixr 9 :>>:

instance Show (TriggerFlow t) where
    show (Permanent t) = "Permanent"
    show (Volatile t) = "Volatile"
    show (a :>>: b) = (show a) ++ " :>>: " ++ (show b)
    show (a :||: b) = (show a) ++ " :||: " ++ (show b)

runTriggerFlow :: (Monad m) => (Trigger t [t] [t] -> m (TriggerResult t [t] [t]))
                            -> TriggerFlow t
                            -> t
                            -> m ([t], Maybe (TriggerFlow t))

runTriggerFlow f (Permanent t) arg = do
    res <- f (t arg)
    case res of
        TResult v -> return (v, Just (Permanent t))
        TYield v g -> return (v, Just (Volatile g :>>: Permanent t))
        TFail -> return ([arg], Just (Permanent t))

runTriggerFlow f (Volatile t) arg = do
    res <- f (t arg)
    case res of
        TResult v -> return (v, Nothing)
        TYield v g -> return (v, Just (Volatile g))
        TFail -> return ([arg], Just (Volatile t))

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


data TriggerResult i y o = TResult o
                         | TYield y (i -> Trigger i y o)
                         | TFail

-- Trigger functor

data TriggerF i y o = Result o
                    | Yield y (i -> o)
                    | Fail
                    | forall a. RunIO (IO a) (a -> o)
                    | Echo String o
                    | Send Communication o
                    | GetUserData (Dynamic -> o)
                    | PutUserData Dynamic o
                    | GetMap (Map -> o)
                    | PutMap Map o
                    | PutUI UIAction o

instance Functor (TriggerF i y) where
    fmap f (Result o) = Result $ f o
    fmap f (Yield o g) = Yield o $ f . g
    fmap f Fail = Fail
    fmap f (RunIO io g) = RunIO io $ f . g
    fmap f (Echo s x) = Echo s $ f x
    fmap f (Send s x) = Send s $ f x
    fmap f (GetUserData g) = GetUserData $ f . g
    fmap f (PutUserData d x) = PutUserData d $ f x
    fmap f (PutUI a x) = PutUI a $ f x
    fmap f (GetMap g) = GetMap $ f . g
    fmap f (PutMap d x) = PutMap d $ f x

-- Free monad of trigger functor

type Trigger i y = Free (TriggerF i y)

(>>>) :: (Monad m) => (a -> m [b]) -> (b -> m [c]) -> (a -> m [c])
a >>> b = a >=> mapM b >=> return . concat

(|||) :: (Monad m) => (a -> m [b]) -> (a -> m [b]) -> (a -> m [b])
a ||| b = \x -> do
    r1 <- a x
    r2 <- b x
    return $ r1 ++ r2

-- Trigger primitives

failT :: Trigger i y o
failT = liftF $ Fail

yield :: y -> Trigger i y i
yield x = liftF $ Yield x id
