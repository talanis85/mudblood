{-# LANGUAGE Arrows #-}

module Control.Trigger.Example where

import Control.Trigger.Monad
import Control.Trigger.Arrow
import Control.Trigger.Flow
import Control.Arrow
import Control.Monad.Free
import Control.Monad.Trans
import Control.Monad.Coroutine

data DummyF a = DummyF a

instance Functor DummyF where
    fmap f (DummyF x) = DummyF (f x)

--liftT = lift . liftF

dummyA :: Trigger (Free DummyF) y r i ()
dummyA = marr $ const $ lift $ liftF (DummyF ())

interp :: Free DummyF a -> IO a
interp (Pure r) = return r
interp (Free (DummyF x)) = putStrLn "DummyF" >> interp x

test :: TriggerM (Free DummyF) String String String
test = do
    --liftT $ DummyF ()
    ret <- yieldT "Hallo"
    return "Welt"

toList :: (Monad m) => Trigger m y r a [a]
toList = pure (: [])

test2 :: Trigger (Free DummyF) [String] String String [String]
--test2 = (discard dummyA) >>> toList >>> yieldA >>> toList -- >>> yieldA
--test2 = proc x -> do
--    dummyA -< x
--    ret <- yieldA <<< toList -< x
--    returnA <<< toList -< ret
test2 = marr $ \x -> do
    lift $ liftF (DummyF ())
    ret <- yieldT [x]
    return [x]

exflow = Permanent test2
