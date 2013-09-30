module Control.Trigger.Arrow
    ( Trigger
    --, yieldTA, failTA
    , pure, marr, discard
    ) where

import Control.Arrow
import Control.Trigger.Monad
import Control.Monad.Coroutine

type Trigger m y r = Kleisli (TriggerM m y r)

marr = Kleisli
pure f = marr $ \x -> return (f x)

{-
yieldTA :: (Monad m) => Trigger m y r y r
yieldTA = marr yieldTM

failTA :: (Monad m) => Trigger m y r i o
failTA = Kleisli $ const failTM
-}

discard a = (returnA &&& a) >>> (arr fst)
