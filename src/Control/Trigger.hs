module Control.Trigger
    ( module Control.Trigger.Monad
    , module Control.Trigger.Flow

    , lift, get, modify, put
    , statefulT
    ) where

import Control.Trigger.Monad
import Control.Trigger.Flow

import Control.Monad.State
import Control.Monad.Trans

-- | Evaluate a state transformed trigger. The StateT is looped infinitely.
statefulT :: (Monad m) => s -> (a -> StateT s (TriggerM m y a) y) -> (a -> TriggerM m y a o)
statefulT initialState trigger = \x -> evalStateT (loop x) initialState
    where loop = trigger >=> lift . yieldT >=> loop
