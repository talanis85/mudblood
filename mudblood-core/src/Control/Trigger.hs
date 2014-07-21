module Control.Trigger
    ( TriggerM
    , Trigger, trig
    , EndoTrigger
    , runEndoTrigger
    , (>:>), (<||>)
    , yield, flop
    , static, chain

    , singleton
    , forever
    ) where

import Control.Monad
import Control.Trigger.Monad
import Control.Trigger.Arrow
import Control.Trigger.Aux

import Control.Monad.State
import Control.Monad.Trans
