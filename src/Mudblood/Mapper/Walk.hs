module Mudblood.Mapper.Walk
    ( walker
    , WalkerControl (..)
    ) where

import Mudblood.Core
import Mudblood.Trigger

data WalkerControl = WalkerStop
                   | WalkerPause
                   | WalkerContinue

walker :: (TriggerEvent -> MBTrigger WalkerControl) -> [String] -> MBTriggerFlow
walker f path = Volatile $ walker' f path
    where walker' f [] ev = return [ev]
          walker' f (step:rest) ev = do
            ret <- f ev
            case ret of
                WalkerStop -> return [ev]
                WalkerPause -> failT
                WalkerContinue -> do
                                  send step
                                  ev' <- yield [ev]
                                  walker' f rest ev'
