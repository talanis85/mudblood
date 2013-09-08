module Mudblood.Mapper.Walk
    ( walker
    , WalkerControl (..)
    , moveTrigger
    ) where

import Data.List

import qualified Data.Graph.Inductive as Gr

import Mudblood.Core
import Mudblood.Trigger
import Mudblood.Mapper.Map
import Mudblood.User

data WalkerControl = WalkerStop
                   | WalkerPause
                   | WalkerContinue

walker :: (TriggerEvent -> MBTrigger WalkerControl) -> [String] -> MBTriggerFlow
walker f path = Volatile $ walker' f path
    where walker' f [] ev = return [ev]
          walker' f (s:rest) ev = do
            ret <- f ev
            case ret of
                WalkerStop -> return [ev]
                WalkerPause -> failT
                WalkerContinue -> do
                                  send s
                                  modifyMap $ step s
                                  ev' <- yield [ev]
                                  walker' f rest ev'

moveTrigger :: MBTriggerFlow
moveTrigger = Permanent $ guardSendEvent >>> \l -> do
                modifyMap $ step l
                returnSend l
