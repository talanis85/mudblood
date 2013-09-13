module Mudblood.Mapper.Walk
    ( walker
    , WalkerControl (..)
    , moveTrigger
    ) where

import Data.List
import Data.Maybe

import qualified Data.Graph.Inductive as Gr

import Mudblood.Core
import Mudblood.Trigger
import Mudblood.Mapper.Map

data WalkerControl = WalkerStop
                   | WalkerPause
                   | WalkerContinue

walker :: Map -> (TriggerEvent -> MBTrigger WalkerControl) -> [String] -> MBTriggerFlow
walker tempmap f path = Volatile $ walker' f path
    where walker' f [] ev = return [ev]
          walker' f (s:rest) ev = do
            ret <- f ev
            case ret of
                WalkerStop -> return [ev]
                WalkerPause -> failT
                WalkerContinue -> do
                                  send s
                                  modifyMap $ \m -> mapSetCurrent
                                                        (fromMaybe (mapCurrentId m) (findNextRoom s tempmap (mapCurrentId m)))
                                                        m
                                  ev' <- yield [ev]
                                  walker' f rest ev'

moveTrigger :: (String -> Map -> Map) -> TriggerEvent -> MBTrigger [TriggerEvent]
moveTrigger stepper = guardSend >=> \l -> do
                modifyMap (stepper l)
                returnSend l
