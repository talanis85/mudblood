module Mudblood.Mapper.Walk
    ( walker
    , WalkerControl (..)
    , moveTrigger
    ) where

import Data.List
import Data.Maybe

import qualified Data.Graph.Inductive as Gr

import Control.Arrow

import Mudblood.Core
import Mudblood.Trigger
import Mudblood.Mapper.Map

data WalkerControl = WalkerStop
                   | WalkerPause
                   | WalkerContinue

walker :: Map -> MBTrigger u TriggerEvent WalkerControl -> [String] -> MBTriggerFlow u
walker tempmap f path = Volatile $ marr $ walker' f path
    where walker' f [] ev = return [ev]
          walker' f (s:rest) ev = do
            ret <- runKleisli f ev
            case ret of
                WalkerStop -> return [ev]
                WalkerPause -> failT
                WalkerContinue -> do
                                  liftT $ send s
                                  liftT $ modifyMap $ \m -> mapSetCurrent (fromMaybe (mapCurrentId m) (findNextRoom s tempmap (mapCurrentId m))) m
                                  ev' <- yieldT [ev]
                                  walker' f rest ev'

moveTrigger :: (String -> Map -> Map) -> MBTrigger u TriggerEvent [TriggerEvent]
moveTrigger stepper = marr $ guardSend >=> \l -> do
                liftT $ modifyMap (stepper l)
                returnSend l
