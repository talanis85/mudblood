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
          walker' f (step:rest) ev = do
            ret <- f ev
            case ret of
                WalkerStop -> return [ev]
                WalkerPause -> failT
                WalkerContinue -> do
                                  send step
                                  ev' <- yield [ev]
                                  walker' f rest ev'

moveTrigger :: MBTriggerFlow
moveTrigger = Permanent $ guardSendEvent >>> \l -> do
                map <- getMap
                let gr = mapGraph map
                    nodes = Gr.out gr (mapCurrent map)
                case find (\(_, out, label) -> exitKey label == l) nodes of
                    Nothing -> returnSend l
                    Just (_, out, _) -> putMap (map { mapCurrent = out }) >> returnSend l
