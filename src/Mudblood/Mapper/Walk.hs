module Mudblood.Mapper.Walk
    ( walker
    , WalkerControl (..)
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

-- | Trigger to auto-walk from one room to another.
walker :: Map                                           -- ^ The map to use
       -> (TriggerEvent -> MBTrigger u WalkerControl)   -- ^ Trigger that decides when to continue or stop
       -> [String]                                      -- ^ The path to walk
       -> MBTrigger u [TriggerEvent]

walker _ _ [] = return []
walker tempmap f (first:path) = do
                                yieldT [SendTEvent first] >>= walker' f path
    where walker' f [] ev = return [ev]
          walker' f (s:rest) ev = do
            ret <- f ev
            case ret of
                WalkerStop -> return [ev]
                WalkerPause -> failT
                WalkerContinue -> do
                                  ev' <- yieldT [ev, SendTEvent s]
                                  walker' f rest ev'
