module Mudblood.Mapper.Walk
    ( walker
    , WalkerControl (..)
    ) where

import Data.List
import Data.Maybe

import qualified Data.Graph.Inductive as Gr

import Control.Arrow

import Mudblood.Core
import Mudblood.Class
import Mudblood.Trigger
import Mudblood.Mapper.Map

data WalkerControl = WalkerStop
                   | WalkerContinue

-- | Trigger to auto-walk from one room to another.
walker :: (MB scr m)
       => (TriggerEvent -> Fallible (EndoTrigger TriggerEvent m) WalkerControl) -- ^ Trigger that decides when to continue or stop
       -> [String]                                      -- ^ The path to walk
       -> FailingEndoTrigger TriggerEvent m ()

walker f [] = return ()
walker f (x:xs) = do
    succeed $ SendEvent x
    (ev, ret) <- try $ stack f
    case ret of
        WalkerStop -> pass ev
        WalkerContinue -> walker f xs
