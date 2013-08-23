module Mudblood.Contrib.DynTrigger
    ( DynTriggerTable, mkDynTriggerTable
    , addDynTrigger
    , runDynTriggers
    ) where

import Mudblood

type DynTrigger = TriggerEvent -> Trigger TriggerEvent [TriggerEvent] [TriggerEvent]
type DynTriggerTable = [(String, DynTrigger)]

mkDynTriggerTable = []

addDynTrigger :: DynTriggerTable -> String -> DynTrigger -> DynTriggerTable
addDynTrigger table key trig = (key, trig) : table

runDynTriggers :: DynTriggerTable -> DynTrigger
runDynTriggers [] ev = return [ev]
runDynTriggers ((key,t):ts) ev = (t ev) >> runDynTriggers ts ev
