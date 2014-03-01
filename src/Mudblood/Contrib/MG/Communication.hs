module Mudblood.Contrib.MG.Communication
    ( triggerCommunication
    ) where

import Control.Monad.Trans
import Control.Monad.State

import Mudblood

import Mudblood.Contrib.Regex
import Mudblood.Contrib.MG.GMCP

triggerCommunication :: (AttrString -> MBTrigger u [TriggerEvent]) -> TriggerEvent -> MBTrigger u [TriggerEvent]
triggerCommunication f =
    (guardGMCP >=> triggerGmcpCommunication >=> f . toAS)
    <||>
    (loopT (guardLine >=> (keep1 $ regexAS "^\\[[^]]+:[^]]+]") >=> f)
           (guardLine >=> (keep1 $ regexAS "^ ") >=> f))
    <||>
    (loopT (guardLine >=> (keep1 $ regexAS "^.+ teilt Dir mit:") >=> f)
           (guardLine >=> (keep1 $ regexAS "^ ") >=> f))
