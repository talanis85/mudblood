module Mudblood.Contrib.MG.Communication
    ( triggerCommunication
    ) where

import Control.Monad.Trans
import Control.Monad.State

import Mudblood

import Mudblood.Contrib.Regex
import Mudblood.Contrib.MG.GMCP

loopT :: (a -> MBTrigger u [TriggerEvent])
      -> (TriggerEvent -> MBTrigger u [TriggerEvent])
      -> (a -> MBTrigger u [TriggerEvent])
loopT startt nextt = startt >=> yieldT >=> loop
    where
        loop x = ((nextt >=> yieldT >=> loop) x) `mplus` (return [x])

triggerCommunication :: (AttrString -> MBTrigger u [TriggerEvent]) -> TriggerEvent -> MBTrigger u [TriggerEvent]
triggerCommunication f =
    (guardGMCP >=> triggerGmcpCommunication >=> f . toAS)
    <||>
    (loopT (guardLine >=> (keep1 $ regexAS "^\\[[^]]+:[^]]+]") >=> f)
           (guardLine >=> (keep1 $ regexAS "^ ") >=> f))
    <||>
    (loopT (guardLine >=> (keep1 $ regexAS "^.+ teilt Dir mit:") >=> f)
           (guardLine >=> (keep1 $ regexAS "^ ") >=> f))
