module Mudblood.Contrib.MG.Communication
    ( triggerCommunication
    ) where

import Control.Monad.Trans
import Control.Monad.State

import Mudblood

import Mudblood.Contrib.Regex
import Mudblood.Contrib.MG.GMCP

triggerCommunication :: TriggerEvent -> StateT Bool (MBTrigger u) String
triggerCommunication = (guardGMCP >=> lift . triggerGmcpCommunication) <||> otherComm
    where
        otherComm =
                (multiline "^\\[[^]]+:[^]]+]" "^ ")
           <||> (multiline "^.+ teilt Dir mit:" "^ ")

multiline init follow =
    guardLine >=> \x -> do
        st <- get
        if st
            then do
                modify $ const False
                lift $ regexAS follow x
                modify $ const True
                return $ fromAS x
            else do
                lift $ regexAS init x
                modify $ const True
                return $ fromAS x
