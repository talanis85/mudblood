{-# LANGUAGE FlexibleContexts #-}

module Mudblood.Contrib.MG.GMCP
    ( gmcpC
    , fetchGMCPChannel
    ) where

import Data.Char
import Data.Maybe
import Data.GMCP

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Lens
import Control.Trigger

import Mudblood
import Mudblood.Telnet hiding (TelnetEvent)

gmcpC :: (Screen m, TelnetEvent :<: e) => MBComponent m e u u
gmcpC = describe "Mudblood.MG.GMCP" $ triggerC 1 triggerGMCPHello

triggerGMCPHello = permanent $ parse' fetchGMCPHello >> lift sendGMCPHello

fetchGMCPHello = fetchTelnet >>= isGmcpHello
  where isGmcpHello x = case x of
            TelnetNeg (Just CMD_WILL) (Just OPT_GMCP) [] -> return ()
            _ -> mzero

isGmcpModule name g = if gmcpModule g == name then return g else mzero

sendGMCPHello = mapM_ send $ gmcpHello ["MG.char 1", "comm.channel 1", "MG.room 1"]

fetchGMCPChannel = fetch >>= guardGMCP >>= isGmcpModule "comm.channel" >>= return . parseChannelMessage . filter (/= '\n') . rstrip . fromMaybe "" . getStringField "msg"
  where
    rstrip = reverse . dropWhile isSpace . reverse
    parseChannelMessage s = case "^\\[([^]]+):([^]]+)] *(.*)$" ~~= s of
        Nothing -> ("", "", s)
        Just [_, a, b, c] -> (a, b, c)
