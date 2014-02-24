{-# LANGUAGE FlexibleContexts #-}

module Mudblood.Contrib.MG.GMCP
    ( triggerGmcpHello, triggerGmcpStat, triggerGmcpCommunication
    ) where

import Data.Has

import Data.Char
import Data.Maybe
import Mudblood
import Mudblood.Telnet
import Data.GMCP

import Mudblood.Contrib.MG.State

triggerGmcpHello :: TelnetNeg -> MBTrigger u ()
triggerGmcpHello t =
    case t of
        TelnetNeg (Just CMD_WILL) (Just OPT_GMCP) [] -> mapM_ send $ gmcpHello ["MG.char 1", "comm.channel 1", "MG.room 1"]
        _ -> failT

triggerGmcpStat :: (Has R_Common u) => GMCP -> MBTrigger u ()
triggerGmcpStat g =
    case gmcpModule g of
        "MG.char.base" ->
            let statfun =
                      (mgCharName     ??~ (getStringField "name" g))
                    . (mgCharRace     ??~ (getStringField "race" g))
                    . (mgCharPresay   ??~ (getStringField "presay" g))
                    . (mgCharTitle    ??~ (getStringField "title" g))
                    . (mgCharWizlevel ??~ (getIntField "wizlevel" g))
                    -- . (mgGuild        ??~ (getStringField "guild" g >>= readGuild))
            in modifyU R_Common statfun
        "MG.char.info" ->
            let statfun =
                      (mgCharLevel       ??~ (getIntField "level" g))
                    . (mgCharGuildLevel  ??~ (getIntField "guild_level" g))
                    . (mgCharGuildTitle  ??~ (getStringField "guild_title" g))
            in modifyU R_Common statfun
        "MG.char.maxvitals" ->
            let statfun =
                      (mgStatMLP    ??~ (getIntField "max_hp" g))
                    . (mgStatMKP    ??~ (getIntField "max_sp" g))
            in modifyU R_Common statfun
        "MG.char.vitals" ->
            let statfun =
                      (mgStatLP     ??~ (getIntField "hp" g))
                    . (mgStatKP     ??~ (getIntField "sp" g))
            in modifyU R_Common statfun
        _ -> failT

triggerGmcpCommunication :: GMCP -> MBTrigger u String
triggerGmcpCommunication g =
    case gmcpModule g of
        "comm.channel" -> return $ rstrip $ fromMaybe "" $ getStringField "msg" g
        _ -> failT
  where
    rstrip = reverse . dropWhile isSpace . reverse
