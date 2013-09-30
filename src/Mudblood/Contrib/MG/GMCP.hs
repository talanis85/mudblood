{-# LANGUAGE FlexibleContexts #-}

module Mudblood.Contrib.MG.GMCP
    ( triggerGmcpHello, triggerGmcpStat
    ) where

import Data.Has
import Control.Arrow

import Data.Maybe
import Mudblood
import Mudblood.Telnet
import Data.GMCP

import Mudblood.Contrib.MG.State

triggerGmcpHello :: MBTrigger u TelnetNeg ()
triggerGmcpHello = marr $ \t ->
    case t of
        TelnetNeg (Just CMD_WILL) (Just OPT_GMCP) [] -> mapM_ send $ gmcpHello ["MG.char 1", "comm.channel 1", "MG.room 1"]
        _ -> failT

triggerGmcpStat :: (Has R_Common u) => MBTrigger u GMCP ()
triggerGmcpStat = marr $ \g ->
    case gmcpModule g of
        "MG.char.base" ->
            let statfun = \s -> s
                    { mgCharName     = fromMaybe (mgCharName s) $ getStringField "name" g
                    , mgCharRace     = fromMaybe (mgCharRace s) $ getStringField "race" g
                    , mgCharPresay   = fromMaybe (mgCharPresay s) $ getStringField "presay" g
                    , mgCharTitle    = fromMaybe (mgCharTitle s) $ getStringField "title" g
                    , mgCharWizlevel = fromMaybe (mgCharWizlevel s) $ getIntField "wizlevel" g
                    , mgGuild        = fromMaybe (mgGuild s) $ getStringField "guild" g >>= readGuild
                    }
            in modifyU R_Common statfun
        "MG.char.info" ->
            let statfun = \s -> s
                    { mgCharLevel       = fromMaybe (mgCharLevel s) $ getIntField "level" g
                    , mgCharGuildLevel  = fromMaybe (mgCharGuildLevel s) $ getIntField "guild_level" g
                    , mgCharGuildTitle  = fromMaybe (mgCharGuildTitle s) $ getStringField "guild_title" g
                    }
            in modifyU R_Common statfun
        "MG.char.maxvitals" ->
            let statfun = \s -> s
                    { mgStatMLP = fromMaybe (mgStatMLP s) $ getIntField "max_hp" g
                    , mgStatMKP = fromMaybe (mgStatMKP s) $ getIntField "max_sp" g
                    }
            in modifyU R_Common statfun
        "MG.char.vitals" ->
            let statfun = \s -> s
                    { mgStatLP  = fromMaybe (mgStatLP s) $ getIntField "hp" g
                    , mgStatKP  = fromMaybe (mgStatKP s) $ getIntField "sp" g
                    }
            in modifyU R_Common statfun
        _ -> return ()
