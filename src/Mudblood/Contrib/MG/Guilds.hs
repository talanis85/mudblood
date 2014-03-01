{-# LANGUAGE FlexibleContexts #-}

module Mudblood.Contrib.MG.Guilds
    ( module Mudblood.Contrib.MG.Guilds.Common
    , module Mudblood.Contrib.MG.Guilds.Tanjian
    , module Mudblood.Contrib.MG.Guilds.Zauberer

    , defaultStatus
    , guardGuild
    ) where

import Data.Maybe
import Data.String.Utils

import Text.Printf

import Data.Has hiding ((^.))
import Control.Lens

import Mudblood

import Mudblood.Contrib.MG.State

import Mudblood.Contrib.MG.Guilds.Common
import Mudblood.Contrib.MG.Guilds.Tanjian
import Mudblood.Contrib.MG.Guilds.Zauberer

defaultStatus :: (Has R_Common u) => MB u String
defaultStatus = do
    stat <- getU R_Common
    let lp  = stat ^. mgStatLP
        mlp = stat ^. mgStatMLP
        kp  = stat ^. mgStatKP
        mkp = stat ^. mgStatMKP
    return $ printf "LP: %d (%d) | KP: %d (%d)" lp mlp kp mkp

guardGuild :: (Has R_Common u) => MGGuild -> a -> MBTrigger u a
guardGuild g = keep1 checkGuild
    where checkGuild _ = getU' R_Common mgGuild >>= guard . (== g)
