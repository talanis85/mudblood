{-# LANGUAGE FlexibleContexts #-}

module Mudblood.Contrib.MG.Guilds
    ( module Mudblood.Contrib.MG.Guilds.Tanjian
    , module Mudblood.Contrib.MG.Guilds.Zauberer

    , spell, hands, unhands
    , defaultStatus
    ) where

import Data.Maybe
import Data.String.Utils

import Text.Printf

import Data.Has hiding ((^.))
import Control.Lens

import Mudblood

import Mudblood.Contrib.MG.State

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

spell :: (MBMonad m u, Has R_Common u) => String -> m ()
spell sp = do
    focus <- getU' R_Common mgFocus
    let final = replace "%f" (fromMaybe "" focus)  sp
    echoA $ (toAS "> ") <++> (setFg Yellow $ toAS final)
    send final

hands :: (MBMonad m u, Has R_Common u) => Int -> m ()
hands n = do
    shield <- getU' R_Common mgShieldName
    case n of
        1 -> send "steck waffe weg"
        2 -> send $ "zieh " ++ shield ++ " aus\nsteck waffe weg"
        _ -> return ()

unhands :: (MBMonad m u, Has R_Common u) => m ()
unhands = do
    shield <- getU' R_Common mgShieldName
    send "zueck waffe"
    send $ "trage " ++ shield
