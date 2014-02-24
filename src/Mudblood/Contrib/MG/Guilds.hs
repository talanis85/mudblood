{-# LANGUAGE FlexibleContexts #-}

module Mudblood.Contrib.MG.Guilds
    ( module Mudblood.Contrib.MG.Guilds.Tanjian
    , module Mudblood.Contrib.MG.Guilds.Zauberer

    , spell, hands, unhands
    ) where

import Data.Maybe
import Data.String.Utils

import Mudblood

import Mudblood.Contrib.MG.State

import Mudblood.Contrib.MG.Guilds.Tanjian
import Mudblood.Contrib.MG.Guilds.Zauberer

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
