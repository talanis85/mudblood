{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies,TypeOperators,FlexibleContexts #-}

module Mudblood.Contrib.MG.State
    ( R_Common (..), MGCommonState (..)
    , MGGuild (..)
    , mkMGCommonState
    , getU, setU, modifyU
    , modifyStateA
    , modifyCommonStateA
    , readGuild
    ) where

import Data.Has

import Mudblood

data R_Common = R_Common
type instance TypeOf R_Common = MGCommonState

data MGGuild = MGGuildTanjian | MGGuildZauberer | MGGuildAbenteurer
    deriving (Eq)

data MGCommonState = MGCommonState
    { mgGuild         :: MGGuild
    , mgFocus         :: Maybe String
    , mgProfile       :: String

    , mgCharName        :: String
    , mgCharRace        :: String
    , mgCharPresay      :: String
    , mgCharTitle       :: String
    , mgCharWizlevel    :: Int
    , mgCharLevel       :: Int
    , mgCharGuildLevel  :: Int
    , mgCharGuildTitle  :: String

    , mgStatLP      :: Int
    , mgStatMLP     :: Int
    , mgStatKP      :: Int
    , mgStatMKP     :: Int
    , mgStatVO      :: Int
    , mgStatFR      :: String
    , mgStatG       :: Int
    , mgStatB       :: Bool
    , mgStatT       :: Bool
    , mgStatF       :: Bool
    }

mkMGCommonState = MGCommonState
    { mgGuild           = MGGuildAbenteurer
    , mgFocus           = Nothing
    , mgProfile         = ""

    , mgCharName        = "Jemand"
    , mgCharRace        = "Etwas"
    , mgCharPresay      = ""
    , mgCharTitle       = ""
    , mgCharWizlevel    = 0
    , mgCharLevel       = 0
    , mgCharGuildLevel  = 0
    , mgCharGuildTitle  = ""

    , mgStatLP      = 0
    , mgStatMLP     = 0
    , mgStatKP      = 0
    , mgStatMKP     = 0
    , mgStatVO      = 0
    , mgStatFR      = ""
    , mgStatG       = 0
    , mgStatB       = False
    , mgStatT       = False
    , mgStatF       = False
    }

------------------------------------------------------------------------------

getU :: (MBMonad m u, Knows a (TypeOf a) u) => a -> m (TypeOf a)
getU a = getUserData >>= (\x -> return (a ^. x))

setU :: (MBMonad m u, Knows a (TypeOf a) u) => a -> (TypeOf a) -> m ()
setU a v = modifyUserData $ a ^= v

modifyU :: (MBMonad m u, Knows a (TypeOf a) u) => a -> (TypeOf a -> TypeOf a) -> m ()
modifyU a f = modifyUserData $ a ^: f

------------------------------------------------------------------------------

modifyStateA :: MBTrigger u (u -> u) ()
modifyStateA = marr modifyUserData

modifyCommonStateA :: (Has R_Common u) => MBTrigger u (MGCommonState -> MGCommonState) ()
modifyCommonStateA = marr $ modifyU R_Common

------------------------------------------------------------------------------


-- helpers
readGuild :: String -> Maybe MGGuild
readGuild "abenteurer"  = Just MGGuildAbenteurer
readGuild "tanjian"     = Just MGGuildTanjian
readGuild "zauberer"    = Just MGGuildZauberer
readGuild _             = Nothing

