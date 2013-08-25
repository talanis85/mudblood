{-# LANGUAGE DeriveDataTypeable #-}
module Mudblood.Contrib.MG.State 
    ( MGState (..)
    , MGStats (..)
    , modifyStats
    , modifyTanjianStats, modifyZaubererStats
    , newMGState, modifyState, getState
    , module Mudblood.Contrib.DynTrigger
    ) where

import Data.Typeable
import Data.Dynamic

import Mudblood
import Mudblood.Contrib.DynTrigger

import Mudblood.Contrib.MG.Gilden

data MGState = MGState
    { mgStats         :: MGStats
    , mgTanjianStats  :: MGTanjianStats
    , mgZaubererStats :: MGZaubererStats
    , mgDynTriggers   :: DynTriggerTable

    , mgFocus         :: Maybe String
    }
  deriving (Typeable)

data MGStats = MGStats
    { mgStatLP      :: Int
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

newMGState = MGState
    { mgStats = MGStats
        { mgStatLP      = 0
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
    , mgTanjianStats    = mkMGTanjianStats
    , mgZaubererStats   = mkMGZaubererStats
    , mgDynTriggers     = mkDynTriggerTable

    , mgFocus           = Nothing
    }

modifyState :: (MGState -> MGState) -> Trigger a b ()
modifyState f = modifyUserData f

getState :: Trigger a b MGState
getState = getUserData

modifyStats f = modifyState (\x -> x { mgStats = (f $ mgStats x) })
modifyTanjianStats f = modifyState (\x -> x { mgTanjianStats = (f $ mgTanjianStats x) })
modifyZaubererStats f = modifyState (\x -> x { mgZaubererStats = (f $ mgZaubererStats x) })
