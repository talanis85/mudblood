{-# LANGUAGE DeriveFunctor #-}

module Mudblood.Contrib.MG.Inventory
  ( R, component
  , stEquipment
  ) where

import Control.Lens

import Mudblood
import Mudblood.Contrib.MG.Event

------------------------------------------------------------------------------

data R a = R
    { _stEquipment :: [String]
    }
  deriving (Functor)

mkSt = R
    { _stEquipment = []
    }

makeLenses ''R

------------------------------------------------------------------------------

component :: (Functor l, Screen m, MGEvent e) => MBComponent m e (Fix l) (Fix (R :*: l))
component = describe "Mudblood.MG.Inventory" $ component'
  where
    component' = stateC mkSt
             >>> triggerC 10 (periodically' 5 queryEquipment)

queryEquipment = do
    yieldSend "ausruestung -k"
    equip <- parse fetchBlock
    lift $ rec . stEquipment .= map fromAS equip
