{-# LANGUAGE FlexibleContexts, TypeOperators #-}

module Mudblood.Contrib.MG
    ( module Mudblood.Contrib.MG.State

    , module Mudblood.Contrib.MG.Combat

    , MGState, mkMGState
    ) where

import Data.Has (Has, FieldOf, fieldOf, (:&:), (&))
import Control.Lens hiding ((&))

import Mudblood
import Mudblood.Contrib.MG.State

import Mudblood.Contrib.MG.Combat

------------------------------------------------------------------------------

type MGState = FieldOf R_Common

mkMGState :: MGState
mkMGState = fieldOf mkMGCommonState
