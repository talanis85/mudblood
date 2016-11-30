module Mudblood.Contrib.MG.Mapper.Types where

import Mudblood.Mapper
import Data.Cache
import Data.Undo
import Data.QuasiEq

type UndoableMap = Undo Map
type IndexedMap = Cache UndoableMap (String -> Maybe Int)
type PortalMap = Cache IndexedMap Map
type OverlayMap = Cache PortalMap Map
type MapStore = QuasiEq OverlayMap
