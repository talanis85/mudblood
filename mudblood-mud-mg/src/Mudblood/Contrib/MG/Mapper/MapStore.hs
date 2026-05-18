module Mudblood.Contrib.MG.Mapper.MapStore
  ( MapStore
  , mapStoreInit
  , mapStoreBase
  , mapStoreEffective
  , mapStorePortals
  , mapStoreOverlay
  ) where

import Control.Lens

import Mudblood.Mapper
import Mudblood.Contrib.MG.Mapper.Portals
import Mudblood.Contrib.MG.Mapper.MGMap

data MapStore = MapStore
  { _mapStoreBase :: MGMap
  , _mapStoreEffective :: MGMap
  , _mapStorePortals :: [Int]
  , _mapStoreOverlay :: [String]
  }

mapStoreInit :: MGRoomData -> [Int] -> [String] -> MapStore
mapStoreInit initRoom portals overlay = MapStore
  { _mapStoreBase = mapEmpty initRoom
  , _mapStoreEffective = makeEffectiveMap portals overlay (mapEmpty initRoom)
  , _mapStorePortals = portals
  , _mapStoreOverlay = overlay
  }

makeEffectiveMap :: [Int] -> [String] -> MGMap -> MGMap
makeEffectiveMap portals overlay m = mapAddPortals portals $ mapOverlay overlay m

mapStoreBase :: Lens' MapStore MGMap
mapStoreBase = lens get set
  where
    get mapStore = _mapStoreBase mapStore
    set mapStore m = mapStore
      { _mapStoreBase = m
      , _mapStoreEffective = makeEffectiveMap (_mapStorePortals mapStore) (_mapStoreOverlay mapStore) m
      }

mapStoreEffective :: Getter MapStore MGMap
mapStoreEffective = to _mapStoreEffective

mapStorePortals :: Lens' MapStore [Int]
mapStorePortals = lens get set
  where
    get mapStore = _mapStorePortals mapStore
    set mapStore x = mapStore
      { _mapStorePortals = x
      , _mapStoreEffective = makeEffectiveMap x (_mapStoreOverlay mapStore) (_mapStoreBase mapStore)
      }

mapStoreOverlay :: Lens' MapStore [String]
mapStoreOverlay = lens get set
  where
    get mapStore = _mapStoreOverlay mapStore
    set mapStore x = mapStore
      { _mapStoreOverlay = x
      , _mapStoreEffective = makeEffectiveMap (_mapStorePortals mapStore) x (_mapStoreBase mapStore)
      }
