{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Mudblood.Contrib.MG.Mapper.MGMap where

import Control.Applicative
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (catMaybes, fromMaybe)

import Mudblood.Mapper
import Data.Cache
import Data.Undo
import Data.QuasiEq

{-
type UndoableMap = Undo Map
type IndexedMap = Cache UndoableMap (String -> Maybe Int)
type PortalMap = Cache IndexedMap Map
type OverlayMap = Cache PortalMap Map
type MapStore = QuasiEq OverlayMap
-}

justIfNotDefault :: (Eq b, ToJSON b) => Key -> a -> Lens' a b -> a -> Maybe Pair
justIfNotDefault key def l x = if x ^. l == def ^. l then Nothing else Just $ key .= toJSON (x ^. l)

data MGRoomData = MGRoomData
  { _mgRoomHash :: [String]
  , _mgRoomDomain :: Maybe String
  , _mgRoomShort :: Maybe String
  , _mgRoomSafe :: Bool
  } deriving (Eq, Show)

initMGRoomData :: MGRoomData
initMGRoomData = MGRoomData
  { _mgRoomHash = []
  , _mgRoomDomain = Nothing
  , _mgRoomShort = Nothing
  , _mgRoomSafe = True
  }

makeLenses ''MGRoomData

instance FromJSON MGRoomData where
  parseJSON (Object v) = MGRoomData
    <$> fmap (fromMaybe (initMGRoomData ^. mgRoomHash)) (v .:? "hash")
    <*> v .:? "domain"
    <*> v .:? "short"
    <*> fmap (fromMaybe (initMGRoomData ^. mgRoomSafe  )) (v .:? "safe")
  parseJSON x = prependFailure "MGRoomData" (typeMismatch "Object or Null" x)

instance ToJSON MGRoomData where
  toJSON x = object $ catMaybes
    [ justIfNotDefault "hash" initMGRoomData mgRoomHash x
    , justIfNotDefault "domain" initMGRoomData mgRoomDomain x
    , justIfNotDefault "short" initMGRoomData mgRoomShort x
    , justIfNotDefault "safe" initMGRoomData mgRoomSafe x
    ]
    -- [ "hash" .= toJSON (x ^. mgRoomHash)
    -- , "domain" .= toJSON (x ^. mgRoomDomain)
    -- , "short" .= toJSON (x ^. mgRoomShort)
    -- , "safe" .= toJSON (x ^. mgRoomSafe)
    -- ]

data MGExitData = MGExitData
  { _mgExitBlockers :: [String]
  , _mgExitBeforeExit :: [String]
  , _mgExitWeight :: Int
  } deriving (Eq, Show)

initMGExitData :: MGExitData
initMGExitData = MGExitData
  { _mgExitBlockers = []
  , _mgExitBeforeExit = []
  , _mgExitWeight = 1
  }

makeLenses ''MGExitData

instance FromJSON MGExitData where
  parseJSON (Object v) = MGExitData
    <$> fmap (fromMaybe (initMGExitData ^. mgExitBlockers  )) (v .:? "blockers")
    <*> fmap (fromMaybe (initMGExitData ^. mgExitBeforeExit)) (v .:? "beforeExit")
    <*> fmap (fromMaybe (initMGExitData ^. mgExitWeight    )) (v .:? "weight")
  parseJSON x = prependFailure "MGExitData" (typeMismatch "Object or Null" x)

instance ToJSON MGExitData where
  toJSON x = object $ catMaybes
    [ justIfNotDefault "blockers" initMGExitData mgExitBlockers x
    , justIfNotDefault "beforeExit" initMGExitData mgExitBeforeExit x
    , justIfNotDefault "weight" initMGExitData mgExitWeight x
    ]
    -- [ "blockers" .= toJSON (x ^. mgExitBlockers)
    -- , "beforeExit" .= toJSON (x ^. mgExitBeforeExit)
    -- , "weight" .= toJSON (x ^. mgExitWeight)
    -- ]

type MGMap = Map MGRoomData MGExitData
