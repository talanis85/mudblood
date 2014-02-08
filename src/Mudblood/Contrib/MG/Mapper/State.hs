{-# LANGUAGE TypeFamilies #-}

module Mudblood.Contrib.MG.Mapper.State
    ( R_Mapper (R_Mapper), MGMapperState (..)
    , MGMapperMode (..)
    , mkMGMapperState
    -- * State lenses
    , mapperRoomHash, mapperLastRoom, mapperOverlay, mapperWalkState
    , mapperLastInput, mapperMode
    ) where

import Control.Lens
import Data.Has

import Mudblood

data R_Mapper = R_Mapper
type instance TypeOf R_Mapper = MGMapperState

data MGMapperState = MGMapperState
    { _mapperRoomHash    :: String
    , _mapperLastRoom    :: Int
    , _mapperOverlay     :: [String]
    , _mapperWalkState   :: Int
    , _mapperLastInput   :: String
    , _mapperMode        :: MGMapperMode
    }

data MGMapperMode = ModeOff | ModeFixed | ModeAuto | ModeManual | ModeUpdate
    deriving (Eq, Show)

mapperRoomHash  :: Lens' MGMapperState String
mapperRoomHash  = lens _mapperRoomHash (\s v -> s { _mapperRoomHash = v })

mapperLastRoom  :: Lens' MGMapperState Int
mapperLastRoom  = lens _mapperLastRoom (\s v -> s { _mapperLastRoom = v })

mapperOverlay   :: Lens' MGMapperState [String]
mapperOverlay   = lens _mapperOverlay (\s v -> s { _mapperOverlay = v })

mapperWalkState :: Lens' MGMapperState Int
mapperWalkState = lens _mapperWalkState (\s v -> s { _mapperWalkState = v })

mapperLastInput :: Lens' MGMapperState String
mapperLastInput = lens _mapperLastInput (\s v -> s { _mapperLastInput = v })

mapperMode      :: Lens' MGMapperState MGMapperMode
mapperMode      = lens _mapperMode (\s v -> s { _mapperMode = v })

mkMGMapperState = MGMapperState
    { _mapperRoomHash      = ""
    , _mapperLastRoom      = 0
    , _mapperOverlay       = ["base"]
    , _mapperWalkState     = 0
    , _mapperLastInput     = ""
    , _mapperMode          = ModeFixed
    }
