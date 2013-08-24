{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Mudblood.Mapper.Map
    ( Map (..)
    , RoomData (..)
    , ExitData (..)
    , mapEmpty
    ) where

import Text.JSON
import Text.JSON.Types

import qualified Data.Map as M

import Data.Graph.Inductive
import Data.Graph.Inductive.Tree

data RoomData = RoomData
    { roomUserData :: M.Map String JSValue
    }
  deriving (Show)

newtype JSRoom = JSRoom { getJSRoom :: LNode RoomData }

data ExitData = ExitData
    { exitLayer :: String
    , exitKey :: String
    , exitUserData :: M.Map String JSValue
    }
  deriving (Show)

newtype JSExit = JSExit { getJSExit :: LEdge ExitData }

newtype JSUserData = JSUserData { getJSUserData :: M.Map String JSValue }

data Map = Map
    { mapGraph :: Gr RoomData ExitData
    , mapCurrent :: Node
    }
  deriving (Show)

mapEmpty :: Map
mapEmpty = Map 
    { mapGraph = mkGraph [] []
    , mapCurrent = 0
    }

instance JSON Map where
    readJSON (JSObject o) = do
        rooms <- valFromObj "rooms" o >>= return . map getJSRoom
        exits <- valFromObj "exits" o >>= return . map getJSExit
        --virtual <- valFromObj "virtual" o
        return $ Map
            { mapGraph   = mkGraph rooms exits
            , mapCurrent = 0
            }

    readJSON _ = fail "Expected object"

    showJSON m = showJSON $ toJSObject [ ("rooms", showJSON $ map JSRoom (labNodes $ mapGraph m))
                                       , ("exits", showJSON $ map JSExit (labEdges $ mapGraph m))
                                       ]

instance JSON JSRoom where
    readJSON (JSObject o) = do
        userdata <- valFromObj "data" o >>= return . getJSUserData
        id <- valFromObj "id" o

        let roomData = RoomData
                { roomUserData = userdata
                }

        return $ JSRoom (id, roomData)

    readJSON _ = fail "Expected object"

    showJSON r = showJSON $ toJSObject [ ("id", showJSON $ fst (getJSRoom r))
                                       , ("userdata", showJSON $ roomUserData $ snd $ getJSRoom r)
                                       ]

instance JSON JSExit where
    readJSON (JSObject o) = do
        src      <- valFromObj "src" o
        dest     <- valFromObj "dest" o
        layer    <- valFromObj "layer" o
        userdata <- valFromObj "data" o >>= return . getJSUserData
        key      <- valFromObj "key" o

        let exitData = ExitData
                { exitLayer    = layer
                , exitKey      = key
                , exitUserData = userdata
                }

        return $ JSExit (src, dest, exitData)

    readJSON _ = fail "Expected object"

    showJSON e = let (src, dest, d) = getJSExit e
                 in showJSON $ toJSObject [ ("src", showJSON src)
                                          , ("dest", showJSON dest)
                                          , ("layer", showJSON $ exitLayer d)
                                          , ("userdata", showJSON $ exitUserData d)
                                          , ("key", showJSON $ exitKey d)
                                          ]

instance JSON JSUserData where
    readJSON (JSObject o) = return $ JSUserData $ M.fromList $ fromJSObject o

    readJSON _ = fail "Expected object"

    showJSON d = showJSON $ toJSObject $ M.toList (getJSUserData d)
