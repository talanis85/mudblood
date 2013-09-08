{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Mudblood.Mapper.Map
    (
    -- * Types
      Map (..)
    , RoomData (..)
    , ExitData (..)
    , UserValue (..)
    , UserValueClass (..)
    -- * Loading maps
    , mapEmpty, mapFromString, mapFromFile
    -- * Transforming a map
    , next, step
    -- * Querying a map
    , mapCurrentData
    , shortestPath
    -- * Handling User Data
    , getUserValue, putUserValue
    ) where

import Text.JSON
import Text.JSON.Types

import qualified Data.Map as M
import Data.List

import Data.Graph.Inductive
import Data.Graph.Inductive.Tree

type MapGraph = Gr RoomData ExitData

data RoomData = RoomData
    { roomUserData :: UserData
    }
  deriving (Show)

mkRoomData = RoomData
    { roomUserData = M.empty
    }

newtype JSRoom = JSRoom { getJSRoom :: LNode RoomData }

data ExitData = ExitData
    { exitLayer :: String
    , exitKey :: String
    , exitUserData :: UserData
    }
  deriving (Show)

newtype JSExit = JSExit { getJSExit :: LEdge ExitData }

newtype JSUserData = JSUserData { getJSUserData :: UserData }

data Map = Map
    { mapGraph :: MapGraph
    , mapCurrentId :: Node
    }
  deriving (Show)

-- | Create an empty map.
mapEmpty :: Map
mapEmpty = Map 
    { mapGraph = mkGraph [(0, mkRoomData)] []
    , mapCurrentId = 0
    }

-- | Load a map from a string in JSON format.
mapFromString :: String -> Maybe Map
mapFromString str = case decode str of
    Ok map -> Just map
    Error _ -> Nothing

-- | Load a map from a file in JSON format.
mapFromFile :: FilePath -> IO (Maybe Map)
mapFromFile path = readFile path >>= return . mapFromString

instance JSON Map where
    readJSON (JSObject o) = do
        rooms <- valFromObj "rooms" o >>= return . map getJSRoom
        exits <- valFromObj "exits" o >>= return . map getJSExit
        --virtual <- valFromObj "virtual" o
        return $ Map
            { mapGraph   = mkGraph rooms exits
            , mapCurrentId = 0
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
                                       , ("userdata", showJSON $ JSUserData $ roomUserData $ snd $ getJSRoom r)
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
                                          , ("userdata", showJSON $ JSUserData $ exitUserData d)
                                          , ("key", showJSON $ exitKey d)
                                          ]

instance JSON JSUserData where
    readJSON (JSObject o) = return $ JSUserData $ M.map toUserValue $ M.fromList $ fromJSObject o

    readJSON _ = fail "Expected object"

    showJSON d = showJSON $ toJSObject $ M.toList $ ((M.map fromUserValue (getJSUserData d)) :: M.Map String JSValue)

------------------------------------------------------------------------------

type UserData = M.Map String UserValue

data UserValue = UserValueNull
               | UserValueBool Bool
               | UserValueRational Rational
               | UserValueString String
               | UserValueArray [UserValue]

instance Show UserValue where
    show UserValueNull = "<null>"
    show (UserValueBool v) = show v
    show (UserValueRational v) = show v
    show (UserValueString v) = v
    show (UserValueArray v) = concat $ intersperse "," (map show v)

class UserValueClass a where
    toUserValue :: a -> UserValue
    fromUserValue :: UserValue -> a

instance UserValueClass JSValue where
    toUserValue JSNull = UserValueNull
    toUserValue (JSBool v) = UserValueBool v
    toUserValue (JSRational _ v) = UserValueRational v
    toUserValue (JSString v) = UserValueString $ fromJSString v
    toUserValue (JSArray v) = UserValueArray $ map toUserValue v
    toUserValue _ = UserValueNull

    fromUserValue UserValueNull = JSNull
    fromUserValue (UserValueBool v) = JSBool v
    fromUserValue (UserValueRational v) = JSRational True v
    fromUserValue (UserValueString v) = JSString $ toJSString v
    fromUserValue (UserValueArray v) = JSArray $ map fromUserValue v

instance UserValueClass Bool where
    toUserValue v = UserValueBool v

    fromUserValue (UserValueBool v)     = v
    fromUserValue (UserValueNull)       = False
    fromUserValue (UserValueRational v) = v /= 0
    fromUserValue (UserValueString v)   = v /= ""
    fromUserValue (UserValueArray v)    = not $ null v
    fromUserValue _                     = False

instance UserValueClass Int where
    toUserValue v = UserValueRational (fromIntegral v)

    fromUserValue (UserValueRational v) = (round v)
    fromUserValue (UserValueBool True)  = 1
    fromUserValue (UserValueBool False) = 0
    fromUserValue (UserValueString "")  = 0
    fromUserValue (UserValueString v)   = 1
    fromUserValue (UserValueArray v)    = length v
    fromUserValue _                     = 0

instance UserValueClass String where
    toUserValue v = UserValueString v

    fromUserValue (UserValueString v)   = v
    fromUserValue (UserValueBool v)     = if v then "yes" else "no"
    fromUserValue (UserValueNull)       = ""
    fromUserValue x                     = show x

getUserValue :: (UserValueClass a) => String -> UserData -> a
getUserValue key ud = fromUserValue $ M.findWithDefault UserValueNull key ud

putUserValue :: (UserValueClass a) => String -> a -> UserData -> UserData
putUserValue key val ud = M.insert key (toUserValue val) ud

------------------------------------------------------------------------------

-- | Get the current room's RoomData.
mapCurrentData :: Map -> RoomData
mapCurrentData m = let cur = mapCurrentId m
                       dat = case lab (mapGraph m) cur of
                               Just dat' -> dat'
                               Nothing   -> mkRoomData
                   in dat

-- | Map a function over the graph of a map.
mapMapGraph :: (MapGraph -> MapGraph) -> Map -> Map
mapMapGraph f g = g { mapGraph = f (mapGraph g) }

-- | Shortest path from one room to another.
shortestPath :: (Real w) => Map -> (ExitData -> w) -> Int -> Int -> [String]
shortestPath m weightfun src dest = let graph = mapGraph m
                                    in case sp src dest (emap weightfun graph) of
                                        []            -> []
                                        (first:nodes) -> reverse $ snd $ foldl (foldPath graph) (first, []) nodes

    where foldPath graph (s, p) d = let (_, _, edge) = head $ filter (goesTo d) $ out graph s
                                    in (d, (exitKey edge):p)
          goesTo d' (_, d, _) = d == d'

------------------------------------------------------------------------------

next :: String -> Map -> Node -> Maybe Node
next key m n = case filter (hasKey key . snd) (lsuc (mapGraph m) n) of
    [] -> Nothing
    (x:xs) -> Just (fst x)
  where
    hasKey key l = exitKey l == key

step :: String -> Map -> Map
step key m = case next key m (mapCurrentId m) of
    Nothing -> m
    Just n  -> m { mapCurrentId = n }

