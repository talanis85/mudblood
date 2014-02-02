{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Mudblood.Mapper.Map
    (
    -- * Types
      Map (..)
    , MapGraph
    , RoomData (..), ExitData (..)
    , mapEmpty, mapFromString
    , mapToString
    -- * User Data
    , UserData, UserValue (..)
    , userValueToInt, userValueFromInt
    , userValueToString, userValueFromString
    , userValueToStringArray, userValueFromStringArray
    , lookupUserValue
    -- * Transforms and queries
    , mapModifyCurrentId, mapModifyGraph
    , mapGetRoomData, mapGetExitData
    , mapModifyRoomData, mapModifyExitData
    , mapFindRoomBy, mapFindRoomByIndex
    , mapGetExits, mapFindAdjacentRoom
    , mapGetEntrances
    , mapAddExit, mapDeleteExit
    , mapAddRoom, mapDeleteRoom
    -- * Map algorithms
    , mapShortestPath, mapOverlay
    -- * Indexes
    , mapGenRoomIndex
    -- * Drawing
    , mapDrawAscii
    ) where

import Prelude hiding (catch)
import Control.Exception

import Text.JSON
import Text.JSON.Types

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Monad

import Data.Graph.Inductive hiding (Gr)
import Data.Graph.Inductive.PatriciaTree

-----------------------------------------------------------------------------
-- DATA DEFINITIONS
-----------------------------------------------------------------------------

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
  deriving (Show, Eq)

newtype JSExit = JSExit { getJSExit :: LEdge ExitData }

newtype JSUserData = JSUserData { getJSUserData :: UserData }

data Map = Map
    { mapGraph :: MapGraph
    , mapCurrentId :: Node
    , mapRoomIndices :: M.Map String (M.Map UserValue Node)
    }

-----------------------------------------------------------------------------
-- CONSTRUCTORS
-----------------------------------------------------------------------------

-- | Create an empty map.
mapEmpty :: Map
mapEmpty = Map 
    { mapGraph = mkGraph [(0, mkRoomData)] []
    , mapCurrentId = 0
    , mapRoomIndices = M.empty
    }

-- | Load a map from a string in JSON format.
mapFromString :: String -> Maybe Map
mapFromString str = case decode str of
    Ok map -> Just map
    Error _ -> Nothing

-- | Load a map from a file in JSON format.
mapFromFile :: FilePath -> IO (Maybe Map)
mapFromFile path = catch (readFile path >>= return . mapFromString) errH
    where errH :: IOException -> IO (Maybe Map)
          errH = const $ return Nothing

-----------------------------------------------------------------------------
-- SAVING
-----------------------------------------------------------------------------

mapToString :: Map -> String
mapToString m = encode m

-----------------------------------------------------------------------------
-- JSON ENCODE / DECODE
-----------------------------------------------------------------------------

instance JSON Map where
    readJSON (JSObject o) = do
        rooms <- valFromObj "rooms" o >>= return . map getJSRoom
        exits <- valFromObj "exits" o >>= return . map getJSExit
        --virtual <- valFromObj "virtual" o
        return $ mapEmpty { mapGraph = mkGraph rooms exits }

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
                                       , ("data", showJSON $ JSUserData $ roomUserData $ snd $ getJSRoom r)
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
                                          , ("data", showJSON $ JSUserData $ exitUserData d)
                                          , ("key", showJSON $ exitKey d)
                                          ]

instance JSON JSUserData where
    readJSON (JSObject o) = return $ JSUserData $ M.map toUserValue $ M.fromList $ fromJSObject o

    readJSON _ = fail "Expected object"

    showJSON d = showJSON $ toJSObject $ M.toList $ ((M.map fromUserValue (getJSUserData d)) :: M.Map String JSValue)

------------------------------------------------------------------------------
-- USER DATA
------------------------------------------------------------------------------

type UserData = M.Map String UserValue

data UserValue = UserValueNull
               | UserValueBool Bool
               | UserValueRational Rational
               | UserValueString String
               | UserValueArray [UserValue]
    deriving (Eq, Ord)

instance Show UserValue where
    show UserValueNull = "<null>"
    show (UserValueBool v) = show v
    show (UserValueRational v) = show v
    show (UserValueString v) = v
    show (UserValueArray v) = concat $ intersperse "," (map show v)

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

lookupUserValue :: String -> UserData -> UserValue
lookupUserValue = M.findWithDefault UserValueNull

userValueToInt :: UserValue -> Maybe Int
userValueToInt (UserValueRational v) = Just $ round v
userValueToInt _ = Nothing

userValueFromInt :: Int -> UserValue
userValueFromInt = UserValueRational . fromIntegral

userValueToString :: UserValue -> Maybe String
userValueToString (UserValueString v) = Just v
userValueToString _ = Nothing

userValueFromString :: String -> UserValue
userValueFromString = UserValueString

userValueToStringArray :: UserValue -> Maybe [String]
userValueToStringArray (UserValueArray a) = Just $ mapMaybe userValueToString a

userValueFromStringArray :: [String] -> UserValue
userValueFromStringArray = UserValueArray . map UserValueString

------------------------------------------------------------------------------
-- TRANSFORMS AND QUERIES
------------------------------------------------------------------------------

mapModifyCurrentId :: (Int -> Int) -> (Map -> Map)
mapModifyCurrentId f m = m { mapCurrentId = f (mapCurrentId m) }

mapModifyGraph :: (MapGraph -> MapGraph) -> (Map -> Map)
mapModifyGraph f m = m { mapGraph = f (mapGraph m) }

getExitData :: Int -> String -> MapGraph -> Maybe ExitData
getExitData r e m =
    case filter exitFilter (out m r) of
            [] -> Nothing
            ((a,b,label):_) -> Just label
  where
    exitFilter (a,b,label) = (exitKey label) == e

getRoomData :: Int -> MapGraph -> Maybe RoomData
getRoomData r m = lab m r

mapGetExitData :: Int -> String -> MapGraph -> UserData
mapGetExitData room ex m = fromMaybe M.empty $ fmap exitUserData $ getExitData room ex m

mapGetRoomData :: Int -> MapGraph -> UserData
mapGetRoomData room m = fromMaybe M.empty $ fmap roomUserData $ getRoomData room m

mapModifyRoomData :: Node -> (UserData -> UserData) -> MapGraph -> MapGraph
mapModifyRoomData node f = gmap (modifyRoom node f)
    where
        modifyRoom node f ctx@(i, n, l, o)
            | node == n = (i, n, l { roomUserData = f (roomUserData l) }, o)
            | otherwise = ctx

mapModifyExitData :: Node -> String -> (UserData -> UserData) -> MapGraph -> MapGraph
mapModifyExitData node key f = gmap (modifyRoom node f)
    where
        modifyRoom node f ctx@(i, n, l, o)
            | node == n = (i, n, l, map (modifier f) o)
            | otherwise = ctx
        modifier f (label, a)
            | exitKey label == key = (label { exitUserData = f (exitUserData label) }, a)
            | otherwise = (label, a)

mapFindRoomsBy :: (UserData -> Bool) -> MapGraph -> [Node]
mapFindRoomsBy f m =
    let folder ctx accu = if f (roomUserData $ lab' ctx) then (node' ctx) : accu else accu
    in ufold folder [] m

mapFindRoomBy :: (UserData -> Bool) -> MapGraph -> Maybe Node
mapFindRoomBy f m = listToMaybe $ mapFindRoomsBy f m

mapFindRoomByIndex :: String -> UserValue -> Map -> Maybe Node
mapFindRoomByIndex key val m =
    join $ fmap (M.lookup val) $ M.lookup key (mapRoomIndices m)

mapGetExits :: Node -> MapGraph -> [(Node, ExitData)]
mapGetExits room m = lsuc m room

mapFindAdjacentRoom :: Node -> String -> MapGraph -> Maybe Node
mapFindAdjacentRoom r key m =
    fmap fst $ listToMaybe $ filter ((== key) . exitKey . snd) $ mapGetExits r m

mapGetEntrances :: Node -> MapGraph -> [(Node, ExitData)]
mapGetEntrances room m = lpre m room

mapAddExit :: Node -> String -> Node -> String -> MapGraph -> MapGraph
mapAddExit src key dest layer =
    insEdge (src, dest, ExitData
        { exitLayer = layer
        , exitKey = key
        , exitUserData = M.empty
        })

mapDeleteExit :: Node -> String -> String -> MapGraph -> MapGraph
mapDeleteExit node key layer g =
    let theEdge = listToMaybe $ filter (isEdge key layer) $ mapGetExits node g
    in case theEdge of
        Nothing -> g
        Just (n, e) -> delLEdge (node, n, e) g
  where
    isEdge key layer (_, d) = key == (exitKey d) && layer == (exitLayer d)

mapAddRoom :: MapGraph -> Maybe (MapGraph, Node)
mapAddRoom g = case newNodes 1 g of
    [] -> Nothing
    (n:_) -> Just (insNode (n, mkRoomData) g, n)

mapDeleteRoom :: Node -> MapGraph -> MapGraph
mapDeleteRoom n = delNode n

------------------------------------------------------------------------------
-- GRAPH ALGORITHMS
------------------------------------------------------------------------------

-- | Shortest path from one room to another.
mapShortestPath :: (Real w) => (ExitData -> w) -> Node -> Node -> MapGraph -> [String]
mapShortestPath weightfun src dest graph =
    case sp src dest (emap weightfun graph) of
        []            -> []
        (first:nodes) -> reverse $ snd $ foldl (foldPath graph) (first, []) nodes
  where
    foldPath graph (s, p) d = let (_, _, edge) = head $ filter (goesTo d) $ out graph s
                              in (d, (exitKey edge):p)
    goesTo d' (_, d, _) = d == d'

mapOverlay :: [String] -> MapGraph -> MapGraph
mapOverlay layers gr = gmap (applyLayers layers) gr
  where
    applyLayers layers (i, n, l, o) = (i, n, l, overlay' layers o)

    overlay' layers edges = foldr (unionBy equalKey) [] $ reverse $ splitByLayers layers edges

    splitByLayers layers edges = snd $ foldr splitByLayers' (edges, []) layers
    splitByLayers' layer (edges, cur) = (edges, filter ((== layer) . exitLayer . fst) edges : cur)

    equalKey (a, _) (b, _) = exitKey a == exitKey b

------------------------------------------------------------------------------
-- INDEXES
------------------------------------------------------------------------------

makeRoomIndex :: MapGraph -> String -> (UserValue -> [UserValue]) -> M.Map UserValue Node
makeRoomIndex m key f = M.fromList $ concat $ map prepIndex $ labNodes m
    where prepIndex (node, l) = case M.lookup key (roomUserData l) of
            Nothing -> []
            Just x -> map (\y -> (y, node)) (f x)

mapGenRoomIndex :: String -> (UserValue -> [UserValue]) -> Map -> Map
mapGenRoomIndex key f m = m { mapRoomIndices = M.insert key newRoomIndex (mapRoomIndices m) }
    where newRoomIndex = makeRoomIndex (mapGraph m) key f


------------------------------------------------------------------------------
-- MAP DRAWING
------------------------------------------------------------------------------

standardExits =
    [ ("n", (0, -1, '|'))
    , ("no", (1, -1, '/'))
    , ("o", (1, 0, '-'))
    , ("so", (1, 1, '\\'))
    , ("s", (0, 1, '|'))
    , ("sw", (-1, 1, '/'))
    , ("w", (-1, 0, '-'))
    , ("nw", (-1, -1, '\\'))
    ]
getDelta ex = lookup ex standardExits

mapDrawAscii :: Int -> Int -> [String] -> Map -> [String]
mapDrawAscii w h over m =
    let cur = mapCurrentId m
        graph = mapOverlay over $ mapGraph m
        shiftCoords dx dy = map $ \(x, y, ch) -> (x+dx, y+dy, ch)
        filterBounds = filter $ \(x, y, ch) -> x >= 0 && y >= 0 && x < w && y < h
        sorter (x, y, _) (x', y', _) = if y == y' then compare x x' else compare y y'
        grouper a b = sorter a b == EQ
        chars = map last $ groupBy grouper $ sortBy sorter $ filterBounds $ shiftCoords (w `div` 2) (h `div` 2) $ dfsDraw cur graph
    in construct 0 0 "" chars
  where
    -- TODO: right fold instead of left fold
    construct :: Int -> Int -> String -> [(Int, Int, Char)] -> [String]
    construct _ _ cur [] = [cur]
    construct x y cur next@((x', y', ch):l) =
        if y' > y
            then cur : construct 0 (y+1) "" next
            else if x' > x
                    then construct (x+1) y (cur ++ " ") next
                    else construct (x+1) y (cur ++ [ch]) l

dfsDraw :: Node -> MapGraph -> [(Int, Int, Char)]
dfsDraw n g = (fst $ dfsDraw' 20 n (0, 0) g) ++ [(0, 0, 'X')]

dfsDraw' :: Int -> Node -> (Int, Int) -> MapGraph -> ([(Int, Int, Char)], MapGraph)
dfsDraw' 0 _ _ g = ([], g)
dfsDraw' _ _ _ g | isEmpty g = ([], g)
dfsDraw' limit n (x, y) g =
    let roompic = (x, y, '#')
    in case match n g of
        (Just (i, n', l, o), g') -> let (l, g'') = foldr (recurse limit x y) ([], g') o
                                    in (roompic:l, g'')
        (Nothing, g') -> ([], g')
  where
    recurse :: Int -> Int -> Int
            -> (ExitData, Node) -> ([(Int, Int, Char)], MapGraph) -> ([(Int, Int, Char)], MapGraph)
    recurse limit x y (d, n) (list, g) =
        case getDelta (exitKey d) of
            Nothing -> (list, g)
            Just (dx, dy, ch) ->
                let shortstroke = [(x+dx, y+dy, ch)]
                    stroke = [(x+dx, y+dy, ch), (x+dx*2, y+dy*2, ch)]
                    (newlist, newg) = dfsDraw' (limit - 1) n (x+dx*3, y+dy*3) g
                in if lookupUserValue "split" (exitUserData d) == UserValueBool True
                        then (shortstroke ++ list, g)
                        else (stroke ++ newlist ++ list, newg)
