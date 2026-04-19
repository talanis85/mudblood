{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Mudblood.Mapper.Map
    (
    -- * Types
      Map
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
    , mapRoomData, mapExitData
    , mapFindRoomBy
    , mapGetExits, mapFindAdjacentRoom
    , mapGetEntrances
    , mapAddExit, mapDeleteExit
    , mapAddProvisionalExit
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
import Control.Lens

import Text.JSON
import Text.JSON.Types

import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Monad

import Data.Graph.Inductive hiding (Gr)
import Data.Graph.Inductive.PatriciaTree

import Mudblood.UserData

-----------------------------------------------------------------------------
-- DATA DEFINITIONS
-----------------------------------------------------------------------------

type Map = Gr RoomData ExitData

data RoomData = RoomData
    { roomUserData :: UserData
    }
  deriving (Show, Eq, Ord)

mkRoomData = RoomData
    { roomUserData = M.empty
    }

newtype JSRoom = JSRoom { getJSRoom :: LNode RoomData }

data ExitData = ExitData
    { exitLayer :: String
    , exitKey :: String
    , exitUserData :: UserData
    , exitProvisional :: Bool
    }
  deriving (Show, Eq, Ord)

newtype JSExit = JSExit { getJSExit :: LEdge ExitData }

-----------------------------------------------------------------------------
-- CONSTRUCTORS
-----------------------------------------------------------------------------

-- | Create an empty map.
mapEmpty :: Map
mapEmpty = mkGraph [(0, mkRoomData)] []

-- | Load a map from a string in JSON format.
mapFromString :: String -> Maybe Map
mapFromString str = case decodeStrict str of
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
        return $ mkGraph rooms exits

    readJSON _ = fail "Expected object"

    showJSON m = let g = elfilter (not . exitProvisional) m
                 in showJSON $ toJSObject [ ("rooms", showJSON $ map JSRoom (labNodes g))
                                          , ("exits", showJSON $ map JSExit (labEdges g))
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
                , exitProvisional = False
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

------------------------------------------------------------------------------
-- TRANSFORMS AND QUERIES
------------------------------------------------------------------------------

getExitData :: Int -> String -> Maybe String -> Map -> Maybe ExitData
getExitData r e l m =
    case filter exitFilter (out m r) of
            [] -> Nothing
            ((a,b,label):_) -> Just label
  where
    exitFilter (a,b,label) = case l of
        Nothing -> (exitKey label) == e
        Just l  -> (exitKey label) == e && (exitLayer label) == l

getRoomData :: Int -> Map -> Maybe RoomData
getRoomData r m = lab m r

mapGetExitData :: Int -> String -> Maybe String -> Map -> UserData
mapGetExitData room ex layer m = fromMaybe M.empty $ fmap exitUserData $ getExitData room ex layer m

mapGetRoomData :: Int -> Map -> UserData
mapGetRoomData room m = fromMaybe M.empty $ fmap roomUserData $ getRoomData room m

mapModifyRoomData :: Node -> (UserData -> UserData) -> Map -> Map
mapModifyRoomData node f = gmap (modifyRoom node f)
    where
        modifyRoom node f ctx@(i, n, l, o)
            | node == n = (i, n, l { roomUserData = f (roomUserData l) }, o)
            | otherwise = ctx

mapModifyExitData :: Node -> String -> Maybe String -> (UserData -> UserData) -> Map -> Map
mapModifyExitData node key layer f = gmap (modifyRoom node f)
    where
        modifyRoom node f ctx@(i, n, l, o)
            | node == n = (i, n, l, map (modifier f) o)
            | otherwise = ctx
        modifier f (label, a) = case layer of
            Just layer -> if exitKey label == key && exitLayer label == layer
                            then (label { exitUserData = f (exitUserData label) }, a)
                            else (label, a)
            Nothing    -> if exitKey label == key
                            then (label { exitUserData = f (exitUserData label) }, a)
                            else (label, a)

mapRoomData :: Node -> Lens' Map UserData
mapRoomData node = lens (mapGetRoomData node) (\m x -> mapModifyRoomData node (const x) m)

mapExitData :: Node -> String -> Maybe String -> Lens' Map UserData
mapExitData node key layer = lens (mapGetExitData node key layer) (\m x -> mapModifyExitData node key layer (const x) m)

mapFindRoomsBy :: (UserData -> Bool) -> Map -> [Node]
mapFindRoomsBy f m =
    let folder ctx accu = if f (roomUserData $ lab' ctx) then (node' ctx) : accu else accu
    in ufold folder [] m

mapFindRoomBy :: (UserData -> Bool) -> Map -> Maybe Node
mapFindRoomBy f m = listToMaybe $ mapFindRoomsBy f m

mapGetExits :: Node -> Map -> [(Node, ExitData)]
--mapGetExits room m = lsuc (elfilter (not . exitProvisional) m) room
mapGetExits room m = lsuc m room

mapFindAdjacentRoom :: Node -> String -> Map -> Maybe Node
mapFindAdjacentRoom r key m =
    fmap fst $ listToMaybe $ filter ((== key) . exitKey . snd) $ mapGetExits r m

mapGetEntrances :: Node -> Map -> [(Node, ExitData)]
mapGetEntrances room m = lpre m room

mapAddExit :: Node -> String -> Node -> String -> Map -> Map
mapAddExit src key dest layer =
    insEdge (src, dest, ExitData
        { exitLayer = layer
        , exitKey = key
        , exitUserData = M.empty
        , exitProvisional = False
        })
    .
    mapDeleteExit src key layer

mapAddProvisionalExit :: Node -> String -> Node -> String -> Map -> Map
mapAddProvisionalExit src key dest layer g =
    case mapFindAdjacentRoom src key g of
        Nothing ->
            insEdge (src, dest, ExitData
                { exitLayer = layer
                , exitKey = key
                , exitUserData = M.empty
                , exitProvisional = True
                }) g
        Just _ -> g

mapDeleteExit :: Node -> String -> String -> Map -> Map
mapDeleteExit node key layer g =
    let edges = filter (isEdge key layer) $ lsuc g node
    in foldr deleteIt g edges
  where
    deleteIt (n, e) g = delLEdge (node, n, e) g
    isEdge key layer (_, d) = key == (exitKey d) && layer == (exitLayer d)

mapAddRoom :: Map -> Maybe (Map, Node)
mapAddRoom g = case newNodes 1 g of
    [] -> Nothing
    (n:_) -> Just (insNode (n, mkRoomData) g, n)

mapDeleteRoom :: Node -> Map -> Map
mapDeleteRoom n = delNode n

------------------------------------------------------------------------------
-- GRAPH ALGORITHMS
------------------------------------------------------------------------------

-- | Shortest path from one room to another.
mapShortestPath :: (Real w) => (ExitData -> w) -> Node -> Node -> Map -> [(String, Node)]
mapShortestPath weightfun src dest graph =
    case sp src dest (emap weightfun graph) of
        Nothing            -> []
        Just []            -> []
        Just (first:nodes) -> reverse $ snd $ foldl (foldPath graph) (first, []) nodes
  where
    foldPath graph (s, p) d = let (_, _, edge) = head $ filter (goesTo d) $ out graph s
                              in (d, ((exitKey edge), d):p)
    goesTo d' (_, d, _) = d == d'

mapOverlay :: [String] -> Map -> Map
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

mapGenRoomIndex :: String -> (UserValue -> [UserValue]) -> Map -> M.Map UserValue Node
mapGenRoomIndex key f m = M.fromList $ concat $ map prepIndex $ labNodes m
    where prepIndex (node, l) = case M.lookup key (roomUserData l) of
            Nothing -> []
            Just x -> map (\y -> (y, node)) (f x)

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

mapDrawAscii :: Int -> Int -> Node -> Map -> [String]
mapDrawAscii w h cur graph =
    let shiftCoords dx dy = map $ \(x, y, ch) -> (x+dx, y+dy, ch)
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

dfsDraw :: Node -> Map -> [(Int, Int, Char)]
dfsDraw n g = (fst $ dfsDraw' 20 n (0, 0) g) ++ [(0, 0, 'X')]

dfsDraw' :: Int -> Node -> (Int, Int) -> Map -> ([(Int, Int, Char)], Map)
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
            -> (ExitData, Node) -> ([(Int, Int, Char)], Map) -> ([(Int, Int, Char)], Map)
    recurse limit x y (d, n) (list, g) =
        case getDelta (exitKey d) of
            Nothing -> (list, g)
            Just (dx, dy, ch') ->
                let ch = if exitProvisional d then ':' else ch'
                    shortstroke = [(x+dx, y+dy, ch)]
                    stroke = [(x+dx, y+dy, ch), (x+dx*2, y+dy*2, ch)]
                    (newlist, newg) = dfsDraw' (limit - 1) n (x+dx*3, y+dy*3) g
                in if lookupUserValue "split" (exitUserData d) == UserValueBool True
                        then (shortstroke ++ list, g)
                        else (stroke ++ newlist ++ list, newg)
