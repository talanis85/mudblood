{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mudblood.Mapper.Map
    (
    -- * Types
      Map
    , RoomData (..), ExitData (..)
    , roomTag, roomValue, roomUserData
    , exitLayer, exitKey, exitUserData, exitProvisional
    , exitSplit, exitValue
    , mapEmpty, mapEmpty'
    -- * User Data
    , UserData, UserValue (..)
    , userValueToInt, userValueFromInt
    , userValueToString, userValueFromString
    , userValueToStringArray, userValueFromStringArray
    , lookupUserValue
    -- * Transforms and queries
    , mapRoomData, mapExitData, mapExitDataByNodes
    , mapFindRoomBy
    , mapGetExits
    , mapFindAdjacentRooms, mapFindAdjacentRoom
    , mapFindPreviousRoom
    , mapGetEntrances
    , mapAddExit, mapAddExit', mapDeleteExit
    -- , mapAddProvisionalExit
    , mapAddRoom, mapAddRoom', mapDeleteRoom
    -- * Map algorithms
    , mapShortestPath, mapOverlay
    -- * Indexes
    -- , mapGenRoomIndex
    -- * Drawing
    , mapDrawAscii
    ) where

import Prelude hiding (catch)
import Control.Exception
import Control.Lens hiding ((&), Context)

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

data RoomData a = RoomData
    { _roomTag :: Maybe String
    , _roomUserData :: UserData
    , _roomValue :: a
    }
  deriving (Show, Eq, Ord)

mkRoomData x = RoomData
    { _roomTag = Nothing
    , _roomUserData = M.empty
    , _roomValue = x
    }

makeLenses ''RoomData

data ExitData a = ExitData
    { _exitLayer :: String
    , _exitKey :: String
    , _exitUserData :: UserData
    , _exitProvisional :: Bool
    , _exitSplit :: Bool
    , _exitValue :: a
    }
  deriving (Show, Eq, Ord)

makeLenses ''ExitData

type Map r e = Gr (RoomData r) (ExitData e)

-----------------------------------------------------------------------------
-- CONSTRUCTORS
-----------------------------------------------------------------------------

-- | Create an empty map.
mapEmpty :: r -> Map r e
mapEmpty x = mkGraph [(0, mkRoomData x)] []

mapEmpty' :: (Monoid r) => Map r e
mapEmpty' = mapEmpty mempty

------------------------------------------------------------------------------
-- TRANSFORMS AND QUERIES
------------------------------------------------------------------------------

{-
getExitData :: Int -> String -> Maybe String -> Map r e -> Maybe (ExitData e)
getExitData r e l m =
    case filter exitFilter (out m r) of
            [] -> Nothing
            ((a,b,label):_) -> Just label
  where
    exitFilter (a,b,label) = case l of
        Nothing -> (exitKey label) == e
        Just l  -> (exitKey label) == e && (exitLayer label) == l

getRoomData :: Int -> Map r e -> Maybe (RoomData r)
getRoomData r m = lab m r

mapGetExitData :: Int -> String -> Maybe String -> Map r e -> UserData
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
-}

{-
mapRoomData :: Node -> Lens' Map UserData
mapRoomData node = lens (mapGetRoomData node) (\m x -> mapModifyRoomData node (const x) m)

mapExitData :: Node -> String -> Maybe String -> Lens' Map UserData
mapExitData node key layer = lens (mapGetExitData node key layer) (\m x -> mapModifyExitData node key layer (const x) m)
-}

{-
maybeNodeLabel :: (Graph gr) => Node -> Lens' (gr a b) (Maybe a)
maybeNodeLabel node = lens a b
  where
    a g = lab g node
    b g l = gmap (setNodeLabel l) g
    setNodeLabel l ctx@(i, n, l', o)
      | node == n = (i, n, l, o)
      | otherwise = ctx

nodeEdgeLabels :: (Graph gr) => Node -> String -> Lens' (gr a b) [LEdge b]
nodeEdgeLabels node = lens a b
  where
    a g = out g node
    b g l = gmap (setEdgeLabel
-}

mapRoomData :: Node -> Traversal' (Map r e) (RoomData r)
mapRoomData node = traversal go
  where
    go :: (Applicative f) => (RoomData r -> f (RoomData r)) -> Map r e -> f (Map r e)
    go focus g = ufold (f focus) (pure empty) g
    f :: (Applicative f) => (RoomData r -> f (RoomData r)) -> Context (RoomData r) (ExitData e) -> f (Map r e) -> f (Map r e)
    f focus ctx@(i, n, l, o) g
      | node == n = flip (&) <$> g <*> ((,,,) <$> pure i <*> pure n <*> focus l <*> pure o)
      | otherwise = flip (&) <$> g <*> ((,,,) <$> pure i <*> pure n <*> pure l <*> pure o)

mapExitData :: Node -> String -> Maybe String -> Traversal' (Map r e) (ExitData e)
mapExitData node key layer = traversal go
  where
    go :: (Applicative f) => (ExitData e -> f (ExitData e)) -> Map r e -> f (Map r e)
    go focus g = ufold (f focus) (pure empty) g
    f :: (Applicative f) => (ExitData e -> f (ExitData e)) -> Context (RoomData r) (ExitData e) -> f (Map r e) -> f (Map r e)
    f focus ctx@(i, n, l, o) g
      | node == n = flip (&) <$> g <*> ((,,,) <$> traverse (ingoing focus) i <*> pure n <*> pure l <*> traverse (outgoing focus) o)
      | otherwise = flip (&) <$> g <*> ((,,,) <$> traverse (ingoing focus) i <*> pure n <*> pure l <*> pure o)
    ingoing focus edge@(label, src)
      | src == node = outgoing focus (label, src)
      | otherwise = pure edge
    outgoing focus edge@(label, dest) = case layer of
      Nothing ->
        if label ^. exitKey == key
           then liftA2 (,) (focus label) (pure dest)
           else pure edge
      Just layer' ->
        if label ^. exitKey == key && label ^. exitLayer == layer'
           then liftA2 (,) (focus label) (pure dest)
           else pure edge

mapExitDataByNodes :: Node -> Node -> Traversal' (Map r e) (ExitData e)
mapExitDataByNodes nodeFrom nodeTo = traversal go
  where
    go focus g = ufold (f focus) (pure empty) g
    f focus ctx@(i, n, l, o) g
      | nodeFrom == n = flip (&) <$> g <*> ((,,,) <$> pure i <*> pure n <*> pure l <*> traverse (outgoing focus) o)
      | nodeTo == n = flip (&) <$> g <*> ((,,,) <$> traverse (ingoing focus) i <*> pure n <*> pure l <*> pure o)
      | otherwise = flip (&) <$> g <*> pure ctx
    ingoing focus edge@(label, src)
      | src == nodeFrom = liftA2 (,) (focus label) (pure src)
      | otherwise = pure edge
    outgoing focus edge@(label, dest)
      | dest == nodeTo = liftA2 (,) (focus label) (pure dest)
      | otherwise = pure edge

mapFindRoomsBy :: (RoomData r -> Bool) -> Map r e -> [Node]
mapFindRoomsBy f m =
    let folder ctx accu = if f (lab' ctx) then (node' ctx) : accu else accu
    in ufold folder [] m

mapFindRoomBy :: (RoomData r -> Bool) -> Map r e -> Maybe Node
mapFindRoomBy f m = listToMaybe $ mapFindRoomsBy f m

mapGetExits :: Node -> Map r e -> [(Node, ExitData e)]
mapGetExits room m = lsuc m room

mapGetEntrances :: Node -> Map r e -> [(Node, ExitData e)]
mapGetEntrances room m = lpre m room

mapFindAdjacentRoom :: Node -> String -> Map r e -> Maybe Node
mapFindAdjacentRoom r key m = listToMaybe $ mapFindAdjacentRooms r key m

mapFindAdjacentRooms :: Node -> String -> Map r e -> [Node]
mapFindAdjacentRooms r key m = fmap fst $ filter ((== key) . view exitKey . snd) $ mapGetExits r m

mapFindPreviousRoom :: Node -> String -> Map r e -> Maybe Node
mapFindPreviousRoom r key m =
    fmap fst $ listToMaybe $ filter ((== key) . view exitKey . snd) $ mapGetEntrances r m

mapAddExit :: Node -> String -> Node -> String -> e -> Map r e -> Map r e
mapAddExit src key dest layer value =
    insEdge (src, dest, ExitData
        { _exitLayer = layer
        , _exitKey = key
        , _exitUserData = M.empty
        , _exitProvisional = False
        , _exitSplit = False
        , _exitValue = value
        })

mapAddExit' :: (Monoid e) => Node -> String -> Node -> String -> Map r e -> Map r e
mapAddExit' src key dest layer = mapAddExit src key dest layer mempty

{-
mapAddProvisionalExit :: Node -> String -> Node -> String -> e -> Map r e -> Map r e
mapAddProvisionalExit src key dest layer value g =
    case mapFindAdjacentRoom src key g of
        Nothing ->
            insEdge (src, dest, ExitData
                { _exitLayer = layer
                , _exitKey = key
                , _exitUserData = M.empty
                , _exitProvisional = True
                , _exitValue = value
                }) g
        Just _ -> g
-}

mapDeleteExit :: Node -> String -> String -> Map r e -> Map r e
mapDeleteExit node key layer g = case match node g of
  (Nothing, _) -> g
  (Just (i,n,l,o), g') -> (i,n,l,filter f o) & g'
  where
    f (l, dest)
      | l ^. exitKey == key && l ^. exitLayer == layer = False
      | otherwise = True

mapAddRoom :: r -> Map r e -> Maybe (Map r e, Node)
mapAddRoom value g = case newNodes 1 g of
    [] -> Nothing
    (n:_) -> Just (insNode (n, mkRoomData value) g, n)

mapAddRoom' :: (Monoid r) => Map r e -> Maybe (Map r e, Node)
mapAddRoom' g = mapAddRoom mempty g

mapDeleteRoom :: Node -> Map r e -> Map r e
mapDeleteRoom n = delNode n

------------------------------------------------------------------------------
-- GRAPH ALGORITHMS
------------------------------------------------------------------------------

-- | Shortest path from one room to another.
mapShortestPath :: (Real w) => (ExitData e -> w) -> Node -> Node -> Map r e -> [(String, Node)]
mapShortestPath weightfun src dest graph =
    case sp src dest (emap weightfun graph) of
        Nothing            -> []
        Just []            -> []
        Just (first:nodes) -> reverse $ snd $ foldl (foldPath graph) (first, []) nodes
  where
    foldPath graph (s, p) d = let (_, _, edge) = head $ filter (goesTo d) $ out graph s
                              in (d, ((edge ^. exitKey), d):p)
    goesTo d' (_, d, _) = d == d'

mapOverlay :: [String] -> Map r e -> Map r e
mapOverlay layers gr = gmap (applyLayers layers) gr
  where
    applyLayers layers (i, n, l, o) = (i, n, l, overlay' layers o)

    overlay' layers edges = foldr (unionBy equalKey) [] $ reverse $ splitByLayers layers edges

    splitByLayers layers edges = snd $ foldr splitByLayers' (edges, []) layers
    splitByLayers' layer (edges, cur) = (edges, filter ((== layer) . view exitLayer . fst) edges : cur)

    equalKey (a, _) (b, _) = a ^. exitKey == b ^. exitKey

------------------------------------------------------------------------------
-- INDEXES
------------------------------------------------------------------------------

{-
mapGenRoomIndex :: String -> (UserValue -> [UserValue]) -> Map r e -> M.Map UserValue Node
mapGenRoomIndex key f m = M.fromList $ concat $ map prepIndex $ labNodes m
    where prepIndex (node, l) = case M.lookup key (roomUserData l) of
            Nothing -> []
            Just x -> map (\y -> (y, node)) (f x)
-}

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

mapDrawAscii :: Int -> Int -> Node -> Map r e -> [String]
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

dfsDraw :: Node -> Map r e -> [(Int, Int, Char)]
dfsDraw n gFull = (fst $ dfsDraw' 20 n (0, 0) gFull) ++ [(0, 0, 'X')]
  where
    dfsDraw' :: Int -> Node -> (Int, Int) -> Map r e -> ([(Int, Int, Char)], Map r e)
    dfsDraw' 0 _ _ g = ([], g)
    dfsDraw' _ _ _ g | isEmpty g = ([], g)
    dfsDraw' limit n (x, y) g =
        let hasNonstandardExits =
              any ((== Nothing) . flip lookup standardExits . view (_2 . exitKey)) $ lsuc gFull n
            symbol = if hasNonstandardExits then '%' else '#'
            roompic = (x, y, symbol)
        in case match n g of
            (Just (i, n', l, o), g') -> let (l, g'') = foldr (recurse limit x y) ([], g') o
                                        in (roompic:l, g'')
            (Nothing, g') -> ([], g')
    recurse :: Int -> Int -> Int
            -> (ExitData e, Node) -> ([(Int, Int, Char)], Map r e) -> ([(Int, Int, Char)], Map r e)
    recurse limit x y (d, n) (list, g) =
        case getDelta (d ^. exitKey) of
            Nothing -> (list, g)
            Just (dx, dy, ch') ->
                let ch = if d ^. exitProvisional then ':' else ch'
                    shortstroke = [(x+dx, y+dy, ch)]
                    stroke = [(x+dx, y+dy, ch), (x+dx*2, y+dy*2, ch)]
                    (newlist, newg) = dfsDraw' (limit - 1) n (x+dx*3, y+dy*3) g
                in if d ^. exitSplit
                        then (shortstroke ++ list, g)
                        else (stroke ++ newlist ++ list, newg)
