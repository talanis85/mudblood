{-# LANGUAGE TypeFamilies,TypeOperators,FlexibleContexts #-}

module Mudblood.Contrib.MG.Mapper
    ( R_Mapper, MGMapperState (..)
    , mkMGMapperState
    , mgPrepareMap
    , mgFindRoom
    , mgWalk
    , mgStepper
    , triggerGmcpRoom
    , mapperWidgets
    ) where

import Data.Has
import Mudblood
import Mudblood.Contrib.MG.State
import Mudblood.Contrib.MG.Common
import qualified Data.Graph.Inductive as G
import Data.Maybe
import qualified Data.Map as M
import Control.Arrow
import Data.GMCP

data R_Mapper = R_Mapper
type instance TypeOf R_Mapper = MGMapperState

data MGMapperState = MGMapperState
    { mgMapperRoomHash    :: String
    }

mkMGMapperState = MGMapperState
    { mgMapperRoomHash      = ""
    }

portals =
    [ (1, "tamibar", "bf586f14b202c43ea8727aefe7d5ae8a")
    , (2, "drachenzinnen", "a8760b707dbcf11d85da1c841abad7c9")
    , (3, "pv", "7ccced1f8b62fabb7b1494a6d9fd164d")
    , (4, "hochebene", "d7fe62577bcf655c97e285096d22c333")
    , (5, "polar", "f0d2288ec1a24d86318c8fbf9a221a08")
--    , (6, "tundra", "")
    , (8, "waldweg", "fbf2d086dd2983fd1f2e02d71941eff6")
    , (9, "valgessa", "0bc13858fa88bc03a57714362ea574ad")
    , (10, "wueste", "69aeaf5183002e46c31abf400d01c5d1")
    , (11, "aurora", "622018029d883479a5b07fb31f830230")
    , (12, "svolvaer", "9a45fd1465923334f1ee7a5d6617013f")
    , (13, "bergdorf", "aa17651e56c3f4f15a0be1bb30b6dac8")
    , (14, "nibelheim", "5429e40b37348ab7f679020a052435c0")
    , (16, "dschungel", "b712083865c62f24846239c6f14cb974")
--    , (18, "fernwest", "")
    , (19, "vland", "521620dec482f66752aa1ef0ca3cb806")
    , (20, "rodelbahn", "a48224c2b328606166f35c0716b03b46")
    , (21, "hobbitdorf", "2e844c5316daef31d19ed74861025396")
    , (22, "akhar", "a7920efb5367e33a39c689bb0814e9ac")
    , (23, "schacht", "1019c848caffd364c9a8f1074ef5a7c4")
    , (24, "endederwelt", "3d444f32639c6569fcc53598a56891d8")
    , (25, "friedhof", "9e1fad054f198baac665ebd228e7e1e3")
    , (26, "shaky", "f538d7b66dc3a114147cb12462a4dafa")
    , (27, "kaempfer", "7a58372be3796c191815e56202706937")
    , (28, "tortuga", "c28da35dcbcc94bb197f790efe7237b7")
    , (29, "katzmandu", "e0f281cbcfc32e9dd049abfac5418ce4")
    , (30, "rieseninsel", "b443892bf4661f2ede266eaee84ea776")
    , (31, "gebirge", "6577c8f842a9cd0661e27f9a0171367b")
    , (32, "portalraum", "297770a7a2af5e85fb461230aa6fdfe4")
    , (33, "umjak", "daf72c5edc7464972d4af4bdd83429b3")
    , (34, "tanjian", "cb057f19224b8d8d571b359487640800")
    , (36, "innuit", "8572f582b7c9fd45ce8e5d7ecbc8df71")
    , (37, "werwolfinsel", "c3917d2ca063f4304938051f1e51db86")
    , (38, "krylaios", "dbcef72d665565d2e277532d0d57f20b")
    , (39, "magieinsel", "eae4cf8f8bbe343c5bfc4f627290bdf6")
    , (40, "abgrund", "9ffe18fd06b3413a982ae16191d27b98")
    ]

mapHash :: Map -> String -> Maybe G.Node
mapHash m hash = case findRoomsWith (\x -> getUserValue "hash" (roomUserData x) == hash) m of
                    [] -> Nothing
                    (x:_) -> Just x

mapAddPortals :: (String -> Maybe G.Node) -> G.Context RoomData ExitData -> G.Context RoomData ExitData
mapAddPortals hashmap ctx@(i, n, l, o) =
    if checkPortal l
        then (i, n, l, o ++ (mapMaybe (genEdge hashmap) portals))
        else ctx
  where
    checkPortal r =
        let hash = getUserValue "hash" (roomUserData r)
        in not $ null $ filter (\(a,b,c) -> c == hash) portals
    genEdge hashmap (n, name, hash) =
        case hashmap hash of
            Nothing -> Nothing
            Just target -> Just (ExitData { exitLayer = "base", exitKey = "t " ++ (show n), exitUserData = M.empty }, target)

mgPrepareMap m = mapMapGraph (G.gmap $ mapAddPortals (mapHash m)) m

mgStepper s m = case findNextRoom s (mgPrepareMap m) (mapCurrentId m) of
                    Nothing -> m
                    Just n -> mapSetCurrent n m

------------------------------------------------------------------------------

mgWalk :: Int -> MB u ()
mgWalk dest = do
    map <- getMap

    let map' = mgPrepareMap map

    let walkerTrigger = arr $ const $ WalkerContinue
    let weightfun edge = max (1 :: Int) (getUserValue "weight" (exitUserData edge))

    case shortestPath map' weightfun (mapCurrentId map) dest of
        []           -> mbError "Path not found"
        (first:path) -> do
                        echo $ "Path is: " ++ show (first:path)
                        modifyTriggers $ fmap (:>>: (walker map' walkerTrigger path))
                        send first
                        modifyMap $ mapStep first

mgFindRoom :: String -> MB u (Maybe Int)
mgFindRoom name = do
    map <- getMap
    case findRoomsWith (\x -> getUserValue "tag" (roomUserData x) == name) map of
        (x:_) -> return $ Just x
        [] -> case findRoomsWith (\x -> getUserValue "hash" (roomUserData x) == name) map of
            (x:_) -> return $ Just x
            [] -> return Nothing

triggerGmcpRoom :: (Has R_Mapper u) => MBTrigger u GMCP ()
triggerGmcpRoom = marr $ \gmcp ->
    case gmcpModule gmcp of
        "MG.room.info" -> modifyU R_Mapper $ \s -> s { mgMapperRoomHash = fromMaybe "" $ getStringField "id" gmcp }
        _ -> failT

mapperWidgets :: (Has R_Mapper u) => MB u [UIWidget]
mapperWidgets = do
    mapstat <- getU R_Mapper
    map <- getMap
    let (id, room) = (mapCurrentId map, mapCurrentData map)
    let maptable = UIWidgetTable
            [ [ "Raum #:", (show $ id) ]
            , [ "Tag:", (getUserValue "tag" $ roomUserData room) ]
            , [ "Hash:", (getUserValue "hash" $ roomUserData room) ]
            , [ "Hash':", mapstat # mgMapperRoomHash ]
            ]
    return [ maptable ]
