{-# LANGUAGE TypeFamilies,TypeOperators,FlexibleContexts #-}

module Mudblood.Contrib.MG.Mapper
    ( module Mudblood.Contrib.MG.Mapper.State
    , module Mudblood.Contrib.MG.Mapper.Portals
    , module Mudblood.Contrib.MG.Mapper.RoomActions
    , module Mudblood.Contrib.MG.Mapper.Hash

    , mgPrepareMap
    -- * Map queries
    , mgFindRoom
    -- * Map actions
    , mgWalk, mgWalkToTag, mgWalkBack
    , mgStep
    -- * Triggers
    --, triggerGmcpRoom
    , roomTriggers
    -- * Widgets
    , mapperWidgets
    ) where

import Data.Maybe
import Data.Monoid
import qualified Data.Map as M

import Control.Monad hiding (forM_)
import Data.Foldable (forM_)
import Control.Lens

import Control.Monad.Trans
import Control.Monad.State hiding (forM_)

import Data.GMCP
import Mudblood
import Mudblood.Contrib.MG.State

import Mudblood.Contrib.MG.Mapper.State
import Mudblood.Contrib.MG.Mapper.Portals
import Mudblood.Contrib.MG.Mapper.RoomActions
import Mudblood.Contrib.MG.Mapper.Hash

standardExits =
    [ ("n", "s")
    , ("no", "sw")
    , ("o", "w")
    , ("so", "nw")
    , ("s", "n")
    , ("sw", "no")
    , ("w", "o")
    , ("nw", "so")
    , ("u", "ob")
    , ("ob", "u")
    ]

mgPrepareMap :: [String] -> Map -> Map
mgPrepareMap over = (mapModifyGraph $ mapOverlay over) . (mapAddPortals [1..40]) -- TODO: Dont hardcode portal set

mgStep :: (MBMonad m u, Has R_Mapper u) => String -> m ()
mgStep s = do
    m <- getMap
    over <- getU R_Mapper >>= return . (^. mapperOverlay)
    case mapFindAdjacentRoom (mapCurrentId m) s (mapGraph $ mgPrepareMap over m) of
                Nothing -> return ()
                Just n -> modifyMap $ mapModifyCurrentId $ const n

------------------------------------------------------------------------------

echoMapper :: (MBMonad m u) => String -> m ()
echoMapper str = echoA $ setFg Magenta $ toAS str

-- TODO: Is this general enough to put in Mudblood.Trigger / Control.Trigger ?
a >>- b = a >>= (keep1 $ const b)

mgWalk :: (Has R_Mapper u) => Int -> MB u ()
mgWalk dest = do
    m <- getMap
    over <- getU R_Mapper >>= return . (^. mapperOverlay)

    let m' = mgPrepareMap over m

    let gmcpChecker = guardGMCP >=> \gmcp ->
            case gmcpModule gmcp of
                "MG.room.info" -> return WalkerContinue
                _ -> failT
    let walkerTrigger = \ev -> do
            s <- getU R_Mapper >>= return . (^. mapperWalkState)
            if s == -1
                then return WalkerStop
                else gmcpChecker ev
    let weightfun edge = fromMaybe 1 $ userValueToInt $ lookupUserValue "weight" (exitUserData edge)

    case mapShortestPath weightfun (mapCurrentId m) dest (mapGraph m') of
        []   -> echoMapper "Path not found"
        path -> do
                echoMapper $ "Path is: " ++ show path
                modifyU R_Mapper $ mapperLastRoom .~ (mapCurrentId m)
                modifyU R_Mapper $ mapperWalkState .~ 0
                runTrigger $ send "ultrakurz" >> walker m' walkerTrigger path >>- send "lang" >>- send "schau"


mgWalkToTag tag = do
    m <- getMap
    case mapFindRoomBy ((== UserValueString tag) . lookupUserValue "tag") (mapGraph m) of
        Nothing -> return ()
        Just r -> mgWalk r

mgWalkBack :: (Has R_Mapper u) => MB u ()
mgWalkBack = getU R_Mapper >>= return . (^. mapperLastRoom) >>= mgWalk

mgFindRoom :: String -> MB u (Maybe Int)
mgFindRoom name = do
    m <- getMap
    let byTag = mapFindRoomBy ((== UserValueString name) . lookupUserValue "tag") $ mapGraph m
    let byHash = mapFindRoomByIndex "hash" (UserValueString name) m
    return $ getFirst $ First byTag `mappend` First byHash

triggerGmcpRoom :: (Has R_Mapper u) => GMCP -> MBTrigger u ()
triggerGmcpRoom gmcp =
    case gmcpModule gmcp of
        "MG.room.info" -> do
            map <- getMap
            modifyU R_Mapper $ mapperRoomHash .~ (fromMaybe "" $ getStringField "id" gmcp)
            forM_ (getStringField "id" gmcp) $ \hash -> do
                when (not $ containsHash hash $ lookupUserValue "hash" $ mapGetRoomData (mapCurrentId map) (mapGraph map)) $ do
                    maybe (echoMapper $ "Raum " ++ hash ++ " nicht gefunden")
                          (modifyMap . mapModifyCurrentId . const)
                          (mapFindRoomByIndex "hash" (UserValueString hash) map)
        _ -> failT

mapperWidgets :: (Has R_Mapper u) => MB u [UIWidget]
mapperWidgets = do
    mapstat <- getU R_Mapper
    m <- getMap
    let id = mapCurrentId m
        room = mapGetRoomData id (mapGraph m)
    let maptable = UIWidgetTable
            [ [ "Raum #:", (show $ id) ]
            , [ "Tag:", (show $ lookupUserValue "tag" room) ]
            , [ "Hash:", (show $ lookupUserValue "hash" room) ]
            , [ "Hash':", mapstat ^. mapperRoomHash ]
            , [ "Overlay:", show $ mapstat ^. mapperOverlay ]
            , [ "Ausgaenge:", let ex = mapGetExits (mapCurrentId m) $ mapGraph $ mgPrepareMap (mapstat ^. mapperOverlay) m
                              in show $ map (\(_, d) -> exitKey d ++ " (" ++ exitLayer d ++ ")") ex
              ]
            ]
    return [ maptable ]

------------------------------------------------------------------------------

roomTriggers :: (Has R_Mapper u) => MBTriggerFlow u
                                 -> MBTriggerFlow u
                                 -> MBTriggerFlow u
roomTriggers o i = Volatile $ statefulT (o, i, "") $ roomTriggers'
    where
        roomTriggers' :: (Has R_Mapper u)
                      => TriggerEvent
                      -> StateT (MBTriggerFlow u, MBTriggerFlow u, String) (MBTrigger u) [TriggerEvent]
        roomTriggers' ev = do
            (roomOutTriggers, roomInTriggers, _) <- get
            case ev of
                SendTEvent s -> do
                    -- Memorize last sent line for auto mode
                    modify $ \(a,b,c) -> (a,b,s)

                    m <- getMap
                    ms <- getU R_Mapper
                    let over = ms ^. mapperOverlay
                        mode = ms ^. mapperMode
                        cur = (mapCurrentId m)
                        next = mapFindAdjacentRoom cur s (mapGraph $ mgPrepareMap over m)
                    if mode == ModeOff
                        then mzero
                        else case next of
                                Nothing -> case (mode, lookup s standardExits) of
                                    (ModeManual, Just opp) ->
                                        case mapAddRoom (mapGraph m) of
                                                Nothing -> do
                                                    echo "Konnte Raum nicht erstellen."
                                                    return [ev]
                                                Just (newm, newroom) -> do
                                                    modifyMap $ mapModifyCurrentId (const newroom) .
                                                                mapModifyGraph (mapAddExit newroom opp cur "base") .
                                                                mapModifyGraph (mapAddExit cur s newroom "base") .
                                                                mapModifyGraph (const newm)
                                                    return [ev]
                                    _ -> mzero
                                Just n -> do
                                    let before = roomActionsBeforeExit m cur s
                                    (ret, tf) <- lift $ liftT $ runTriggerFlow roomOutTriggers ev

                                    modify $ \(a,b,c) -> (tf,b,c)

                                    wegFrei <- lift $ roomCheckBlockers m cur s
                                    if not (wegFrei == [])
                                       then do
                                            modifyU R_Mapper $ mapperWalkState .~ -1
                                            echo $ "BLOCKER: " ++ (show wegFrei)
                                            return []
                                       else do
                                            modifyMap $ mapModifyCurrentId $ const n
                                            return $ before ++ ret
                GMCPTEvent gmcp ->
                    case gmcpModule gmcp of
                        "MG.room.info" -> do
                            (_, _, lastline) <- get

                            m <- getMap
                            ms <- getU R_Mapper

                            let over = ms ^. mapperOverlay
                                mode = ms ^. mapperMode
                                cur = (mapCurrentId m)
                                curhash = lookupUserValue "hash" $ mapGetRoomData cur $ mapGraph m
                                newhash = fromMaybe "" $ getStringField "id" gmcp
                                newroom = mapFindRoomByIndex "hash" (UserValueString newhash) m

                            when (mode == ModeOff) mzero

                            modifyU R_Mapper $ mapperRoomHash .~ newhash

                            when (newhash /= "" && not (containsHash newhash curhash)) $
                                case mode of
                                    ModeAuto ->
                                        case newroom of
                                            Nothing -> case mapAddRoom (mapGraph m) of
                                                        Nothing ->
                                                            echoMapper "Konnte Raum nicht erstellen."
                                                        Just (newm, newroom) -> do
                                                            modifyMap $ mapModifyCurrentId (const newroom)
                                                                      . mapModifyGraph (mapModifyRoomData newroom $
                                                                            M.insert "hash" (UserValueString newhash))
                                                                      . mapModifyGraph (mapAddExit cur lastline newroom "base")
                                                                      . mapModifyGraph (const newm)
                                                            updateHashIndex
                                            Just r -> modifyMap $ mapModifyCurrentId (const r) .
                                                                  mapModifyGraph (mapAddExit cur lastline r "base")
                                    ModeUpdate ->
                                        case newroom of
                                            Nothing ->
                                                case newhash of
                                                    "" -> return ()
                                                    newhash' ->
                                                        -- Make an array of curhash if needed
                                                        let addHash h x = case x of
                                                                UserValueString v -> UserValueArray [h, UserValueString v]
                                                                UserValueArray v -> UserValueArray (h : v)
                                                                UserValueNull -> h
                                                                x -> x
                                                        in do
                                                           modifyMap $ mapModifyGraph (mapModifyRoomData cur $
                                                                       M.alter (Just . addHash (UserValueString newhash') . fromMaybe UserValueNull) "hash")
                                                           updateHashIndex
                                            Just r ->
                                                echoMapper $ "Hash " ++ newhash ++ " ist bereits belegt."
                                    _ ->
                                        maybe (echoMapper $ "Raum " ++ newhash ++ " nicht gefunden")
                                               (modifyMap . mapModifyCurrentId . const)
                                               (mapFindRoomByIndex "hash" (UserValueString newhash) m)

                            (ret, tf) <- lift $ liftT $ runTriggerFlow roomInTriggers ev
                            modify $ \(a,b,c) -> (a,tf,c)
                            return ret
                        _ -> mzero
                _ -> mzero
