{-# LANGUAGE FlexibleContexts #-}

module Mudblood.Contrib.MG.Commands
    ( mgCommands
    , mapperCommands
    ) where

import Data.Has (Has)
import Data.Maybe
import Control.Lens

import Mudblood
import Mudblood.Contrib.MG.Common
import Mudblood.Contrib.MG.State
import Mudblood.Contrib.MG.Mapper

mgCommands :: (Has R_Common u, Has R_Mapper u) => [(String, Exp (MB u) Value)]
mgCommands =
    [ ("focus", cmdFocus)
    , ("walk", cmdWalk)
    ]

cmdFocus :: (Has R_Common u) => Exp (MB u) Value
cmdFocus =
    Function ["..."] $ do
        args <- getSymbol "..." >>= typeList
        case args of
            [] -> liftL (getU R_Common) >>= return . mkStringValue . fromMaybe "" . (^. mgFocus)
            (x:[]) -> do
                f <- typeString x
                liftL $ modifyU R_Common $ mgFocus .~ (if f == "" then Nothing else Just f)
                return $ mkStringValue f

cmdWalk :: (Has R_Mapper u) => Exp (MB u) Value
cmdWalk =
    Function ["target"] $ do
        target <- getSymbol "target"
        case target of
            Value (IntValue x)    -> liftL $ mgWalk x
            Value (StringValue x) -> do
                r <- liftL $ mgFindRoom x
                case r of
                    Nothing -> throwError "Room not found"
                    Just r -> liftL $ mgWalk r
        return nil

mapperCommands :: [(String, Exp (MB u) Value)]
mapperCommands =
    [ ("map.findTag", cmdMapFindTag)
    , ("map.findHash", cmdMapFindHash)
    , ("map.load", cmdMapLoad)
    , ("map.fly", cmdMapFly)
    , ("room.setHash", cmdRoomSetHash)
    , ("room.current", cmdRoomCurrent)
    ]

cmdMapFindTag :: Exp (MB u) Value
cmdMapFindTag =
    Function ["tag"] $ do
        tag <- getSymbol "tag" >>= typeString
        map <- liftL $ getMap
        case findRoomsBy (\x -> getUserValue "tag" (roomUserData x) == tag) map of
            [] -> return nil
            (destroom:_) -> return $ mkIntValue destroom

cmdMapFindHash :: Exp (MB u) Value
cmdMapFindHash =
    Function ["hash"] $ do
        tag <- getSymbol "hash" >>= typeString
        map <- liftL $ getMap
        case findRoomsBy (\x -> getUserValue "hash" (roomUserData x) == tag) map of
            [] -> return nil
            (destroom:_) -> return $ mkIntValue destroom

cmdMapLoad :: Exp (MB u) Value
cmdMapLoad =
    Function ["filename"] $ do
        filename <- getSymbol "filename" >>= typeString
        map <- liftL $ io $ mapFromFile filename
        case map of
            Just map' -> do
                         liftL $ putMap map'
            Nothing -> throwError "Error loading map"
        return nil

cmdMapFly :: Exp (MB u) Value
cmdMapFly =
    Function ["room"] $ do
        room <- getSymbol "room" >>= typeInt
        liftL $ modifyMap $ mapSetCurrent room
        return nil

cmdRoomSetHash :: Exp (MB u) Value
cmdRoomSetHash =
    Function ["room", "hash"] $ do
        room <- getSymbol "room" >>= typeInt
        hash <- getSymbol "hash" >>= typeString
        liftL $ modifyMap $ mapModifyRoomData room (roomModifyUserData $ putUserValue "hash" hash)
        return nil

cmdRoomCurrent :: Exp (MB u) Value
cmdRoomCurrent =
    Function [] $ do
        m <- liftL getMap
        return $ Value $ IntValue (mapCurrentId m)
