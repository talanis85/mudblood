{-# LANGUAGE DeriveDataTypeable #-}
module Mudblood.Contrib.MG.State 
    ( MGState (..)
    , MGStats (..)
    , modifyStats
    , modifyTanjianStats, modifyZaubererStats
    , newMGState, modifyState, getState
    -- * Modify the state
    , setFocus, setGuild
    -- * Widgets
    , mkMGStatWidgets
    , updateWidgetList
    ) where

import Prelude hiding (error)

import Data.Typeable
import Data.Dynamic
import qualified Data.Map as M

import Mudblood

import Mudblood.Contrib.MG.Gilden
import Mudblood.Contrib.MG.Settings

data MGState = MGState
    { mgStats         :: MGStats
    , mgTanjianStats  :: MGTanjianStats
    , mgZaubererStats :: MGZaubererStats

    , mgGuild         :: MGGuild
    , mgFocus         :: Maybe String

    , mgSettings      :: MGSettings

    , mgProfile       :: String
    }
  deriving (Typeable)

data MGStats = MGStats
    { mgStatLP      :: Int
    , mgStatMLP     :: Int
    , mgStatKP      :: Int
    , mgStatMKP     :: Int
    , mgStatVO      :: Int
    , mgStatFR      :: String
    , mgStatG       :: Int
    , mgStatB       :: Bool
    , mgStatT       :: Bool
    , mgStatF       :: Bool
    }

newMGState = MGState
    { mgStats = MGStats
        { mgStatLP      = 0
        , mgStatMLP     = 0
        , mgStatKP      = 0
        , mgStatMKP     = 0
        , mgStatVO      = 0
        , mgStatFR      = ""
        , mgStatG       = 0
        , mgStatB       = False
        , mgStatT       = False
        , mgStatF       = False
        }
    , mgTanjianStats    = mkMGTanjianStats
    , mgZaubererStats   = mkMGZaubererStats

    , mgGuild           = MGGuildAbenteurer
    , mgFocus           = Nothing

    , mgSettings        = mkMGSettings

    , mgProfile         = ""
    }

modifyState f = modifyUserData f

getState :: (MBMonad m) => m MGState
getState = getUserData

modifyStats f = modifyState (\x -> x { mgStats = (f $ mgStats x) })
modifyTanjianStats f = modifyState (\x -> x { mgTanjianStats = (f $ mgTanjianStats x) })
modifyZaubererStats f = modifyState (\x -> x { mgZaubererStats = (f $ mgZaubererStats x) })

setFocus :: String -> MB ()
setFocus arg = case arg of
    "" -> modifyUserData $ \s -> s { mgFocus = Nothing }
    f  -> modifyUserData $ \s -> s { mgFocus = Just f }

setGuild :: String -> MB ()
setGuild arg = case readGuild arg of
    Nothing -> error $ "Unbekannte Gilde: " ++ arg
    Just g  -> do
               modifyUserData $ \s -> s { mgGuild = g }
               updateWidgetList

updateWidgetList :: MB ()
updateWidgetList = do
    guild <- getUserData >>= return . mgGuild
    modifyWidgets $ \_ ->
        [ UIWidgetText $ return "--- MorgenGrauen ---"
        ] ++ mkMGStatWidgets
          ++ mkMGMapWidgets
          ++ case guild of
                MGGuildZauberer -> mkMGZaubererWidgets (getUserData >>= return . mgZaubererStats)
                MGGuildTanjian -> mkMGTanjianWidgets (getUserData >>= return . mgTanjianStats)
                _ -> []

mkMGStatWidgets =
    [ UIWidgetTable $ do
        stats <- getUserData >>= return . mgStats
        return
            [ [ "LP:", (show $ mgStatLP stats) ++ " (" ++ (show $ mgStatMLP stats) ++ ")" ]
            , [ "KP:", (show $ mgStatKP stats) ++ " (" ++ (show $ mgStatMKP stats) ++ ")" ]
            , [ "Vorsicht:", (show $ mgStatVO stats) ]
            , [ "Fluchtrichtung:", (show $ mgStatFR stats) ]
            , [ "Gift:", (showGift $ mgStatG stats) ]
            , [ "Blind:", (showBool $ mgStatB stats) ]
            , [ "Taub:", (showBool $ mgStatT stats) ]
            , [ "Frosch:", (showBool $ mgStatF stats) ]
            ]
    ]
  where
    showGift 0 = "Nein"
    showGift _ = "Ja"

    showBool True = "Ja"
    showBool False = "Nein"

mkMGMapWidgets =
    [ UIWidgetText $ return "--- Mapper ---"
    , UIWidgetTable $ do
        map <- getMap
        let (id, room) = (mapCurrentId map, mapCurrentData map)
        return
            [ [ "Raum #:", (show $ id) ]
            , [ "Tag:", (getUserValue "tag" $ roomUserData room) ]
            , [ "Hash:", (getUserValue "hash" $ roomUserData room) ]
            ]
    ]
