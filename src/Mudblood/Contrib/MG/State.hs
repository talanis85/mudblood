{-# LANGUAGE DeriveDataTypeable #-}
module Mudblood.Contrib.MG.State 
    ( MGState (..)
    , MGStats (..)
    , modifyStats
    , modifyTanjianStats, modifyZaubererStats
    , newMGState, modifyState, getState
    , module Mudblood.Contrib.DynTrigger
    -- * Commands to modify the state
    , cmdFocus, cmdGuild
    -- * Widgets
    , mkMGStatWidgets
    , updateWidgetList
    ) where

import Data.Typeable
import Data.Dynamic

import Mudblood
import Mudblood.Contrib.DynTrigger

import Mudblood.Contrib.MG.Gilden

data MGState = MGState
    { mgStats         :: MGStats
    , mgTanjianStats  :: MGTanjianStats
    , mgZaubererStats :: MGZaubererStats
    , mgDynTriggers   :: DynTriggerTable

    , mgGuild         :: MGGuild
    , mgFocus         :: Maybe String
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
    , mgDynTriggers     = mkDynTriggerTable

    , mgGuild           = MGGuildAbenteurer
    , mgFocus           = Nothing
    }

modifyState :: (MGState -> MGState) -> Trigger a b ()
modifyState f = modifyUserData f

getState :: Trigger a b MGState
getState = getUserData

modifyStats f = modifyState (\x -> x { mgStats = (f $ mgStats x) })
modifyTanjianStats f = modifyState (\x -> x { mgTanjianStats = (f $ mgTanjianStats x) })
modifyZaubererStats f = modifyState (\x -> x { mgZaubererStats = (f $ mgZaubererStats x) })

cmdFocus :: MBCommand
cmdFocus = Command ["name"] $ do
    arg <- getStringParam 0
    case arg of
        "" -> lift $ modifyUserData $ \s -> s { mgFocus = Nothing }
        f  -> lift $ modifyUserData $ \s -> s { mgFocus = Just f }

cmdGuild :: MBCommand
cmdGuild = Command ["name"] $ do
    arg <- getStringParam 0
    case readGuild arg of
        Nothing -> fail $ "Unbekannte Gilde: " ++ arg
        Just g  -> do
                   lift $ modifyUserData $ \s -> s { mgGuild = g }
                   lift $ updateWidgetList

updateWidgetList :: MB ()
updateWidgetList = do
    guild <- getUserData >>= return . mgGuild
    modifyWidgets $ \_ ->
        [ UIWidgetText $ return "--- MorgenGrauen ---"
        ] ++ mkMGStatWidgets
          ++ case guild of
                MGGuildZauberer -> mkMGZaubererWidgets (getUserData >>= return . mgZaubererStats)
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
