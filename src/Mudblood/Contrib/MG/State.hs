{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Mudblood.Contrib.MG.State 
    ( MGState (..), mkMGState
    , MGStats (..)
    , MGChar (..)
    -- * Lenses
    , getU, setU, updateMaybeU
    , mgChar, mgStats, mgTanjianStats, mgZaubererStats, mgGuild, mgFocus, mgSettings, mgProfile
    , mgCharName, mgCharRace, mgCharPresay, mgCharTitle, mgCharWizlevel, mgCharLevel, mgCharGuildLevel, mgCharGuildTitle
    , mgStatLP, mgStatMLP, mgStatKP, mgStatMKP, mgStatVO, mgStatFR, mgStatG, mgStatB, mgStatT, mgStatF
    , (^.)
    -- * Modify the state
    , setFocus, setGuild
    -- * Widgets
    , mkMGStatWidgets
    , updateWidgetList
    ) where

import Control.Lens

import Data.Typeable
import Data.Dynamic
import qualified Data.Map as M

import Mudblood

import Mudblood.Contrib.MG.Gilden
import Mudblood.Contrib.MG.Settings

data MGState = MGState
    { _mgChar          :: MGChar
    , _mgStats         :: MGStats
    , _mgTanjianStats  :: MGTanjianStats
    , _mgZaubererStats :: MGZaubererStats

    , _mgGuild         :: MGGuild
    , _mgFocus         :: Maybe String

    , _mgSettings      :: MGSettings

    , _mgProfile       :: String
    }
  deriving (Typeable)

mkMGState = MGState
    { _mgChar            = mkMGChar
    , _mgStats           = mkMGStats
    , _mgTanjianStats    = mkMGTanjianStats
    , _mgZaubererStats   = mkMGZaubererStats

    , _mgGuild           = MGGuildAbenteurer
    , _mgFocus           = Nothing

    , _mgSettings        = mkMGSettings

    , _mgProfile         = ""
    }

data MGChar = MGChar
    { _mgCharName        :: String
    , _mgCharRace        :: String
    , _mgCharPresay      :: String
    , _mgCharTitle       :: String
    , _mgCharWizlevel    :: Int
    , _mgCharLevel       :: Int
    , _mgCharGuildLevel  :: Int
    , _mgCharGuildTitle  :: String
    }

mkMGChar = MGChar
    { _mgCharName        = "Jemand"
    , _mgCharRace        = "Etwas"
    , _mgCharPresay      = ""
    , _mgCharTitle       = ""
    , _mgCharWizlevel    = 0
    , _mgCharLevel       = 0
    , _mgCharGuildLevel  = 0
    , _mgCharGuildTitle  = ""
    }

data MGStats = MGStats
    { _mgStatLP      :: Int
    , _mgStatMLP     :: Int
    , _mgStatKP      :: Int
    , _mgStatMKP     :: Int
    , _mgStatVO      :: Int
    , _mgStatFR      :: String
    , _mgStatG       :: Int
    , _mgStatB       :: Bool
    , _mgStatT       :: Bool
    , _mgStatF       :: Bool
    }

mkMGStats = MGStats
    { _mgStatLP      = 0
    , _mgStatMLP     = 0
    , _mgStatKP      = 0
    , _mgStatMKP     = 0
    , _mgStatVO      = 0
    , _mgStatFR      = ""
    , _mgStatG       = 0
    , _mgStatB       = False
    , _mgStatT       = False
    , _mgStatF       = False
    }

makeLenses ''MGStats
makeLenses ''MGChar
makeLenses ''MGState

{-
modifyState f = modifyUserData f

getState :: (MBMonad m) => m MGState
getState = getUserData

modifyChar f = modifyState (\x -> x { mgChar = (f $ mgChar x) })
modifyStats f = modifyState (\x -> x { mgStats = (f $ mgStats x) })
modifyTanjianStats f = modifyState (\x -> x { mgTanjianStats = (f $ mgTanjianStats x) })
modifyZaubererStats f = modifyState (\x -> x { mgZaubererStats = (f $ mgZaubererStats x) })
-}

getU a = getUserData >>= (return . (^. a))
setU a v = modifyUserData $ set a v

updateMaybeU a v = case v of
    Nothing -> return ()
    Just v' -> setU a v'

setFocus :: String -> MB ()
setFocus arg = case arg of
    "" -> setU mgFocus Nothing
    f  -> setU mgFocus (Just "hallo")

setGuild :: String -> MB ()
setGuild arg = case readGuild arg of
    Nothing -> mbError $ "Unbekannte Gilde: " ++ arg
    Just g  -> do
               setU mgGuild g
               updateWidgetList

updateWidgetList :: MB ()
updateWidgetList = do
    guild <- getU mgGuild
    modifyWidgets $ \_ ->
        [ UIWidgetText $ return "--- MorgenGrauen ---"
        ] ++ mkMGCharWidgets
          ++ mkMGStatWidgets
          ++ mkMGMapWidgets
          ++ case guild of
                MGGuildZauberer -> mkMGZaubererWidgets $ getU mgZaubererStats
                MGGuildTanjian -> mkMGTanjianWidgets $ getU mgTanjianStats
                _ -> []

mkMGCharWidgets =
    [ UIWidgetTable $ do
        char <- getU mgChar
        return
            [ [ "Name:", (char ^. mgCharName) ]
            , [ "Rasse:", (char ^. mgCharRace) ]
            , [ "Level:", (show $ char ^. mgCharLevel) ]
            ]
    ]

mkMGStatWidgets =
    [ UIWidgetTable $ do
        stats <- getU mgStats
        return
            [ [ "LP:", (show $ stats ^. mgStatLP) ++ " (" ++ (show $ stats ^. mgStatMLP) ++ ")" ]
            , [ "KP:", (show $ stats ^. mgStatKP) ++ " (" ++ (show $ stats ^. mgStatMKP) ++ ")" ]
            , [ "Vorsicht:", (show $ stats ^. mgStatVO) ]
            , [ "Fluchtrichtung:", (show $ stats ^. mgStatFR) ]
            , [ "Gift:", (showGift $ stats ^. mgStatG) ]
            , [ "Blind:", (showBool $ stats ^. mgStatB) ]
            , [ "Taub:", (showBool $ stats ^. mgStatT) ]
            , [ "Frosch:", (showBool $ stats ^. mgStatF) ]
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
