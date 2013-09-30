{-# LANGUAGE TypeFamilies,TypeOperators,FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module Mudblood.Contrib.MG.Gilden.Tanjian
    ( R_Tanjian (..), MGTanjianStats (..)
    , mkMGTanjianStats
    , mgTanjianReport
    , tanjianTriggers
    , tanjianWidgets
    ) where

import Data.Has
import Control.Arrow

import Text.Regex.TDFA

import Mudblood
import Mudblood.Contrib.MG.Common
import Mudblood.Contrib.MG.State

------------------------------------------------------------------------------

data R_Tanjian = R_Tanjian
type instance TypeOf R_Tanjian = MGTanjianStats

data TriState = On
              | Off
              | Between

data MGTanjianStats = MGTanjianStats
    { mgTanjianStatM  :: TriState
    , mgTanjianStatKO :: Bool
    , mgTanjianStatTE :: TriState
    , mgTanjianStatHA :: Bool
    , mgTanjianStatAK :: TriState
    , mgTanjianStatAKQuality :: TriState
    , mgTanjianStatAKTime :: Int
    }

mkMGTanjianStats = MGTanjianStats
    { mgTanjianStatM  = Off
    , mgTanjianStatKO = False
    , mgTanjianStatTE = Off
    , mgTanjianStatHA = False
    , mgTanjianStatAK = Off
    , mgTanjianStatAKQuality = Off
    , mgTanjianStatAKTime = 0
    }

------------------------------------------------------------------------------

tanjianWidgets :: (Has R_Tanjian u) => MB u [UIWidget]
tanjianWidgets = do
    stats <- getU R_Tanjian
    t <- getTime
    let tanjitable = UIWidgetTable
            [ [ "Meditation:",  showMeditation $ stats # mgTanjianStatM ]
            , [ "Kokoro:",      showBool $ stats # mgTanjianStatKO ]
            , [ "Tegatana:",    showTegatana $ stats # mgTanjianStatTE ]
            , [ "Omamori:",     showOmamori $ stats # mgTanjianStatTE ]
            , [ "Hayai:",       showBool $ stats # mgTanjianStatHA ]
            , [ "Akshara:",     showAkshara (stats # mgTanjianStatAK)
                                            (stats # mgTanjianStatAKQuality)
                                            (t - (stats # mgTanjianStatAKTime)) ]
            ]
    return [ tanjitable ]
  where
    showBool True  = "An"
    showBool False = "Aus"

    showMeditation On      = "Ja"
    showMeditation Off     = "Nein"
    showMeditation Between = "Abklingend"

    showTegatana On = "Ja"
    showTegatana _  = "Nein"

    showOmamori Between = "Ja"
    showOmamori _       = "Nein"

    showAkshara :: TriState -> TriState -> Int -> String
    showAkshara On q t = "Ja (noch " ++ (show $ (aksharaTime q) - t) ++ "s)"
    showAkshara Off _ _ = "Nein"
    showAkshara Between _ t = "Busy (noch " ++ (show $ 150 - t) ++ "s)"

    aksharaTime :: TriState -> Int
    aksharaTime On = 75
    aksharaTime Between = 60
    aksharaTime Off = 45

mgTanjianReport :: (Has R_Common u, Has R_Tanjian u) => MBTrigger u AttrString ()
mgTanjianReport = marr $ \x ->
    case x =~ "^\\$REPORT\\$ ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) '(.+)' ([JN])([JN])([JN])([JN]) ([[:word:]]+) ([ -+]) ([[:word:]]+) ([[:word:]]+) ([[:word:]]+) ([JjN]) ([[:digit:]]+)" :: [[String]] of
        r:rs ->
            let statfun = \s -> s
                  { mgStatLP        = (read $ r !! 1)
                  , mgStatMLP       = (read $ r !! 2)
                  , mgStatKP        = (read $ r !! 3)
                  , mgStatMKP       = (read $ r !! 4)
                  , mgStatVO        = (read $ r !! 5)
                  , mgStatFR        = r !! 6
                  , mgStatG         = (if (r !! 7) == "J" then 1 else 0)
                  , mgStatB         = (if (r !! 8) == "J" then True else False)
                  , mgStatT         = (if (r !! 9) == "J" then True else False)
                  , mgStatF         = (if (r !! 10) == "J" then True else False)
                  }
                tanjifun = \s -> s
                  { mgTanjianStatKO = (if (r !! 11) == "ja" then True else False)
                  , mgTanjianStatTE  = case (r !! 12) of
                            "+" -> On
                            "-" -> Between
                            _   -> Off
                  , mgTanjianStatHA = (if (r !! 13) == "ja" then True else False)
                  , mgTanjianStatAK = case (r !! 14) of
                            "ja" -> On
                            "busy" -> Between
                            _   -> Off
                  , mgTanjianStatM = case (r !! 16) of
                            "J" -> On
                            "j" -> Between
                            _   -> Off
                  }
            in modifyUserData $ (R_Common ^: statfun) . (R_Tanjian ^: tanjifun)
        [] -> failT

mgAksharaTimeTriggers :: (Has R_Tanjian u) => MBTriggerFlow u
mgAksharaTimeTriggers = Permanent (withLine >>> ak1) :>>: Permanent (withLine >>> ak2) :>>: Permanent (withLine >>> ak3)
    where
        ak1 = proc x -> do
                regex "^Deine Haende fangen ploetzlich an, leicht zu leuchten." -< x
                aksharaSetup Off -< x
        ak2 = proc x -> do
                regex "^Deine Haende fangen ploetzlich an, hell zu leuchten." -< x
                aksharaSetup Between -< x
        ak3 = proc x -> do
                regex "^Deine Haende fangen ploetzlich an, sehr hell zu leuchten." -< x
                aksharaSetup On -< x

aksharaSetup :: (Has R_Tanjian u) => TriState -> MBTrigger u AttrString [TriggerEvent]
aksharaSetup quality = marr $ \x -> do
    getTime >>= \t -> modifyU R_Tanjian $ \x -> x { mgTanjianStatAKTime = t, mgTanjianStatAKQuality = quality }
    returnLine x

-------------------------------------------------------------------------------}

tanjianTriggers :: (Has R_Common u, Has R_Tanjian u) => MBTriggerFlow u
tanjianTriggers = mgAksharaTimeTriggers
             :>>: Permanent (gag $ withLine >>> mgTanjianReport)
