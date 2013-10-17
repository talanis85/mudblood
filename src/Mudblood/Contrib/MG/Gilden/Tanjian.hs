{-# LANGUAGE TypeFamilies,TypeOperators,FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module Mudblood.Contrib.MG.Gilden.Tanjian
    ( R_Tanjian (..), MGTanjianStats (..)
    , mkMGTanjianStats
    , mgTanjianReport
    , tanjianTriggers
    , tanjianWidgets
    ) where

import Data.Has hiding ((^.))
import Control.Lens

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
    { _tanjianStatM  :: TriState
    , _tanjianStatKO :: Bool
    , _tanjianStatTE :: TriState
    , _tanjianStatHA :: Bool
    , _tanjianStatAK :: TriState
    , _tanjianStatAKQuality :: TriState
    , _tanjianStatAKTime :: Int
    }

mkMGTanjianStats = MGTanjianStats
    { _tanjianStatM  = Off
    , _tanjianStatKO = False
    , _tanjianStatTE = Off
    , _tanjianStatHA = False
    , _tanjianStatAK = Off
    , _tanjianStatAKQuality = Off
    , _tanjianStatAKTime = 0
    }

------------------------------------------------------------------------------

tanjianStatM    :: Lens' MGTanjianStats TriState
tanjianStatM    = lens _tanjianStatM $ \s v -> s { _tanjianStatM = v }

tanjianStatKO   :: Lens' MGTanjianStats Bool
tanjianStatKO   = lens _tanjianStatKO $ \s v -> s { _tanjianStatKO = v }

tanjianStatTE   :: Lens' MGTanjianStats TriState
tanjianStatTE   = lens _tanjianStatTE $ \s v -> s { _tanjianStatTE = v }

tanjianStatHA   :: Lens' MGTanjianStats Bool
tanjianStatHA   = lens _tanjianStatHA $ \s v -> s { _tanjianStatHA = v }

tanjianStatAK   :: Lens' MGTanjianStats TriState
tanjianStatAK   = lens _tanjianStatAK $ \s v -> s { _tanjianStatAK = v }

tanjianStatAKQuality :: Lens' MGTanjianStats TriState
tanjianStatAKQuality = lens _tanjianStatAKQuality $ \s v -> s { _tanjianStatAKQuality = v }

tanjianStatAKTime :: Lens' MGTanjianStats Int
tanjianStatAKTime = lens _tanjianStatAKTime $ \s v -> s { _tanjianStatAKTime = v }

------------------------------------------------------------------------------

tanjianWidgets :: (Has R_Tanjian u) => MB u [UIWidget]
tanjianWidgets = do
    stats <- getU R_Tanjian
    t <- getTime
    let tanjitable = UIWidgetTable
            [ [ "Meditation:",  showMeditation  $ stats ^. tanjianStatM ]
            , [ "Kokoro:",      showBool        $ stats ^. tanjianStatKO ]
            , [ "Tegatana:",    showTegatana    $ stats ^. tanjianStatTE ]
            , [ "Omamori:",     showOmamori     $ stats ^. tanjianStatTE ]
            , [ "Hayai:",       showBool        $ stats ^. tanjianStatHA ]
            , [ "Akshara:",     showAkshara (stats ^. tanjianStatAK)
                                            (stats ^. tanjianStatAKQuality)
                                            (t - (stats ^. tanjianStatAKTime)) ]
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
            let statfun =
                    (mgStatLP        .~ (read $ r !! 1))
                  . (mgStatMLP       .~ (read $ r !! 2))
                  . (mgStatKP        .~ (read $ r !! 3))
                  . (mgStatMKP       .~ (read $ r !! 4))
                  . (mgStatVO        .~ (read $ r !! 5))
                  . (mgStatFR        .~ r !! 6)
                  . (mgStatG         .~ (if (r !! 7) == "J" then 1 else 0))
                  . (mgStatB         .~ (if (r !! 8) == "J" then True else False))
                  . (mgStatT         .~ (if (r !! 9) == "J" then True else False))
                  . (mgStatF         .~ (if (r !! 10) == "J" then True else False))
                tanjifun =
                    (tanjianStatKO  .~ (if (r !! 11) == "ja" then True else False))
                  . (tanjianStatTE  .~ case (r !! 12) of
                            "+" -> On
                            "-" -> Between
                            _   -> Off
                    )
                  . (tanjianStatHA  .~ (if (r !! 13) == "ja" then True else False))
                  . (tanjianStatAK  .~ case (r !! 14) of
                            "ja" -> On
                            "busy" -> Between
                            _   -> Off
                    )
                  . (tanjianStatM   .~ case (r !! 16) of
                            "J" -> On
                            "j" -> Between
                            _   -> Off
                    )
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
    getTime >>= \t -> modifyU R_Tanjian $ (tanjianStatAKTime .~ t) . (tanjianStatAKQuality .~ quality)
    returnLine x

-------------------------------------------------------------------------------}

tanjianTriggers :: (Has R_Common u, Has R_Tanjian u) => MBTriggerFlow u
tanjianTriggers = mgAksharaTimeTriggers
             :>>: Permanent (gag $ withLine >>> mgTanjianReport)
