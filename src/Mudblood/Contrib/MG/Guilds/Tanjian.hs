{-# LANGUAGE TypeFamilies,TypeOperators,FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module Mudblood.Contrib.MG.Guilds.Tanjian
    ( R_Tanjian (..), MGTanjianState (..)
    , TriState (..)
    , tanjianStateM, tanjianStateKO, tanjianStateTE, tanjianStateHA, tanjianStateAK
    , mkMGTanjianState
    , tanjianReport
    , tanjianTriggers
    , tanjianWidgets
    , tanjianSkillLevels
    , Akshara (..), aksharaTime
    ) where

import Data.Has hiding ((^.))
import Control.Lens

import Mudblood
import Mudblood.Contrib.Regex
import Mudblood.Contrib.MG.State

------------------------------------------------------------------------------

data R_Tanjian = R_Tanjian
type instance TypeOf R_Tanjian = MGTanjianState

data TriState = On
              | Off
              | Between

data Quality = Bad | Medium | Good

data Akshara = AksharaReady | AksharaBusy Int | AksharaOn Quality Int

data MGTanjianState = MGTanjianState
    { _tanjianStateM  :: TriState
    , _tanjianStateKO :: Bool
    , _tanjianStateTE :: TriState
    , _tanjianStateHA :: Bool
    , _tanjianStateAK :: Akshara
    }

mkMGTanjianState = MGTanjianState
    { _tanjianStateM  = Off
    , _tanjianStateKO = False
    , _tanjianStateTE = Off
    , _tanjianStateHA = False
    , _tanjianStateAK = AksharaReady
    }

------------------------------------------------------------------------------

tanjianStateM    :: Lens' MGTanjianState TriState
tanjianStateM    = lens _tanjianStateM $ \s v -> s { _tanjianStateM = v }

tanjianStateKO   :: Lens' MGTanjianState Bool
tanjianStateKO   = lens _tanjianStateKO $ \s v -> s { _tanjianStateKO = v }

tanjianStateTE   :: Lens' MGTanjianState TriState
tanjianStateTE   = lens _tanjianStateTE $ \s v -> s { _tanjianStateTE = v }

tanjianStateHA   :: Lens' MGTanjianState Bool
tanjianStateHA   = lens _tanjianStateHA $ \s v -> s { _tanjianStateHA = v }

tanjianStateAK   :: Lens' MGTanjianState Akshara
tanjianStateAK   = lens _tanjianStateAK $ \s v -> s { _tanjianStateAK = v }

------------------------------------------------------------------------------

aksharaTime :: Akshara -> Int -> Int
aksharaTime (AksharaOn Good t) t' = 75 - (t' - t)
aksharaTime (AksharaOn Medium t) t' = 60 - (t' - t)
aksharaTime (AksharaOn Bad t) t' = 45 - (t' - t)
aksharaTime (AksharaBusy t) t' = 150 - (t' - t)
aksharaTime _ _ = 0

------------------------------------------------------------------------------

tanjianWidgets :: (Has R_Tanjian u) => MB u [UIWidget]
tanjianWidgets = do
    stats <- getU R_Tanjian
    t <- getTime
    let tanjitable = UIWidgetTable
            [ [ "Meditation:",  showMeditation  $ stats ^. tanjianStateM ]
            , [ "Kokoro:",      showBool        $ stats ^. tanjianStateKO ]
            , [ "Tegatana:",    showTegatana    $ stats ^. tanjianStateTE ]
            , [ "Omamori:",     showOmamori     $ stats ^. tanjianStateTE ]
            , [ "Hayai:",       showBool        $ stats ^. tanjianStateHA ]
            , [ "Akshara:",     showAkshara     (stats ^. tanjianStateAK) t ]
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

    showAkshara :: Akshara -> Int -> String
    showAkshara ak@(AksharaOn _ _) t = "Ja (noch " ++ show (aksharaTime ak t) ++ "s)"
    showAkshara AksharaReady _ = "Bereit"
    showAkshara ak@(AksharaBusy _) t = "Busy (noch " ++ show (aksharaTime ak t) ++ "s)"

tanjianReport :: (Has R_Common u, Has R_Tanjian u) => AttrString -> MBTrigger u ()
tanjianReport x =
    case matchAS "^\\$REPORT\\$ ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) '(.+)' ([JN])([JN])([JN])([JN]) ([[:word:]]+) ([ -+]) ([[:word:]]+) ([[:word:]]+) ([[:word:]]+) ([JjN]) ([[:digit:]]+)" x of
        [] -> failT
        r  ->
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
                    (tanjianStateKO  .~ (if (r !! 11) == "ja" then True else False))
                  . (tanjianStateTE  .~ case (r !! 12) of
                            "+" -> On
                            "-" -> Between
                            _   -> Off
                    )
                  . (tanjianStateHA  .~ (if (r !! 13) == "ja" then True else False))
                  {- we do this without the report
                  . (tanjianStateAK  .~ case (r !! 14) of
                            "ja" -> On
                            "busy" -> Between
                            _   -> Off
                    )
                  -}
                  . (tanjianStateM   .~ case (r !! 16) of
                            "J" -> On
                            "j" -> Between
                            _   -> Off
                    )
            in modifyUserData $ (R_Common ^: statfun) . (R_Tanjian ^: tanjifun)

aksharaTimer :: (Has R_Tanjian u) => TriggerEvent -> MBTrigger u [TriggerEvent]
aksharaTimer = keep $ guardLine >=> (ak1 <||> ak2 <||> ak3 <||> akOff)
    where
        ak1 = regexAS "^Deine Haende fangen ploetzlich an, leicht zu leuchten\\." >=> (const $ aksharaSetup Bad)
        ak2 = regexAS "^Deine Haende fangen ploetzlich an, hell zu leuchten\\." >=> (const $ aksharaSetup Medium)
        ak3 = regexAS "^Deine Haende fangen ploetzlich an, sehr hell zu leuchten\\." >=> (const $ aksharaSetup Good)
        akOff = regexAS "^Du verlaesst den Pfad des Lichtes\\." >=> (const $ modifyU R_Tanjian $ tanjianStateAK .~ AksharaReady)

aksharaSetup :: (Has R_Tanjian u) => Quality -> MBTrigger u ()
aksharaSetup quality = do
    getTime >>= \t -> modifyU R_Tanjian $ tanjianStateAK .~ AksharaOn quality t

-------------------------------------------------------------------------------}

tanjianTriggers :: (Has R_Common u, Has R_Tanjian u) => MBTriggerFlow u
tanjianTriggers = Permanent aksharaTimer
             :>>: Permanent (gag $ guardLine >=> tanjianReport)

tanjianSkillLevels = [
        "aeusserst uebel",
        "sehr uebel",
        "uebel",
        "noch uebel",
        "aeusserst miserabel",
        "sehr miserabel",
        "miserabel",
        "noch miserabel",
        "aeusserst schlecht",
        "sehr schlecht",
        "reichlich schlecht",
        "ziemlich schlecht",
        "schlecht",
        "gerade noch schlecht",
        "aeusserst ungenuegend",
        "ungenuegend",
        "noch ungenuegend",
        "aeusserst mangelhaft",
        "sehr mangelhaft",
        "mangelhaft",
        "noch mangelhaft",
        "fast ausreichend",
        "ausreichend",
        "gut ausreichend",
        "fast befriedigend",
        "befriedigend",
        "sehr befriedigend",
        "fast gut",
        "gut",
        "ziemlich gut",
        "wirklich gut",
        "sehr gut",
        "aussergewoehnlich gut",
        "unglaublich gut",
        "besser als gut",
        "viel besser als gut",
        "beinahe perfekt",
        "so gut wie perfekt",
        "wirklich fast perfekt"
        ]
