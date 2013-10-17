{-# LANGUAGE TypeFamilies,TypeOperators,FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module Mudblood.Contrib.MG.Gilden.Zauberer
    ( R_Zauberer (..), MGZaubererStats (..)
    , mkMGZaubererStats
    , mgZaubererReport
    , zaubererTriggers
    , zaubererWidgets
    ) where

import Data.Has hiding ((^.))
import Control.Lens
import Control.Arrow

import Text.Regex.TDFA

import Mudblood
import Mudblood.Contrib.MG.Common
import Mudblood.Contrib.MG.State

------------------------------------------------------------------------------

data R_Zauberer = R_Zauberer
type instance TypeOf R_Zauberer = MGZaubererStats

data MGZaubererHand = MGZaubererHandAus
                    | MGZaubererHandNormal
                    | MGZaubererHandFeuer
                    | MGZaubererHandEis
                    | MGZaubererHandSaeure

data MGZaubererSchutz = MGZaubererSchutzAus
                      | MGZaubererSchutzSchutz
                      | MGZaubererSchutzSchutzhuelle

data MGZaubererStats = MGZaubererStats
    { _zaubererStatSP    :: Int
    , _zaubererStatSPMax :: Int
    , _zaubererStatH     :: MGZaubererHand
    , _zaubererStatS     :: MGZaubererSchutz
    , _zaubererStatXH    :: Bool
    , _zaubererStatW     :: Bool
    , _zaubererStatB     :: Bool
    , _zaubererStatE     :: Bool
    }

mkMGZaubererStats = MGZaubererStats
    { _zaubererStatSP      = 0
    , _zaubererStatSPMax   = 0
    , _zaubererStatH       = MGZaubererHandAus
    , _zaubererStatS       = MGZaubererSchutzAus
    , _zaubererStatXH      = False
    , _zaubererStatW       = False
    , _zaubererStatB       = False
    , _zaubererStatE       = False
    }

------------------------------------------------------------------------------

zaubererStatSP      :: Lens' MGZaubererStats Int
zaubererStatSP      = lens _zaubererStatSP      $ \s v -> s { _zaubererStatSP = v }

zaubererStatSPMax   :: Lens' MGZaubererStats Int
zaubererStatSPMax   = lens _zaubererStatSPMax   $ \s v -> s { _zaubererStatSPMax = v }

zaubererStatH       :: Lens' MGZaubererStats MGZaubererHand
zaubererStatH       = lens _zaubererStatH       $ \s v -> s { _zaubererStatH = v }

zaubererStatS       :: Lens' MGZaubererStats MGZaubererSchutz
zaubererStatS       = lens _zaubererStatS       $ \s v -> s { _zaubererStatS = v }

zaubererStatXH      :: Lens' MGZaubererStats Bool
zaubererStatXH      = lens _zaubererStatXH      $ \s v -> s { _zaubererStatXH = v }

zaubererStatW       :: Lens' MGZaubererStats Bool
zaubererStatW       = lens _zaubererStatW       $ \s v -> s { _zaubererStatW = v }

zaubererStatB       :: Lens' MGZaubererStats Bool
zaubererStatB       = lens _zaubererStatB       $ \s v -> s { _zaubererStatB = v }

zaubererStatE       :: Lens' MGZaubererStats Bool
zaubererStatE       = lens _zaubererStatE       $ \s v -> s { _zaubererStatE = v }

------------------------------------------------------------------------------

zaubererWidgets :: (Has R_Zauberer u) => MB u [UIWidget]
zaubererWidgets = do
    stats <- getU R_Zauberer
    let zaubtable = UIWidgetTable
            [ [ "SP:",          (show $ stats ^. zaubererStatSP) ++ " (" ++ (show $ stats ^. zaubererStatSPMax) ++ ")" ]
            , [ "Schutz:",      showSchutz      $ stats ^. zaubererStatS ]
            , [ "Hand:",        showHand        $ stats ^. zaubererStatH ]
            , [ "Extrahand:",   showExtrahand   $ stats ^. zaubererStatXH ]
            , [ "Wille:",       showWille       $ stats ^. zaubererStatW ]
            ]
    return [ zaubtable ]
  where
    showSchutz MGZaubererSchutzAus          = "Aus"
    showSchutz MGZaubererSchutzSchutz       = "Mechanisch"
    showSchutz MGZaubererSchutzSchutzhuelle = "Magisch"
    
    showHand MGZaubererHandAus    = "Aus"
    showHand MGZaubererHandNormal = "Feuer (schwach)"
    showHand MGZaubererHandFeuer  = "Feuer"
    showHand MGZaubererHandEis    = "Eis"
    showHand MGZaubererHandSaeure = "Saeure"

    showExtrahand True  = "An"
    showExtrahand False = "Aus"

    showWille True  = "An"
    showWille False = "Aus"

mgZaubererReport :: (Has R_Common u, Has R_Zauberer u) => MBTrigger u AttrString ()
mgZaubererReport = marr $ \x ->
    --case x =~ "^\\$REPORT\\$ (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) '(.+)' ([JN])([JN])([JN])([JN]) ([FES ]) ([W ]) ([X ]) ([sSVZ ]) ([B ]) ([E ]) (\\w+)" :: [[String]] of
    case x =~ "^\\$REPORT\\$ ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) '(.+)' ([JN])([JN])([JN])([JN]) ([FES ]) ([W ]) ([X ]) ([sSVZ ]) ([B ]) ([E ]) ([[:word:]]+)" :: [[String]] of
        r:rs ->
            let statfun =
                    (mgStatLP        .~ (read $ r !! 1))
                  . (mgStatMLP       .~ (read $ r !! 2))
                  . (mgStatKP        .~ (read $ r !! 3))
                  . (mgStatMKP       .~ (read $ r !! 4))
                  . (mgStatVO        .~ (read $ r !! 7))
                  . (mgStatFR        .~ r !! 8)
                  . (mgStatG         .~ (if (r !! 9) == "J" then 1 else 0))
                  . (mgStatB         .~ (if (r !! 10) == "J" then True else False))
                  . (mgStatT         .~ (if (r !! 11) == "J" then True else False))
                  . (mgStatF         .~ (if (r !! 12) == "J" then True else False))
                zaubfun =
                    (zaubererStatSP     .~ (read $ r !! 5))
                  . (zaubererStatSPMax  .~ (read $ r !! 6))
                  . (zaubererStatH      .~ case (r !! 13) of
                        "F" -> MGZaubererHandFeuer
                        "E" -> MGZaubererHandEis
                        "S" -> MGZaubererHandSaeure
                        _   -> MGZaubererHandAus
                    )
                  . (zaubererStatW      .~ (if (r !! 14) == "W" then True else False))
                  . (zaubererStatXH     .~ (if (r !! 15) == "X" then True else False))
                  . (zaubererStatS      .~ case (r !! 16) of
                        "s" -> MGZaubererSchutzSchutz
                        "S" -> MGZaubererSchutzSchutzhuelle
                        _   -> MGZaubererSchutzAus
                    )
                  . (zaubererStatB      .~ (if (r !! 17) == "W" then True else False))
                  . (zaubererStatE      .~ (if (r !! 18) == "X" then True else False))
            in modifyUserData $ (R_Common ^: statfun) . (R_Zauberer ^: zaubfun)
        [] -> failT

zaubererTriggers :: (Has R_Common u, Has R_Zauberer u) => MBTriggerFlow u
zaubererTriggers = Permanent (gag $ withLine >>> mgZaubererReport)
