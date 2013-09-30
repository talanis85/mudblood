{-# LANGUAGE TypeFamilies,TypeOperators,FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module Mudblood.Contrib.MG.Gilden.Zauberer
    ( R_Zauberer (..), MGZaubererStats (..)
    , mkMGZaubererStats
    , mgZaubererReport
    , zaubererTriggers
    , zaubererWidgets
    ) where

import Data.Has
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
    { mgZaubererStatSP    :: Int
    , mgZaubererStatSPMax :: Int
    , mgZaubererStatH     :: MGZaubererHand
    , mgZaubererStatS     :: MGZaubererSchutz
    , mgZaubererStatXH    :: Bool
    , mgZaubererStatW     :: Bool
    , mgZaubererStatB     :: Bool
    , mgZaubererStatE     :: Bool
    }

mkMGZaubererStats = MGZaubererStats
    { mgZaubererStatSP      = 0
    , mgZaubererStatSPMax   = 0
    , mgZaubererStatH       = MGZaubererHandAus
    , mgZaubererStatS       = MGZaubererSchutzAus
    , mgZaubererStatXH      = False
    , mgZaubererStatW       = False
    , mgZaubererStatB       = False
    , mgZaubererStatE       = False
    }

zaubererWidgets :: (Has R_Zauberer u) => MB u [UIWidget]
zaubererWidgets = do
    stats <- getU R_Zauberer
    let zaubtable = UIWidgetTable
            [ [ "SP:",          (show $ stats # mgZaubererStatSP) ++ " (" ++ (show $ stats # mgZaubererStatSPMax) ++ ")" ]
            , [ "Schutz:",      showSchutz $ stats # mgZaubererStatS ]
            , [ "Hand:",        showHand $ stats # mgZaubererStatH ]
            , [ "Extrahand:",   showExtrahand $ stats # mgZaubererStatXH ]
            , [ "Wille:",       showWille $ stats # mgZaubererStatW ]
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
            let statfun = \s -> s
                  { mgStatLP        = (read $ r !! 1)
                  , mgStatMLP       = (read $ r !! 2)
                  , mgStatKP        = (read $ r !! 3)
                  , mgStatMKP       = (read $ r !! 4)
                  , mgStatVO        = (read $ r !! 7)
                  , mgStatFR        = r !! 8
                  , mgStatG         = (if (r !! 9) == "J" then 1 else 0)
                  , mgStatB         = (if (r !! 10) == "J" then True else False)
                  , mgStatT         = (if (r !! 11) == "J" then True else False)
                  , mgStatF         = (if (r !! 12) == "J" then True else False)
                  }
                zaubfun = \s -> s
                  { mgZaubererStatSP    = (read $ r !! 5)
                  , mgZaubererStatSPMax = (read $ r !! 6)
                  , mgZaubererStatH     = case (r !! 13) of
                        "F" -> MGZaubererHandFeuer
                        "E" -> MGZaubererHandEis
                        "S" -> MGZaubererHandSaeure
                        _   -> MGZaubererHandAus
                  , mgZaubererStatW     = (if (r !! 14) == "W" then True else False)
                  , mgZaubererStatXH    = (if (r !! 15) == "X" then True else False)
                  , mgZaubererStatS     = case (r !! 16) of
                        "s" -> MGZaubererSchutzSchutz
                        "S" -> MGZaubererSchutzSchutzhuelle
                        _   -> MGZaubererSchutzAus
                  , mgZaubererStatB     = (if (r !! 17) == "W" then True else False)
                  , mgZaubererStatE     = (if (r !! 18) == "X" then True else False)
                  }
            in modifyUserData $ (R_Common ^: statfun) . (R_Zauberer ^: zaubfun)
        [] -> failT

zaubererTriggers :: (Has R_Common u, Has R_Zauberer u) => MBTriggerFlow u
zaubererTriggers = Permanent (gag $ withLine >>> mgZaubererReport)
