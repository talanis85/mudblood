module Mudblood.Contrib.MG.Gilden.Zauberer
    ( MGZaubererStats (..), MGZaubererHand (..), MGZaubererSchutz (..)
    , mkMGZaubererStats
    , mkMGZaubererWidgets
    ) where

import Mudblood

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

mkMGZaubererWidgets :: MB MGZaubererStats -> [UIWidget]
mkMGZaubererWidgets statfun =
    [ UIWidgetText $ return "--- Zauberer ---"
    , UIWidgetTable $ do
            stats <- statfun
            return
                [ [ "SP:", (show $ mgZaubererStatSP stats) ++ " (" ++ (show $ mgZaubererStatSPMax stats) ++ ")" ]
                , [ "Schutz:", showSchutz $ mgZaubererStatS stats ]
                , [ "Hand:", showHand $ mgZaubererStatH stats ]
                , [ "Extrahand:", showExtrahand $ mgZaubererStatXH stats ]
                , [ "Wille:", showWille $ mgZaubererStatW stats ]
                ]
    ]
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
