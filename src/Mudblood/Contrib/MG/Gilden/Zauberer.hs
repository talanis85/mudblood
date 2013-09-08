{-# LANGUAGE TemplateHaskell #-}

module Mudblood.Contrib.MG.Gilden.Zauberer
    ( MGZaubererStats (..), MGZaubererHand (..), MGZaubererSchutz (..)
    , mkMGZaubererStats
    , mkMGZaubererWidgets
    -- * Lenses
    , mgZaubererStatSP, mgZaubererStatSPMax, mgZaubererStatH, mgZaubererStatS, mgZaubererStatXH
    , mgZaubererStatW, mgZaubererStatB, mgZaubererStatE
    ) where

import Control.Lens

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
    { _mgZaubererStatSP    :: Int
    , _mgZaubererStatSPMax :: Int
    , _mgZaubererStatH     :: MGZaubererHand
    , _mgZaubererStatS     :: MGZaubererSchutz
    , _mgZaubererStatXH    :: Bool
    , _mgZaubererStatW     :: Bool
    , _mgZaubererStatB     :: Bool
    , _mgZaubererStatE     :: Bool
    }

mkMGZaubererStats = MGZaubererStats
    { _mgZaubererStatSP      = 0
    , _mgZaubererStatSPMax   = 0
    , _mgZaubererStatH       = MGZaubererHandAus
    , _mgZaubererStatS       = MGZaubererSchutzAus
    , _mgZaubererStatXH      = False
    , _mgZaubererStatW       = False
    , _mgZaubererStatB       = False
    , _mgZaubererStatE       = False
    }

makeLenses ''MGZaubererStats

mkMGZaubererWidgets :: MB MGZaubererStats -> [UIWidget]
mkMGZaubererWidgets statfun =
    [ UIWidgetText $ return "--- Zauberer ---"
    , UIWidgetTable $ do
            stats <- statfun
            return
                [ [ "SP:",          (show $ stats ^. mgZaubererStatSP) ++ " (" ++ (show $ stats ^. mgZaubererStatSPMax) ++ ")" ]
                , [ "Schutz:",      showSchutz $ stats ^. mgZaubererStatS ]
                , [ "Hand:",        showHand $ stats ^. mgZaubererStatH ]
                , [ "Extrahand:",   showExtrahand $ stats ^. mgZaubererStatXH ]
                , [ "Wille:",       showWille $ stats ^. mgZaubererStatW ]
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
