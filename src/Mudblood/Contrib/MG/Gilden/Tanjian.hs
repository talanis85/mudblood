{-# LANGUAGE TemplateHaskell #-}

module Mudblood.Contrib.MG.Gilden.Tanjian
    ( MGTanjianStats (..)
    , TriState (..)
    , mkMGTanjianStats
    , mkMGTanjianWidgets
    -- * Lenses
    , mgTanjianStatM, mgTanjianStatKO, mgTanjianStatTE, mgTanjianStatHA, mgTanjianStatAK
    ) where

import Control.Lens

import Mudblood

data TriState = On
              | Off
              | Between

data MGTanjianStats = MGTanjianStats
    { _mgTanjianStatM  :: TriState
    , _mgTanjianStatKO :: Bool
    , _mgTanjianStatTE :: TriState
    , _mgTanjianStatHA :: Bool
    , _mgTanjianStatAK :: TriState
    }

mkMGTanjianStats = MGTanjianStats
    { _mgTanjianStatM  = Off
    , _mgTanjianStatKO = False
    , _mgTanjianStatTE = Off
    , _mgTanjianStatHA = False
    , _mgTanjianStatAK = Off
    }

makeLenses ''MGTanjianStats

mkMGTanjianWidgets :: MB MGTanjianStats -> [UIWidget]
mkMGTanjianWidgets statfun =
    [ UIWidgetText $ return "--- Tanjian ---"
    , UIWidgetTable $ do
            stats <- statfun
            return
                [ [ "Meditation:",  showMeditation $ stats ^. mgTanjianStatM ]
                , [ "Kokoro:",      showBool $ stats ^. mgTanjianStatKO ]
                , [ "Tegatana:",    showTegatana $ stats ^. mgTanjianStatTE ]
                , [ "Omamori:",     showOmamori $ stats ^. mgTanjianStatTE ]
                , [ "Hayai:",       showBool $ stats ^. mgTanjianStatHA ]
                , [ "Akshara:",     showAkshara $ stats ^. mgTanjianStatAK ]
                ]
    ]
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

    showAkshara On = "Ja"
    showAkshara Off = "Nein"
    showAkshara Between = "Busy"
