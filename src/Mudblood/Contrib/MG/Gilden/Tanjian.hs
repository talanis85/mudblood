module Mudblood.Contrib.MG.Gilden.Tanjian
    ( MGTanjianStats (..)
    , TriState (..)
    , mkMGTanjianStats
    , mkMGTanjianWidgets
    ) where

import Mudblood

data TriState = On
              | Off
              | Between

data MGTanjianStats = MGTanjianStats
    { mgTanjianStatM  :: TriState
    , mgTanjianStatKO :: Bool
    , mgTanjianStatTE :: TriState
    , mgTanjianStatHA :: Bool
    , mgTanjianStatAK :: TriState
    }

mkMGTanjianStats = MGTanjianStats
    { mgTanjianStatM  = Off
    , mgTanjianStatKO = False
    , mgTanjianStatTE = Off
    , mgTanjianStatHA = False
    , mgTanjianStatAK = Off
    }

mkMGTanjianWidgets :: MB MGTanjianStats -> [UIWidget]
mkMGTanjianWidgets statfun =
    [ UIWidgetText $ return "--- Tanjian ---"
    , UIWidgetTable $ do
            stats <- statfun
            return
                [ [ "Meditation:", showMeditation $ mgTanjianStatM stats ]
                , [ "Kokoro:", showBool $ mgTanjianStatKO stats ]
                , [ "Tegatana:", showTegatana $ mgTanjianStatTE stats ]
                , [ "Omamori:", showOmamori $ mgTanjianStatTE stats ]
                , [ "Hayai:", showBool $ mgTanjianStatHA stats ]
                , [ "Akshara:", showAkshara $ mgTanjianStatAK stats ]
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
