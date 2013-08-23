module Mudblood.Contrib.MG.Gilden.Tanjian
    ( MGTanjianStats (..)
    , TriState (..)
    , mkMGTanjianStats
    ) where

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
