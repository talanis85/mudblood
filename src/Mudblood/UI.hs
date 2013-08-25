module Mudblood.UI
    ( UIValue (UIStringValue, UIIntValue, UINilValue)
    , UIAction (UIStatus, UISetValue, UIUpdateMap)
    ) where

data UIValue = UIStringValue String
             | UIIntValue Int
             | UINilValue

instance Show UIValue where
    show (UIStringValue s) = s
    show (UIIntValue i) = show i
    show UINilValue = "NIL"

data UIAction = UIStatus String
              | UIUpdateMap
              | UISetValue String UIValue
