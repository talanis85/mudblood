module Mudblood.UiValue
    ( UiValue (UiStringValue, UiIntValue, UiNilValue)
    ) where

data UiValue = UiStringValue String
             | UiIntValue Int
             | UiNilValue

instance Show UiValue where
    show (UiStringValue s) = s
    show (UiIntValue i) = show i
    show UiNilValue = "NIL"
