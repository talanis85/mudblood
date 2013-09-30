module Mudblood.UI
    ( UIValue (UIStringValue, UIIntValue, UINilValue)
    , UIAction (..)
    -- * Widgets
    , UIWidget (..)
    ) where

import Mudblood.Keys
import Mudblood.Text
import Mudblood.Mapper.Map

data UIValue = UIStringValue String
             | UIIntValue Int
             | UINilValue

instance Show UIValue where
    show (UIStringValue s) = s
    show (UIIntValue i) = show i
    show UINilValue = "NIL"

data UIAction m = UIStatus String
                | UIUpdateMap Map
                | UIShowSidebar Bool
                | UISetValue String UIValue
                | UISetColor Color String
                | UISetBgColor String
                | UIBind [Key] (m ())

-- | Widgets are small pieces of information to be displayed by the screen.
data UIWidget = UIWidgetText String        -- ^ A singe line of text
              | UIWidgetTable [[String]]   -- ^ A table of textual cells
