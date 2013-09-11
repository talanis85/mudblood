module Mudblood.UI
    ( UIValue (UIStringValue, UIIntValue, UINilValue)
    , UIAction (..)
    -- * Widgets
    , UIWidget (..)
    ) where

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
                | UISetValue String UIValue
                | UIUpdateWidgets [UIWidget m]
                | UISetColor Color String
                | UISetBgColor String

-- | Widgets are small pieces of information to be displayed by the screen.
data UIWidget m = UIWidgetText (m String)        -- ^ A singe line of text
                | UIWidgetTable (m [[String]])   -- ^ A table of textual cells
