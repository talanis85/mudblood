module Mudblood.UI
    ( UIValue (UIStringValue, UIIntValue, UINilValue)
    , UIAction (..)
    , UIDialogDescription (..), UIDialogResult (..)
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

data UIAction m = UIUpdateMap Map
                | UISetValue String UIValue
                | UISetColor Color String
                | UISetBgColor String
                | UIBind [Key] (m ())
                | UIUpdateWindow String
                | UISetCompletion [String]
                | UIBell
                | UIPrompt String (String -> m ())

-- | Widgets are small pieces of information to be displayed by the screen.
data UIWidget = UIWidgetText String        -- ^ A singe line of text
              | UIWidgetTable [[String]]   -- ^ A table of textual cells
              | UIWidgetGauge Int Int Int
    deriving (Eq)

data UIDialogDescription = UITextDialogDescription String
data UIDialogResult = UITextDialogResult String
