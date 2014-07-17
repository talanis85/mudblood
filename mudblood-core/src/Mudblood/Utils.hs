module Mudblood.Utils
    ( splitLinesWithPrompt
    ) where

import Data.List.Split

splitLinesWithPrompt :: String -> String -> ([String], String)
splitLinesWithPrompt oldprompt input =
    case strictLines input of
        (l:[]) -> ([], oldprompt ++ l)
        (l:ls) -> ((oldprompt ++ l) : (init ls), last ls)
  where
    strictLines str = splitWhen (== '\n') str
