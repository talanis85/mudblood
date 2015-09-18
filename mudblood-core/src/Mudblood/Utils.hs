module Mudblood.Utils
    ( splitLinesWithPrompt
    ) where

import Data.List.Split

splitLinesWithPrompt :: (Eq a) => a -> [a] -> [a] -> ([[a]], [a])
splitLinesWithPrompt nl oldprompt input =
    case strictLines input of
        (l:[]) -> ([], oldprompt ++ l)
        (l:ls) -> ((oldprompt ++ l) : (init ls), last ls)
  where
    strictLines str = splitWhen (== nl) str
