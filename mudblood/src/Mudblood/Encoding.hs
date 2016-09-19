module Mudblood.Encoding
  ( decode
  ) where

import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.String.Utils
import           Data.Word

import           Mudblood.Text

splitLinesWithPrompt :: (Eq a) => a -> [a] -> [a] -> ([[a]], [a])
splitLinesWithPrompt nl oldprompt input =
    case strictLines input of
        (l:[]) -> ([], oldprompt ++ l)
        (l:ls) -> ((oldprompt ++ l) : (init ls), last ls)
  where
    strictLines str = splitWhen (== nl) str

decode :: [Word8] -> [Word8] -> Attr -> ([AttrString], [Word8], Attr)
decode oldprompt input oldattr =
    let (ls, newprompt)   = splitLinesWithPrompt 10 oldprompt input
        (attrls, newattr) = foldl decodeFold ([], oldattr) ls
    in (attrls, newprompt, newattr)
  where decodeFold (l, a) cur =
          let (next, a') = case decodeAS cur a of
                Nothing      -> ((toAS $ "Error decoding ANSI: " ++ escapeAll cur), a)
                Just (s, a') -> (s, a')
          in (l ++ [next], a')
