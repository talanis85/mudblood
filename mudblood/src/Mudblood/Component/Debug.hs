module Mudblood.Component.Debug
  ( regexTestC
  ) where

import Mudblood
import Mudblood.Trigger.Regex

regexTestC = commandC "testRegex" "<regex> <string>" "Tests if <regex> matches in <string>." $ do
    re <- getStringArg 0
    text <- getStringArg 1
    if re ~= text
        then lift (echo $ toAS "matches")
        else lift (echo $ toAS "does not match")
