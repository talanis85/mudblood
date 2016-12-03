module Mudblood.Component.Debug
  ( regexTestC
  ) where

import Mudblood
import Mudblood.Trigger.Regex

regexTestC = commandC regexTestCmd

regexTestCmd = mkCommand "testRegex" "Tests if <regex> matches in <string>."
                 (f <$> arg stringParser "regex" "Regex to test"
                    <*> arg stringParser "string" "String to test against")
  where
    f re text =
      if re ~= text
         then lift (echo $ toAS "matches")
         else lift (echo $ toAS "does not match")
