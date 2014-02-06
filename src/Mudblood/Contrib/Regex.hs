module Mudblood.Contrib.Regex
    ( compileRegex, compileRegex', execRegex, execRegex'
    , match, match', matchAS, matchAS'
    , regex, regexAS
    ) where

import Mudblood

import qualified Data.Array as Array
import qualified Text.Regex.TDFA as Regex
import qualified Text.Regex.TDFA.String as RegexString

compileRegex = RegexString.compile Regex.defaultCompOpt Regex.defaultExecOpt
compileRegex' = either (const Nothing) Just . compileRegex

execRegex r s =
    case RegexString.execute r s of
        Left _ -> []
        Right Nothing -> []
        Right (Just arr) -> map matches $ Array.elems arr
  where matches (off, len) = Regex.extract (off, len) s

execRegex' r s = not $ null $ execRegex r s

match r = case compileRegex' r of
    Nothing -> const []
    Just re -> \s -> execRegex re s

match' r = not . null . match r

matchAS r = match r . fromAS
matchAS' r = match' r . fromAS

regex pat = \s -> case match pat s of
    [] -> failT
    (whole:groups) -> return (whole, groups)

regexAS pat = \s -> regex pat $ fromAS s
