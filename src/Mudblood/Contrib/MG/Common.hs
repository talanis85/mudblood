{-# LANGUAGE Arrows #-}

module Mudblood.Contrib.MG.Common
    ( (#), keep, gag
    , regex, withLine, withGMCP, withTelneg
    ) where

import Control.Arrow
import Text.Regex.TDFA
import Mudblood
import Mudblood.Telnet
import Data.GMCP

------------------------------------------------------------------------------

(#) :: a -> (a -> b) -> b
a # f = f a

infix 1 #

------------------------------------------------------------------------------

keep :: MBTrigger u a b -> MBTrigger u a [a]
keep a = (returnA &&& a) >>> (arr $ \x -> [fst x])

gag :: MBTrigger u a b -> MBTrigger u a [a]
gag a = (returnA &&& a) >>> (arr $ const [])

withLine :: MBTrigger u TriggerEvent AttrString
withLine = marr $ guardLine

withGMCP :: MBTrigger u TriggerEvent GMCP
withGMCP = marr $ \ev ->
    case ev of
        GMCPTEvent gmcp -> return gmcp
        _ -> failT

withTelneg :: MBTrigger u TriggerEvent TelnetNeg
withTelneg = marr $ \ev ->
    case ev of
        TelnetTEvent t -> return t
        _ -> failT

regex pat = marr $ \s -> guardT (s =~ pat :: Bool) >> return s
