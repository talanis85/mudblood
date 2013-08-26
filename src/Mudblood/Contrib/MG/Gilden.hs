module Mudblood.Contrib.MG.Gilden
    ( module Mudblood.Contrib.MG.Gilden.Tanjian
    , module Mudblood.Contrib.MG.Gilden.Zauberer
    , MGGuild (..)
    , readGuild
    ) where

import Mudblood.Contrib.MG.Gilden.Tanjian
import Mudblood.Contrib.MG.Gilden.Zauberer

data MGGuild = MGGuildTanjian | MGGuildZauberer | MGGuildAbenteurer

readGuild :: String -> Maybe MGGuild
readGuild "abenteurer"  = Just MGGuildAbenteurer
readGuild "tanjian"     = Just MGGuildTanjian
readGuild "zauberer"    = Just MGGuildZauberer
readGuild _             = Nothing
