module Mudblood.Contrib.MG.Gilden.Zauberer
    ( MGZaubererStats (..), MGZaubererHand (..), MGZaubererSchutz (..)
    , mkMGZaubererStats
    ) where

data MGZaubererHand = MGZaubererHandAus
                    | MGZaubererHandNormal
                    | MGZaubererHandFeuer
                    | MGZaubererHandEis
                    | MGZaubererHandSaeure

data MGZaubererSchutz = MGZaubererSchutzAus
                      | MGZaubererSchutzSchutz
                      | MGZaubererSchutzSchutzhuelle

data MGZaubererStats = MGZaubererStats
    { mgZaubererStatSP    :: Int
    , mgZaubererStatSPMax :: Int
    , mgZaubererStatH     :: MGZaubererHand
    , mgZaubererStatS     :: MGZaubererSchutz
    , mgZaubererStatXH    :: Bool
    , mgZaubererStatW     :: Bool
    , mgZaubererStatB     :: Bool
    , mgZaubererStatE     :: Bool
    }

mkMGZaubererStats = MGZaubererStats
    { mgZaubererStatSP      = 0
    , mgZaubererStatSPMax   = 0
    , mgZaubererStatH       = MGZaubererHandAus
    , mgZaubererStatS       = MGZaubererSchutzAus
    , mgZaubererStatXH      = False
    , mgZaubererStatW       = False
    , mgZaubererStatB       = False
    , mgZaubererStatE       = False
    }
