module Mudblood.Contrib.MG.Fight
    ( fightColorizer
    ) where

import Data.Foldable
import Data.Monoid

import Mudblood

import Text.Regex.PCRE

attackMap = [ ("verfehlst ([^%.]+)",                                0,   0,   "")
            , ("kitzelst (.+) am Bauch",                            1,   1,   "kitzelst")
            , ("kratzt ([^%.]+)",                                   2,   3,   "kratzt")
            , ("triffst (.+) sehr hart",                            11,  20,  "triffst sehr hart")
            , ("triffst (.+) hart",                                 6,   10,  "triffst hart")
            , ("triffst ([^%.]+)",                                  4,   5,   "triffst")
            , ("schlaegst (.+) mit dem Krachen brechender Knochen", 21,  30,  "krachst")
            , ("zerschmetterst (.+) in kleine Stueckchen",          31,  50,  "schmetterst")
            , ("schlaegst (.+) zu Brei",                            51,  75,  "breist")
            , ("pulverisierst ([^%.]+)",                            76,  100, "pulverst")
            , ("zerstaeubst ([^%.]+)",                              101, 150, "zerstaeubst")
            , ("atomisierst ([^%.]+)",                              151, 200, "atomisierst")
            , ("vernichtest ([^%.]+)",                              201, 300, "vernichtest")
            ]

defenseMap = [ ("verfehlt Dich",                                    0,   0,   "")
             , ("kitzelt Dich am Bauch",                            1,   1,   "kitzelt Dich")
             , ("kratzt Dich",                                      2,   3,   "kratzt Dich")
             , ("trifft Dich sehr hart",                            11,  20,  "trifft dich sehr hart")
             , ("trifft Dich hart",                                 6,   10,  "trifft Dich hart")
             , ("trifft Dich",                                      4,   5,   "trifft Dich")
             , ("schlaegt Dich mit dem Krachen brechender Knochen", 21,  30,  "kracht Dich")
             , ("zerschmettert Dich in kleine Stueckchen",          31,  50,  "schmettert Dich")
             , ("schlaegt Dich zu Brei",                            51,  75,  "breit Dich")
             , ("pulverisiert Dich",                                76,  100, "pulvert Dich")
             , ("zerstaeubt Dich",                                  101, 150, "zerstaeubt Dich")
             , ("atomisiert Dich",                                  151, 200, "atomisitert Dich")
             , ("vernichtet Dich",                                  201, 300, "vernichtet Dich")
             ]

fightColorizer :: MBTriggerFlow
fightColorizer = Permanent $ guardLineEvent >>> \x ->
    let attackResult = foldMap (check "^  Du " x) attackMap
        defenseResult = foldMap (check "^  .+ " x) defenseMap
    in case getFirst attackResult of
        Just (min, max, short) -> returnLine $ formatA x min max
        Nothing -> case getFirst defenseResult of
            Just (min, max, short) -> returnLine $ formatD x min max
            Nothing -> returnLine x

  where check p x (re, min, max, short) = if x =~ (p ++ re) :: Bool
                                            then First $ Just $ (min, max, short)
                                            else First Nothing
        formatA l min max = setFg Green (l `mappend` toAttrString (" (" ++ (show min) ++ "-" ++ (show max) ++ ")"))
        formatD l min max = setFg Red (l `mappend` toAttrString (" (" ++ (show min) ++ "-" ++ (show max) ++ ")"))
