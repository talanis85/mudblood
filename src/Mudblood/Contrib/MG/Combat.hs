module Mudblood.Contrib.MG.Combat
    ( quantizeFitness
    , colorizeCombat
    ) where

import Mudblood
import Mudblood.Contrib.Regex

import Data.Maybe
import Data.Monoid
import Data.Foldable

-- | Compiled regexes for attack messages
attackMap = map f
            [ ("verfehlst (.+)",                                    0,   0,   "")
            , ("kitzelst (.+) am Bauch",                            1,   1,   "kitzelst")
            , ("kratzt (.+)",                                       2,   3,   "kratzt")
            , ("triffst (.+) sehr hart",                            11,  20,  "triffst sehr hart")
            , ("triffst (.+) hart",                                 6,   10,  "triffst hart")
            , ("triffst (.+)",                                      4,   5,   "triffst")
            , ("schlaegst (.+) mit dem Krachen brechender Knochen", 21,  30,  "krachst")
            , ("zerschmetterst (.+) in kleine Stueckchen",          31,  50,  "schmetterst")
            , ("schlaegst (.+) zu Brei",                            51,  75,  "breist")
            , ("pulverisierst (.+)",                                76,  100, "pulverst")
            , ("zerstaeubst (.+)",                                  101, 150, "zerstaeubst")
            , ("atomisierst (.+)",                                  151, 200, "atomisierst")
            , ("vernichtest (.+)",                                  201, 300, "vernichtest")
            ]
    where f (r,a,b,c) = (matchAS' ("^  Du " ++ r ++ "\\.$"), a, b, c)

-- | Compiled regexes for defend messages
defenseMap = map f
             [ ("verfehlt Dich",                                    0,   0,   "")
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
             , ("atomisiert Dich",                                  151, 200, "atomisiert Dich")
             , ("vernichtet Dich",                                  201, 300, "vernichtet Dich")
             ]
    where f (r,a,b,c) = (matchAS' ("^  (.+) " ++ r ++ "\\.$"), a, b, c)

-- | Compiled regexes for fitness descriptions
fitnessMap = map f
      [ ("ist absolut fit",                    100)
      , ("ist schon etwas geschwaecht",        90)
      , ("fuehlte sich heute schon besser",    80)
      , ("ist leicht angeschlagen",            70)
      , ("sieht nicht mehr taufrisch aus",     60)
      , ("macht einen mitgenommenen Eindruck", 50)
      , ("wankt bereits bedenklich",           40)
      , ("ist in keiner guten Verfassung",     30)
      , ("braucht dringend einen Arzt",        20)
      , ("steht auf der Schwelle des Todes",   10)
      ]
    where f (r,v) = (matchAS' (r ++ "\\.$"), v)

-- | A trigger that appends percent values after fitness descriptions
quantizeFitness :: TriggerEvent -> MBTrigger u [TriggerEvent]
quantizeFitness = guardLine >=> \x ->
    let res = listToMaybe $ catMaybes $ map (\(r,v) -> if r x then Just v else Nothing) fitnessMap
    in case res of
        Nothing -> returnLine x
        Just n  -> returnLine $ x <> (toAS $ " (" ++ (show n) ++ "%)")

-- | A colorizer for combat messages
colorizeCombat :: Color -> Color -> TriggerEvent -> MBTrigger u [TriggerEvent]
colorizeCombat attackColor defendColor = guardLine >=> \x ->
    let attackResult = foldMap (check x) attackMap
        defenseResult = foldMap (check x) defenseMap
    in case getFirst attackResult of
         Just (min, max, short) -> returnLine $ formatA x min max
         Nothing -> case getFirst defenseResult of
             Just (min, max, short) -> returnLine $ formatD x min max
             Nothing -> returnLine x

  where check x (re, min, max, short) = if re x
                                            then First $ Just $ (min, max, short)
                                            else First Nothing
        formatA l min max = setFg attackColor (l <> toAS (" (" ++ (show min) ++ "-" ++ (show max) ++ ")"))
        formatD l min max = setFg defendColor (l <> toAS (" (" ++ (show min) ++ "-" ++ (show max) ++ ")"))
