module Mudblood.Screen.Vty.Keys
  ( mapKey
  , mapKeyMod
  ) where

import           Mudblood.Keys
import qualified Graphics.Vty as V

-----------------------------------------------------------------------------

mapKey :: V.Key -> Maybe Key
mapKey k = case k of
    V.KPageUp       -> Just KPgUp
    V.KPageDown     -> Just KPgDn
    V.KUp           -> Just KUp
    V.KDown         -> Just KDown
    V.KLeft         -> Just KLeft
    V.KRight        -> Just KRight
    V.KChar '\t'    -> Just KTab
    V.KEnter        -> Just KEnter
    V.KBS           -> Just KBS
    V.KChar '\b'    -> Just KBS
    V.KEsc          -> Just KEsc
    V.KHome         -> Just KHome
    V.KIns          -> Just KInsert
    V.KEnd          -> Just KEnd
    V.KFun 1        -> Just $ KFun 1
    V.KFun 2        -> Just $ KFun 2
    V.KFun 3        -> Just $ KFun 3
    V.KFun 4        -> Just $ KFun 4
    V.KFun 5        -> Just $ KFun 5
    V.KFun 6        -> Just $ KFun 6
    V.KFun 7        -> Just $ KFun 7
    V.KFun 8        -> Just $ KFun 8
    V.KFun 9        -> Just $ KFun 9
    V.KFun 10       -> Just $ KFun 10
    V.KFun 11       -> Just $ KFun 11
    V.KFun 12       -> Just $ KFun 12
    V.KChar c       -> Just $ KAscii c
    _               -> Nothing

mapKeyMod :: V.Modifier -> KeyMod
mapKeyMod m = case m of
    V.MShift    -> MShift
    V.MCtrl     -> MCtrl
    V.MMeta     -> MMeta
    V.MAlt      -> MAlt
