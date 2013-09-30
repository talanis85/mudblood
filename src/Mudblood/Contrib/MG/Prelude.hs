{-# LANGUAGE FlexibleContexts, Arrows #-}

module Mudblood.Contrib.MG.Prelude
    ( module Mudblood.Contrib.MG.Common
    , module Mudblood.Contrib.MG.State
    , module Mudblood.Contrib.MG.Gilden.Zauberer
    , module Mudblood.Contrib.MG.Gilden.Tanjian
    , module Mudblood.Contrib.MG.Mapper

    , triggerLoop
    , commonWidgets
    , guildWidgets
    , colorFight
    , guildTriggers
    , mgCommands
    ) where

import Text.Regex.TDFA
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Control.Arrow
import Control.Monad
import Data.Has

import Mudblood
import Mudblood.Contrib.MG.Common
import Mudblood.Contrib.MG.State
import Mudblood.Contrib.MG.Gilden.Zauberer
import Mudblood.Contrib.MG.Gilden.Tanjian
import Mudblood.Contrib.MG.Mapper

------------------------------------------------------------------------------

triggerLoop :: MBTrigger u a [TriggerEvent] -> MBTrigger u TriggerEvent [TriggerEvent] -> MBTrigger u a [TriggerEvent]
triggerLoop startt nextt = startt >>> (marr $ yieldT) >>> loop
    where
        loop = (nextt >>> (marr $ yieldT) >>> loop) <+> (arr $ return)
        --triggerRegexLine pat = guardLine >=> \s -> guardT (s =~ pat :: Bool) >> return s

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

colorFight :: MBTrigger u AttrString [TriggerEvent]
colorFight = marr $ \x ->
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

------------------------------------------------------------------------------

guildTriggers :: (Has R_Common u, Has R_Tanjian u, Has R_Zauberer u) => MBTriggerFlow u
guildTriggers = tanjianTriggers :>>: zaubererTriggers

------------------------------------------------------------------------------

commonWidgets :: (Has R_Common u) => MB u [UIWidget]
commonWidgets = do
    stats <- getU R_Common
    let chartable = UIWidgetTable
            [ [ "Name:", (stats # mgCharName) ]
            , [ "Rasse:", (stats # mgCharRace) ]
            , [ "Level:", (show $ stats # mgCharLevel) ]
            ]
    let stattable = UIWidgetTable
            [ [ "LP:", (show $ stats # mgStatLP) ++ " (" ++ (show $ stats # mgStatMLP) ++ ")" ]
            , [ "KP:", (show $ stats # mgStatKP) ++ " (" ++ (show $ stats # mgStatMKP) ++ ")" ]
            , [ "Vorsicht:", (show $ stats # mgStatVO) ]
            , [ "Fluchtrichtung:", (show $ stats # mgStatFR) ]
            , [ "Gift:", (showGift $ stats # mgStatG) ]
            , [ "Blind:", (showBool $ stats # mgStatB) ]
            , [ "Taub:", (showBool $ stats # mgStatT) ]
            , [ "Frosch:", (showBool $ stats # mgStatF) ]
            ]
    return [ UIWidgetText "--- CHARACTER ---"
           , chartable
           , UIWidgetText "--- STATUS ---"
           , stattable
           ]
  where
    showGift 0 = "Nein"
    showGift _ = "Ja"

    showBool True = "Ja"
    showBool False = "Nein"

guildWidgets :: (Has R_Zauberer u, Has R_Tanjian u, Has R_Common u) => MB u [UIWidget]
guildWidgets = do
    guild <- getU R_Common >>= return . mgGuild
    case guild of
        MGGuildTanjian -> tanjianWidgets
        MGGuildZauberer -> zaubererWidgets
        _ -> return []

------------------------------------------------------------------------------

mgCommands :: (Has R_Common u) => [(String, Exp (MB u) Value)]
mgCommands =
    [ ("focus", Function ["..."] $ do
        args <- getSymbol "..." >>= typeList
        case args of
            [] -> liftL (getU R_Common) >>= return . mkStringValue . fromMaybe "" . mgFocus
            (x:[]) -> do
                f <- typeString x
                liftL $ modifyU R_Common $ \s -> s { mgFocus = (if f == "" then Nothing else Just f)}
                return $ mkStringValue f
      )
    , ("walk", Function ["target"] $ do
        target <- getSymbol "target"
        case target of
            Value (IntValue x)    -> liftL $ mgWalk x
            Value (StringValue x) -> do
                r <- liftL $ mgFindRoom x
                case r of
                    Nothing -> throwError "Room not found"
                    Just r -> liftL $ mgWalk r
        return nil
      )
    ]
