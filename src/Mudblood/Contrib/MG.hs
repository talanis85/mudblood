{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies,TypeOperators,FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module Mudblood.Contrib.MG
    (
    -- * State
      MGState (..), mkMGState
    , MGStats (..)
    , MGChar (..)
    , getU, setU, modifyU
    , (^.)
    , R_Common, MGCommon
    -- * Profiles
    , addProfile, loadProfile
    -- * Widgets
    , mkMGStatWidgets
    , updateWidgetList
    -- * GMCP
    , triggerGmcpStat
    , applyState
    -- * Report
    , reportTrigger
    -- * Spells
    , spell
    -- * Colors
    , line, regex, triggerRegexMultiline
    , colorFight
    -- * Guilds
    , readGuild, guildTriggers
    -- ** Tanjian
    -- ** Zauberer
    -- * Mapper
    --, mgPrepareMap
    --, mgStepper
    -- * Commands
    --, mgCommands
    ) where

import Control.Monad
import Control.Arrow
import Data.Has
import Data.Typeable
import Data.Foldable
import Data.Monoid
import Data.Char
import Data.Maybe
import Text.Regex.TDFA
import qualified Data.Map as M
import qualified Data.Graph.Inductive as G

import Data.GMCP
import Mudblood

------------------------------------------------------------------------------

(#) :: a -> (a -> b) -> b
a # f = f a

infix 1 #

------------------------------------------------------------------------------

-- STATE: Tanjian

data R_Tanjian = R_Tanjian deriving (Typeable)
type instance TypeOf R_Tanjian = MGTanjianStats

data TriState = On
              | Off
              | Between

data MGTanjianStats = MGTanjianStats
    { mgTanjianStatM  :: TriState
    , mgTanjianStatKO :: Bool
    , mgTanjianStatTE :: TriState
    , mgTanjianStatHA :: Bool
    , mgTanjianStatAK :: TriState
    , mgTanjianStatAKQuality :: TriState
    , mgTanjianStatAKTime :: Int
    }
  deriving (Typeable)

mkMGTanjianStats = MGTanjianStats
    { mgTanjianStatM  = Off
    , mgTanjianStatKO = False
    , mgTanjianStatTE = Off
    , mgTanjianStatHA = False
    , mgTanjianStatAK = Off
    , mgTanjianStatAKQuality = Off
    , mgTanjianStatAKTime = 0
    }

-- STATE: Zauberer

data R_Zauberer = R_Zauberer deriving (Typeable)
type instance TypeOf R_Zauberer = MGZaubererStats

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
  deriving (Typeable)

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

------------------------------------------------------------------------------

getUserData' :: (MBMonad m) => m MGState
getUserData' = getUserData

modifyUserData' :: (MBMonad m) => (MGState -> MGState) -> m ()
modifyUserData' = modifyUserData

getU :: (MBMonad m, Knows a (TypeOf a) MGState) => a -> m (TypeOf a)
getU a = getUserData' >>= (\x -> return (a ^. x))

setU :: (MBMonad m, Knows a (TypeOf a) MGState) => a -> (TypeOf a) -> m ()
setU a v = modifyUserData' $ a ^= v

modifyU :: (MBMonad m, Knows a (TypeOf a) MGState) => a -> (TypeOf a -> TypeOf a) -> m ()
modifyU a f = modifyUserData' $ a ^: f

------------------------------------------------------------------------------

addProfile :: String -> MB ()
addProfile name = do
    ex <- io $ existsUserPath [name]
    if ex
        then mbError "Profil existiert"
        else do
            profilepath <- io $ initUserPath [name]
            io $ writeFile (profilepath </> "rc") ""
            echo "Profil erstellt."

loadProfile :: String -> MB ()
loadProfile name = do
    profilepath <- io $ initUserPath [name]
    rcfile <- io $ readUserFile (profilepath </> "rc")
    case rcfile of
        Nothing -> mbError "Profil nicht gefunden"
        Just file -> do
                     modifyU R_Common $ \s -> s { mgProfile = name }
                     command file
                     echo $ "Profil '" ++ name ++ "' geladen."

------------------------------------------------------------------------------

updateWidgetList :: (MBMonad m) => m ()
updateWidgetList = do
    guild <- getU R_Common >>= return . mgGuild
    ui $ UIUpdateWidgets $
        [ UIWidgetText $ return "--- MorgenGrauen ---"
        ] ++ mkMGCharWidgets
          ++ mkMGStatWidgets
          ++ mkMGMapWidgets
          ++ case guild of
                MGGuildZauberer -> mkMGZaubererWidgets $ getU R_Zauberer
                MGGuildTanjian -> mkMGTanjianWidgets $ getU R_Tanjian
                _ -> []

mkMGCharWidgets =
    [ UIWidgetTable $ do
        char <- getU R_Char
        return
            [ [ "Name:", (char # mgCharName) ]
            , [ "Rasse:", (char # mgCharRace) ]
            , [ "Level:", (show $ char # mgCharLevel) ]
            ]
    ]

mkMGStatWidgets =
    [ UIWidgetTable $ do
        stats <- getU R_Stats
        return
            [ [ "LP:", (show $ stats # mgStatLP) ++ " (" ++ (show $ stats # mgStatMLP) ++ ")" ]
            , [ "KP:", (show $ stats # mgStatKP) ++ " (" ++ (show $ stats # mgStatMKP) ++ ")" ]
            , [ "Vorsicht:", (show $ stats # mgStatVO) ]
            , [ "Fluchtrichtung:", (show $ stats # mgStatFR) ]
            , [ "Gift:", (showGift $ stats # mgStatG) ]
            , [ "Blind:", (showBool $ stats # mgStatB) ]
            , [ "Taub:", (showBool $ stats # mgStatT) ]
            , [ "Frosch:", (showBool $ stats # mgStatF) ]
            ]
    ]
  where
    showGift 0 = "Nein"
    showGift _ = "Ja"

    showBool True = "Ja"
    showBool False = "Nein"

mkMGMapWidgets =
    [ UIWidgetText $ return "--- Mapper ---"
    , UIWidgetTable $ do
        map <- getMap
        let (id, room) = (mapCurrentId map, mapCurrentData map)
        return
            [ [ "Raum #:", (show $ id) ]
            , [ "Tag:", (getUserValue "tag" $ roomUserData room) ]
            , [ "Hash:", (getUserValue "hash" $ roomUserData room) ]
            ]
    ]

------------------------------------------------------------------------------

triggerGmcpStat :: MBTrigger TriggerEvent (MGState -> MGState)
triggerGmcpStat = arr $ \ev ->
    case ev of
        GMCPTEvent g ->
            case gmcpModule g of
                "MG.char.base" ->
                    let charfun = \s -> s
                            { mgCharName     = fromMaybe (mgCharName s) $ getStringField "name" g
                            , mgCharRace     = fromMaybe (mgCharRace s) $ getStringField "race" g
                            , mgCharPresay   = fromMaybe (mgCharPresay s) $ getStringField "presay" g
                            , mgCharTitle    = fromMaybe (mgCharTitle s) $ getStringField "title" g
                            , mgCharWizlevel = fromMaybe (mgCharWizlevel s) $ getIntField "wizlevel" g
                            }
                        commonfun = \s -> s
                            { mgGuild        = case getStringField "guild" g >>= readGuild of
                                                Nothing    -> mgGuild s
                                                Just guild -> guild
                            }
                    in mkStateFun commonfun charfun id
                "MG.char.info" ->
                    let charfun = \s -> s
                            { mgCharLevel       = fromMaybe (mgCharLevel s) $ getIntField "level" g
                            , mgCharGuildLevel  = fromMaybe (mgCharGuildLevel s) $ getIntField "guild_level" g
                            , mgCharGuildTitle  = fromMaybe (mgCharGuildTitle s) $ getStringField "guild_title" g
                            }
                    in mkStateFun id charfun id
                "MG.char.maxvitals" ->
                    let statfun = \s -> s
                            { mgStatMLP = fromMaybe (mgStatMLP s) $ getIntField "max_hp" g
                            , mgStatMKP = fromMaybe (mgStatMKP s) $ getIntField "max_sp" g
                            }
                    in mkStateFun id id statfun
                "MG.char.vitals" ->
                    let statfun = \s -> s
                            { mgStatLP  = fromMaybe (mgStatLP s) $ getIntField "hp" g
                            , mgStatKP  = fromMaybe (mgStatKP s) $ getIntField "sp" g
                            }
                    in mkStateFun id id statfun
                _ -> id
        _ -> id
    where
        mkStateFun :: (MGCommon -> MGCommon) -> (MGChar -> MGChar) -> (MGStats -> MGStats) -> MGState -> MGState
        mkStateFun a b c = (R_Common ^: a) . (R_Char ^: b) . (R_Stats ^: c)

{-
                "comm.channel" -> case getStringField "msg" g of
                    Just msg -> echoA $ setFg Blue $ toAttrString (rstrip msg)
                    Nothing -> return ()
                _ -> return ()
            return [ev]
        _ -> return [ev]
    where
        rstrip = reverse . dropWhile isSpace . reverse
        -}

------------------------------------------------------------------------------

line :: MBTrigger TriggerEvent AttrString
line = marr $ guardLine

regex pat = marr $ \s -> guardT (s =~ pat :: Bool) >> return s

triggerRegexMultiline start startt next nextt = triggerRegexLine start
                                            >=> startt >=> yieldT
                                            >=> whileT guardLine (=~ next) nextt returnLine
    where
        triggerRegexLine pat = guardLine >=> \s -> guardT (s =~ pat :: Bool) >> return s


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

colorFight :: MBTrigger AttrString [TriggerEvent]
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

spell :: (MBMonad m) => String -> m ()
spell sp = do
    focus <- getU R_Common >>= return . mgFocus
    let final = case focus of
                    Nothing -> sp
                    Just f  -> replace "%f" f sp
    echoA $ (toAttrString "> ") `mappend` (setFg Yellow $ toAttrString final)
    send final
  where
    replace :: Eq a => [a] -> [a] -> [a] -> [a]
    replace needle replacement haystack
      = case begins haystack needle of
          Just remains -> replacement ++ remains
          Nothing      -> case haystack of
                            []     -> []
                            x : xs -> x : replace needle replacement xs

    begins :: Eq a => [a] -> [a] -> Maybe [a]
    begins haystack []                = Just haystack
    begins (x : xs) (y : ys) | x == y = begins xs ys
    begins _        _                 = Nothing

------------------------------------------------------------------------------

reportTrigger :: MBTrigger TriggerEvent [TriggerEvent]
reportTrigger = (marr guardLine) >>> proc x -> do
    guild <- marr (\_ -> getU R_Common >>= return . mgGuild) -< ()
    case guild of
        MGGuildZauberer -> do
                           (stat, zaub) <- mgZaubererReport -< x
                           marr (modifyU R_Stats) -< stat
                           marr (modifyU R_Zauberer) -< zaub
                           returnA -< []
        MGGuildTanjian  -> do
                           (stat, tanji) <- mgTanjianReport -< x
                           marr (modifyU R_Stats) -< stat
                           marr (modifyU R_Tanjian) -< tanji
                           returnA -< []
        _               -> (marr $ const failT) -< ()

guildTriggers :: MBTriggerFlow
guildTriggers = tanjianTriggers

------------------------------------------------------------------------------

mkMGTanjianWidgets :: MB MGTanjianStats -> [UIWidget MB]
mkMGTanjianWidgets statfun =
    [ UIWidgetText $ return "--- Tanjian ---"
    , UIWidgetTable $ do
            stats <- statfun
            t <- getTime
            return
                [ [ "Meditation:",  showMeditation $ stats # mgTanjianStatM ]
                , [ "Kokoro:",      showBool $ stats # mgTanjianStatKO ]
                , [ "Tegatana:",    showTegatana $ stats # mgTanjianStatTE ]
                , [ "Omamori:",     showOmamori $ stats # mgTanjianStatTE ]
                , [ "Hayai:",       showBool $ stats # mgTanjianStatHA ]
                , [ "Akshara:",     showAkshara (stats # mgTanjianStatAK)
                                                (stats # mgTanjianStatAKQuality)
                                                (t - (stats # mgTanjianStatAKTime)) ]
                ]
    ]
  where
    showBool True  = "An"
    showBool False = "Aus"

    showMeditation On      = "Ja"
    showMeditation Off     = "Nein"
    showMeditation Between = "Abklingend"

    showTegatana On = "Ja"
    showTegatana _  = "Nein"

    showOmamori Between = "Ja"
    showOmamori _       = "Nein"

    showAkshara :: TriState -> TriState -> Int -> String
    showAkshara On q t = "Ja (noch " ++ (show $ (aksharaTime q) - t) ++ "s)"
    showAkshara Off _ _ = "Nein"
    showAkshara Between _ t = "Busy (noch " ++ (show $ 150 - t) ++ "s)"

    aksharaTime :: TriState -> Int
    aksharaTime On = 75
    aksharaTime Between = 60
    aksharaTime Off = 45

mgTanjianReport = marr $ \x ->
    case x =~ "^\\$REPORT\\$ (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) '(.+)' ([JN])([JN])([JN])([JN]) (\\w+) ([ -+]) (\\w+) (\\w+) (\\w+) ([JjN]) (\\d+)" :: [[String]] of
        r:rs ->
            let statfun = \s -> s
                  { mgStatLP        = (read $ r !! 1)
                  , mgStatMLP      = (read $ r !! 2)
                  , mgStatKP        = (read $ r !! 3)
                  , mgStatMKP       = (read $ r !! 4)
                  , mgStatVO        = (read $ r !! 5)
                  , mgStatFR        = r !! 6
                  , mgStatG         = (if (r !! 7) == "J" then 1 else 0)
                  , mgStatB         = (if (r !! 8) == "J" then True else False)
                  , mgStatT         = (if (r !! 9) == "J" then True else False)
                  , mgStatF         = (if (r !! 10) == "J" then True else False)
                  }
                tanjifun = \s -> s
                  { mgTanjianStatKO = (if (r !! 11) == "ja" then True else False)
                  , mgTanjianStatM  = case (r !! 12) of
                            "+" -> On
                            "-" -> Between
                            _   -> Off
                  , mgTanjianStatHA = (if (r !! 13) == "ja" then True else False)
                  , mgTanjianStatAK = case (r !! 14) of
                            "ja" -> On
                            "busy" -> Between
                            _   -> Off
                  , mgTanjianStatTE = case (r !! 16) of
                            "J" -> On
                            "j" -> Between
                            _   -> Off
                  }
            in return (statfun, tanjifun)
        [] -> failT

{-
mgAksharaTimeTriggers :: MBTriggerFlow
mgAksharaTimeTriggers = Permanent ak1 :>>: Permanent ak2 :>>: Permanent ak3
    where
        ak1 = triggerRegexLine "^Deine Haende fangen ploetzlich an, leicht zu leuchten." >=> \x -> do
                                              getTime >>= setU (mgTanjianStats . mgTanjianStatAKTime)
                                              setU (mgTanjianStats . mgTanjianStatAKQuality) Off
                                              returnLine x
        ak2 = triggerRegexLine "^Deine Haende fangen ploetzlich an, hell zu leuchten." >=> \x -> do
                                              getTime >>= setU (mgTanjianStats . mgTanjianStatAKTime)
                                              setU (mgTanjianStats . mgTanjianStatAKQuality) Between
                                              returnLine x
        ak3 = triggerRegexLine "^Deine Haende fangen ploetzlich an, sehr hell zu leuchten." >=> \x -> do
                                              getTime >>= setU (mgTanjianStats . mgTanjianStatAKTime)
                                              setU (mgTanjianStats . mgTanjianStatAKQuality) On
                                              returnLine x
-------------------------------------------------------------------------------}

tanjianTriggers :: MBTriggerFlow
--tanjianTriggers = mgAksharaTimeTriggers
tanjianTriggers = Permanent $ marr $ return . (:[])

------------------------------------------------------------------------------

mkMGZaubererWidgets :: MB MGZaubererStats -> [UIWidget MB]
mkMGZaubererWidgets statfun =
    [ UIWidgetText $ return "--- Zauberer ---"
    , UIWidgetTable $ do
            stats <- statfun
            return
                [ [ "SP:",          (show $ stats # mgZaubererStatSP) ++ " (" ++ (show $ stats # mgZaubererStatSPMax) ++ ")" ]
                , [ "Schutz:",      showSchutz $ stats # mgZaubererStatS ]
                , [ "Hand:",        showHand $ stats # mgZaubererStatH ]
                , [ "Extrahand:",   showExtrahand $ stats # mgZaubererStatXH ]
                , [ "Wille:",       showWille $ stats # mgZaubererStatW ]
                ]
    ]
  where
    showSchutz MGZaubererSchutzAus          = "Aus"
    showSchutz MGZaubererSchutzSchutz       = "Mechanisch"
    showSchutz MGZaubererSchutzSchutzhuelle = "Magisch"
    
    showHand MGZaubererHandAus    = "Aus"
    showHand MGZaubererHandNormal = "Feuer (schwach)"
    showHand MGZaubererHandFeuer  = "Feuer"
    showHand MGZaubererHandEis    = "Eis"
    showHand MGZaubererHandSaeure = "Saeure"

    showExtrahand True  = "An"
    showExtrahand False = "Aus"

    showWille True  = "An"
    showWille False = "Aus"

mgZaubererReport = marr $ \x ->
    case x =~ "^\\$REPORT\\$ (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) '(.+)' ([JN])([JN])([JN])([JN]) ([FES ]) ([W ]) ([X ]) ([sSVZ ]) ([B ]) ([E ]) (\\w+)" :: [[String]] of
        r:rs ->
            let statfun = \s -> s
                  { mgStatLP        = (read $ r !! 1)
                  , mgStatMLP      = (read $ r !! 2)
                  , mgStatKP        = (read $ r !! 3)
                  , mgStatMKP       = (read $ r !! 4)
                  , mgStatVO        = (read $ r !! 7)
                  , mgStatFR        = r !! 8
                  , mgStatG         = (if (r !! 9) == "J" then 1 else 0)
                  , mgStatB         = (if (r !! 10) == "J" then True else False)
                  , mgStatT         = (if (r !! 11) == "J" then True else False)
                  , mgStatF         = (if (r !! 12) == "J" then True else False)
                  }
                zaubfun = \s -> s
                  { mgZaubererStatSP    = (read $ r !! 5)
                  , mgZaubererStatSPMax = (read $ r !! 6)
                  , mgZaubererStatH     = case (r !! 13) of
                        "F" -> MGZaubererHandFeuer
                        "E" -> MGZaubererHandEis
                        "S" -> MGZaubererHandSaeure
                        _   -> MGZaubererHandAus
                  , mgZaubererStatW     = (if (r !! 14) == "W" then True else False)
                  , mgZaubererStatXH    = (if (r !! 15) == "X" then True else False)
                  , mgZaubererStatS     = case (r !! 16) of
                        "s" -> MGZaubererSchutzSchutz
                        "S" -> MGZaubererSchutzSchutzhuelle
                        _   -> MGZaubererSchutzAus
                  , mgZaubererStatB     = (if (r !! 17) == "W" then True else False)
                  , mgZaubererStatE     = (if (r !! 18) == "X" then True else False)
                  }
            in return (statfun, zaubfun)
        [] -> failT

------------------------------------------------------------------------------

