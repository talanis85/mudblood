{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Mudblood.Contrib.MG
    ( MGState (..), mkMGState
    , MGStats (..)
    , MGChar (..)
    -- * Lenses
    -- ** General
    , getU, setU, updateMaybeU
    , (^.)
    -- ** MGState
    , mgChar, mgStats, mgTanjianStats, mgZaubererStats, mgGuild, mgFocus, mgProfile
    -- ** MGChar
    , mgCharName, mgCharRace, mgCharPresay, mgCharTitle, mgCharWizlevel, mgCharLevel, mgCharGuildLevel, mgCharGuildTitle
    -- ** MGStats
    , mgStatLP, mgStatMLP, mgStatKP, mgStatMKP, mgStatVO, mgStatFR, mgStatG, mgStatB, mgStatT, mgStatF
    -- * Profiles
    , addProfile, loadProfile
    -- * Modify the state
    , setFocus, setGuild
    -- * Widgets
    , mkMGStatWidgets
    , updateWidgetList
    -- * GMCP
    , gmcpTrigger
    -- * Report
    , reportTrigger
    -- * Spells
    , spell
    -- * Colors
    , triggerRegexLine, triggerRegexMultiline
    , colorFight
    -- * Guilds
    , readGuild, guildTriggers
    -- ** Tanjian
    -- ** Zauberer
    -- * Mapper
    , mgPrepareMap
    , mgStepper
    ) where

import Control.Monad
import Control.Lens
import Data.Typeable
import Data.Foldable
import Data.Monoid
import Data.Char
import Data.Maybe
import Text.Regex.PCRE
import qualified Data.Map as M
import qualified Data.Graph.Inductive as G

import Data.GMCP
import Mudblood

------------------------------------------------------------------------------

data TriState = On
              | Off
              | Between

data MGTanjianStats = MGTanjianStats
    { _mgTanjianStatM  :: TriState
    , _mgTanjianStatKO :: Bool
    , _mgTanjianStatTE :: TriState
    , _mgTanjianStatHA :: Bool
    , _mgTanjianStatAK :: TriState
    , _mgTanjianStatAKQuality :: TriState
    , _mgTanjianStatAKTime :: Int
    }

mkMGTanjianStats = MGTanjianStats
    { _mgTanjianStatM  = Off
    , _mgTanjianStatKO = False
    , _mgTanjianStatTE = Off
    , _mgTanjianStatHA = False
    , _mgTanjianStatAK = Off
    , _mgTanjianStatAKQuality = Off
    , _mgTanjianStatAKTime = 0
    }

makeLenses ''MGTanjianStats

data MGZaubererHand = MGZaubererHandAus
                    | MGZaubererHandNormal
                    | MGZaubererHandFeuer
                    | MGZaubererHandEis
                    | MGZaubererHandSaeure

data MGZaubererSchutz = MGZaubererSchutzAus
                      | MGZaubererSchutzSchutz
                      | MGZaubererSchutzSchutzhuelle

data MGZaubererStats = MGZaubererStats
    { _mgZaubererStatSP    :: Int
    , _mgZaubererStatSPMax :: Int
    , _mgZaubererStatH     :: MGZaubererHand
    , _mgZaubererStatS     :: MGZaubererSchutz
    , _mgZaubererStatXH    :: Bool
    , _mgZaubererStatW     :: Bool
    , _mgZaubererStatB     :: Bool
    , _mgZaubererStatE     :: Bool
    }

mkMGZaubererStats = MGZaubererStats
    { _mgZaubererStatSP      = 0
    , _mgZaubererStatSPMax   = 0
    , _mgZaubererStatH       = MGZaubererHandAus
    , _mgZaubererStatS       = MGZaubererSchutzAus
    , _mgZaubererStatXH      = False
    , _mgZaubererStatW       = False
    , _mgZaubererStatB       = False
    , _mgZaubererStatE       = False
    }

makeLenses ''MGZaubererStats

data MGGuild = MGGuildTanjian | MGGuildZauberer | MGGuildAbenteurer
    deriving (Eq)

data MGState = MGState
    { _mgChar          :: MGChar
    , _mgStats         :: MGStats
    , _mgTanjianStats  :: MGTanjianStats
    , _mgZaubererStats :: MGZaubererStats

    , _mgGuild         :: MGGuild
    , _mgFocus         :: Maybe String

    , _mgProfile       :: String
    }
  deriving (Typeable)

mkMGState = MGState
    { _mgChar            = mkMGChar
    , _mgStats           = mkMGStats
    , _mgTanjianStats    = mkMGTanjianStats
    , _mgZaubererStats   = mkMGZaubererStats

    , _mgGuild           = MGGuildAbenteurer
    , _mgFocus           = Nothing

    , _mgProfile         = ""
    }

data MGChar = MGChar
    { _mgCharName        :: String
    , _mgCharRace        :: String
    , _mgCharPresay      :: String
    , _mgCharTitle       :: String
    , _mgCharWizlevel    :: Int
    , _mgCharLevel       :: Int
    , _mgCharGuildLevel  :: Int
    , _mgCharGuildTitle  :: String
    }

mkMGChar = MGChar
    { _mgCharName        = "Jemand"
    , _mgCharRace        = "Etwas"
    , _mgCharPresay      = ""
    , _mgCharTitle       = ""
    , _mgCharWizlevel    = 0
    , _mgCharLevel       = 0
    , _mgCharGuildLevel  = 0
    , _mgCharGuildTitle  = ""
    }

data MGStats = MGStats
    { _mgStatLP      :: Int
    , _mgStatMLP     :: Int
    , _mgStatKP      :: Int
    , _mgStatMKP     :: Int
    , _mgStatVO      :: Int
    , _mgStatFR      :: String
    , _mgStatG       :: Int
    , _mgStatB       :: Bool
    , _mgStatT       :: Bool
    , _mgStatF       :: Bool
    }

mkMGStats = MGStats
    { _mgStatLP      = 0
    , _mgStatMLP     = 0
    , _mgStatKP      = 0
    , _mgStatMKP     = 0
    , _mgStatVO      = 0
    , _mgStatFR      = ""
    , _mgStatG       = 0
    , _mgStatB       = False
    , _mgStatT       = False
    , _mgStatF       = False
    }

makeLenses ''MGStats
makeLenses ''MGChar
makeLenses ''MGState

------------------------------------------------------------------------------

getU a = getUserData >>= (return . (^. a))
setU a v = modifyUserData $ set a v

updateMaybeU a v = case v of
    Nothing -> return ()
    Just v' -> setU a v'

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
                     setU mgProfile name
                     commands file
                     echo $ "Profil '" ++ name ++ "' geladen."

------------------------------------------------------------------------------

setFocus :: String -> MB ()
setFocus arg = case arg of
    "" -> setU mgFocus Nothing
    f  -> setU mgFocus (Just "hallo")

setGuild :: (MBMonad m) => String -> m ()
setGuild arg = case readGuild arg of
    Nothing -> echoErr $ "Unbekannte Gilde: " ++ arg
    Just g  -> do
               setU mgGuild g
               updateWidgetList

------------------------------------------------------------------------------

updateWidgetList :: (MBMonad m) => m ()
updateWidgetList = do
    guild <- getU mgGuild
    ui $ UIUpdateWidgets $
        [ UIWidgetText $ return "--- MorgenGrauen ---"
        ] ++ mkMGCharWidgets
          ++ mkMGStatWidgets
          ++ mkMGMapWidgets
          ++ case guild of
                MGGuildZauberer -> mkMGZaubererWidgets $ getU mgZaubererStats
                MGGuildTanjian -> mkMGTanjianWidgets $ getU mgTanjianStats
                _ -> []

mkMGCharWidgets =
    [ UIWidgetTable $ do
        char <- getU mgChar
        return
            [ [ "Name:", (char ^. mgCharName) ]
            , [ "Rasse:", (char ^. mgCharRace) ]
            , [ "Level:", (show $ char ^. mgCharLevel) ]
            ]
    ]

mkMGStatWidgets =
    [ UIWidgetTable $ do
        stats <- getU mgStats
        return
            [ [ "LP:", (show $ stats ^. mgStatLP) ++ " (" ++ (show $ stats ^. mgStatMLP) ++ ")" ]
            , [ "KP:", (show $ stats ^. mgStatKP) ++ " (" ++ (show $ stats ^. mgStatMKP) ++ ")" ]
            , [ "Vorsicht:", (show $ stats ^. mgStatVO) ]
            , [ "Fluchtrichtung:", (show $ stats ^. mgStatFR) ]
            , [ "Gift:", (showGift $ stats ^. mgStatG) ]
            , [ "Blind:", (showBool $ stats ^. mgStatB) ]
            , [ "Taub:", (showBool $ stats ^. mgStatT) ]
            , [ "Frosch:", (showBool $ stats ^. mgStatF) ]
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

gmcpTrigger ev = do
    case ev of
        GMCPTEvent g -> do
            case gmcpModule g of
                "MG.char.base" -> do
                    updateMaybeU (mgChar . mgCharName)      $ getStringField "name" g
                    updateMaybeU (mgChar . mgCharRace)      $ getStringField "race" g
                    updateMaybeU (mgChar . mgCharPresay)    $ getStringField "presay" g
                    updateMaybeU (mgChar . mgCharTitle)     $ getStringField "title" g
                    updateMaybeU (mgChar . mgCharWizlevel)  $ getIntField "wizlevel" g
                    case getStringField "guild" g >>= readGuild of
                        Nothing -> return ()
                        Just guild -> do
                                      oldguild <- getU mgGuild
                                      when (oldguild /= guild) $ setU mgGuild guild
                                      updateWidgetList
                "MG.char.info" -> do
                    updateMaybeU (mgChar . mgCharLevel)      $ getIntField "level" g
                    updateMaybeU (mgChar . mgCharGuildLevel) $ getIntField "guild_level" g
                    updateMaybeU (mgChar . mgCharGuildTitle) $ getStringField "guild_title" g
                "MG.char.maxvitals" -> do
                    updateMaybeU (mgStats . mgStatMLP)  $ getIntField "max_hp" g
                    updateMaybeU (mgStats . mgStatMKP)  $ getIntField "max_sp" g
                "MG.char.vitals" -> do
                    updateMaybeU (mgStats . mgStatLP)   $ getIntField "hp" g
                    updateMaybeU (mgStats . mgStatKP)   $ getIntField "sp" g
                "comm.channel" -> case getStringField "msg" g of
                    Just msg -> echoA $ setFg Blue $ toAttrString (rstrip msg)
                    Nothing -> return ()
                _ -> return ()
            return [ev]
        _ -> return [ev]
    where
        rstrip = reverse . dropWhile isSpace . reverse
------------------------------------------------------------------------------

triggerRegexLine pat = guardLine >=> \s -> guardT (s =~ pat :: Bool) >> return s

triggerRegexMultiline start startt next nextt = triggerRegexLine start
                                            >=> startt >=> yield
                                            >=> whileT guardLine (=~ next) nextt returnLine

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

colorFight :: TriggerEvent -> MBTrigger [TriggerEvent]
colorFight = guardLine >=> \x ->
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
    focus <- getU mgFocus
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

reportTrigger :: TriggerEvent -> MBTrigger [TriggerEvent]
reportTrigger = guardLine >=> \x -> do
    guild <- getU mgGuild
    case guild of
        MGGuildZauberer -> mgZaubererReport x
        MGGuildTanjian  -> mgTanjianReport x
        _               -> returnLine x

guildTriggers :: MBTriggerFlow
guildTriggers = tanjianTriggers

------------------------------------------------------------------------------

readGuild :: String -> Maybe MGGuild
readGuild "abenteurer"  = Just MGGuildAbenteurer
readGuild "tanjian"     = Just MGGuildTanjian
readGuild "zauberer"    = Just MGGuildZauberer
readGuild _             = Nothing

------------------------------------------------------------------------------

mkMGTanjianWidgets :: MB MGTanjianStats -> [UIWidget MB]
mkMGTanjianWidgets statfun =
    [ UIWidgetText $ return "--- Tanjian ---"
    , UIWidgetTable $ do
            stats <- statfun
            t <- getTime
            return
                [ [ "Meditation:",  showMeditation $ stats ^. mgTanjianStatM ]
                , [ "Kokoro:",      showBool $ stats ^. mgTanjianStatKO ]
                , [ "Tegatana:",    showTegatana $ stats ^. mgTanjianStatTE ]
                , [ "Omamori:",     showOmamori $ stats ^. mgTanjianStatTE ]
                , [ "Hayai:",       showBool $ stats ^. mgTanjianStatHA ]
                , [ "Akshara:",     showAkshara (stats ^. mgTanjianStatAK)
                                                (stats ^. mgTanjianStatAKQuality)
                                                (t - (stats ^. mgTanjianStatAKTime)) ]
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

mgTanjianReport x =
    case x =~ "^\\$REPORT\\$ (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) '(.+)' ([JN])([JN])([JN])([JN]) (\\w+) ([ -+]) (\\w+) (\\w+) (\\w+) ([JjN]) (\\d+)" :: [[String]] of
        r:rs -> do
            setU (mgStats . mgStatLP)   (read $ r !! 1)
            setU (mgStats . mgStatMLP)  (read $ r !! 2)
            setU (mgStats . mgStatKP)   (read $ r !! 3)
            setU (mgStats . mgStatMKP)  (read $ r !! 4)
            setU (mgStats . mgStatVO)   (read $ r !! 5)
            setU (mgStats . mgStatFR)   (r !! 6)
            setU (mgStats . mgStatG)    (if (r !! 7) == "J" then 1 else 0)
            setU (mgStats . mgStatB)    (if (r !! 8) == "J" then True else False)
            setU (mgStats . mgStatT)    (if (r !! 9) == "J" then True else False)
            setU (mgStats . mgStatF)    (if (r !! 10) == "J" then True else False)

            setU (mgTanjianStats . mgTanjianStatKO) $ if (r !! 11) == "ja" then True else False
            setU (mgTanjianStats . mgTanjianStatTE) $
                case (r !! 12) of
                    "+" -> On
                    "-" -> Between
                    _   -> Off

            setU (mgTanjianStats . mgTanjianStatHA) $ if (r !! 13) == "ja" then True else False
            setU (mgTanjianStats . mgTanjianStatAK) $
                case (r !! 14) of
                    "ja" -> On
                    "busy" -> Between
                    _   -> Off

            setU (mgTanjianStats . mgTanjianStatM) $
                case (r !! 16) of
                    "J" -> On
                    "j" -> Between
                    _   -> Off

            return []
        [] -> returnLine x

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

tanjianTriggers :: MBTriggerFlow
tanjianTriggers = mgAksharaTimeTriggers

------------------------------------------------------------------------------

mkMGZaubererWidgets :: MB MGZaubererStats -> [UIWidget MB]
mkMGZaubererWidgets statfun =
    [ UIWidgetText $ return "--- Zauberer ---"
    , UIWidgetTable $ do
            stats <- statfun
            return
                [ [ "SP:",          (show $ stats ^. mgZaubererStatSP) ++ " (" ++ (show $ stats ^. mgZaubererStatSPMax) ++ ")" ]
                , [ "Schutz:",      showSchutz $ stats ^. mgZaubererStatS ]
                , [ "Hand:",        showHand $ stats ^. mgZaubererStatH ]
                , [ "Extrahand:",   showExtrahand $ stats ^. mgZaubererStatXH ]
                , [ "Wille:",       showWille $ stats ^. mgZaubererStatW ]
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

mgZaubererReport x =
    case x =~ "^\\$REPORT\\$ (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) '(.+)' ([JN])([JN])([JN])([JN]) ([FES ]) ([W ]) ([X ]) ([sSVZ ]) ([B ]) ([E ]) (\\w+)" :: [[String]] of
        r:rs -> do
            setU (mgStats . mgStatLP)   (read $ r !! 1)
            setU (mgStats . mgStatMLP)  (read $ r !! 2)
            setU (mgStats . mgStatKP)   (read $ r !! 3)
            setU (mgStats . mgStatMKP)  (read $ r !! 4)
            setU (mgStats . mgStatVO)   (read $ r !! 7)
            setU (mgStats . mgStatFR)   (r !! 8)
            setU (mgStats . mgStatG)    (if (r !! 9) == "J" then 1 else 0)
            setU (mgStats . mgStatB)    (if (r !! 10) == "J" then True else False)
            setU (mgStats . mgStatT)    (if (r !! 11) == "J" then True else False)
            setU (mgStats . mgStatF)    (if (r !! 12) == "J" then True else False)

            setU (mgZaubererStats . mgZaubererStatSP)       $ read $ r !! 5
            setU (mgZaubererStats . mgZaubererStatSPMax)    $ read $ r !! 6
            setU (mgZaubererStats . mgZaubererStatH)        $
                case (r !! 13) of
                    "F" -> MGZaubererHandFeuer
                    "E" -> MGZaubererHandEis
                    "S" -> MGZaubererHandSaeure
                    _   -> MGZaubererHandAus
            setU (mgZaubererStats . mgZaubererStatW)        $ if (r !! 14) == "W" then True else False
            setU (mgZaubererStats . mgZaubererStatXH)       $ if (r !! 15) == "X" then True else False
            setU (mgZaubererStats . mgZaubererStatS)        $
                case (r !! 16) of
                    "s" -> MGZaubererSchutzSchutz
                    "S" -> MGZaubererSchutzSchutzhuelle
                    _   -> MGZaubererSchutzAus
            setU (mgZaubererStats . mgZaubererStatB)        $ if (r !! 17) == "B" then True else False
            setU (mgZaubererStats . mgZaubererStatE)        $ if (r !! 18) == "E" then True else False

            return []
        [] -> returnLine x

------------------------------------------------------------------------------

portals =
    [ (1, "tamibar", "bf586f14b202c43ea8727aefe7d5ae8a")
    , (2, "drachenzinnen", "a8760b707dbcf11d85da1c841abad7c9")
    , (3, "pv", "7ccced1f8b62fabb7b1494a6d9fd164d")
    , (4, "hochebene", "d7fe62577bcf655c97e285096d22c333")
    , (5, "polar", "f0d2288ec1a24d86318c8fbf9a221a08")
--    , (6, "tundra", "")
    , (8, "waldweg", "fbf2d086dd2983fd1f2e02d71941eff6")
    , (9, "valgessa", "0bc13858fa88bc03a57714362ea574ad")
    , (10, "wueste", "69aeaf5183002e46c31abf400d01c5d1")
    , (11, "aurora", "622018029d883479a5b07fb31f830230")
    , (12, "svolvaer", "9a45fd1465923334f1ee7a5d6617013f")
    , (13, "bergdorf", "aa17651e56c3f4f15a0be1bb30b6dac8")
    , (14, "nibelheim", "5429e40b37348ab7f679020a052435c0")
    , (16, "dschungel", "b712083865c62f24846239c6f14cb974")
--    , (18, "fernwest", "")
    , (19, "vland", "521620dec482f66752aa1ef0ca3cb806")
    , (20, "rodelbahn", "a48224c2b328606166f35c0716b03b46")
    , (21, "hobbitdorf", "2e844c5316daef31d19ed74861025396")
    , (22, "akhar", "a7920efb5367e33a39c689bb0814e9ac")
    , (23, "schacht", "1019c848caffd364c9a8f1074ef5a7c4")
    , (24, "endederwelt", "3d444f32639c6569fcc53598a56891d8")
    , (25, "friedhof", "9e1fad054f198baac665ebd228e7e1e3")
    , (26, "shaky", "f538d7b66dc3a114147cb12462a4dafa")
    , (27, "kaempfer", "7a58372be3796c191815e56202706937")
    , (28, "tortuga", "c28da35dcbcc94bb197f790efe7237b7")
    , (29, "katzmandu", "e0f281cbcfc32e9dd049abfac5418ce4")
    , (30, "rieseninsel", "b443892bf4661f2ede266eaee84ea776")
    , (31, "gebirge", "6577c8f842a9cd0661e27f9a0171367b")
    , (32, "portalraum", "297770a7a2af5e85fb461230aa6fdfe4")
    , (33, "umjak", "daf72c5edc7464972d4af4bdd83429b3")
    , (34, "tanjian", "cb057f19224b8d8d571b359487640800")
    , (36, "innuit", "8572f582b7c9fd45ce8e5d7ecbc8df71")
    , (37, "werwolfinsel", "c3917d2ca063f4304938051f1e51db86")
    , (38, "krylaios", "dbcef72d665565d2e277532d0d57f20b")
    , (39, "magieinsel", "eae4cf8f8bbe343c5bfc4f627290bdf6")
    , (40, "abgrund", "9ffe18fd06b3413a982ae16191d27b98")
    ]

mapHash :: Map -> String -> Maybe G.Node
mapHash m hash = case findRoomsWith (\x -> getUserValue "hash" (roomUserData x) == hash) m of
                    [] -> Nothing
                    (x:_) -> Just x

mapAddPortals :: (String -> Maybe G.Node) -> G.Context RoomData ExitData -> G.Context RoomData ExitData
mapAddPortals hashmap ctx@(i, n, l, o) =
    if checkPortal l
        then (i, n, l, o ++ (mapMaybe (genEdge hashmap) portals))
        else ctx
  where
    checkPortal r =
        let hash = getUserValue "hash" (roomUserData r)
        in not $ null $ filter (\(a,b,c) -> c == hash) portals
    genEdge hashmap (n, name, hash) =
        case hashmap hash of
            Nothing -> Nothing
            Just target -> Just (ExitData { exitLayer = "base", exitKey = "t " ++ (show n), exitUserData = M.empty }, target)

mgPrepareMap m = mapMapGraph (G.gmap $ mapAddPortals (mapHash m)) m

mgStepper s m = case findNextRoom s (mgPrepareMap m) (mapCurrentId m) of
                    Nothing -> m
                    Just n -> mapSetCurrent n m
