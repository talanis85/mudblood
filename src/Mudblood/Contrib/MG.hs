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
    , readGuild
    -- ** Tanjian
    -- ** Zauberer
    ) where

import Control.Lens
import Data.Typeable
import Data.Foldable
import Data.Monoid
import Text.Regex.PCRE

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
    }

mkMGTanjianStats = MGTanjianStats
    { _mgTanjianStatM  = Off
    , _mgTanjianStatKO = False
    , _mgTanjianStatTE = Off
    , _mgTanjianStatHA = False
    , _mgTanjianStatAK = Off
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

setGuild :: String -> MB ()
setGuild arg = case readGuild arg of
    Nothing -> mbError $ "Unbekannte Gilde: " ++ arg
    Just g  -> do
               setU mgGuild g
               updateWidgetList

------------------------------------------------------------------------------

updateWidgetList :: MB ()
updateWidgetList = do
    guild <- getU mgGuild
    modifyWidgets $ \_ ->
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
                    Just msg -> echoA $ setFg Blue $ toAttrString msg
                    Nothing -> return ()
                _ -> return ()
            return [ev]
        _ -> return [ev]

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
    case focus of
        Nothing -> send sp
        Just f  -> send $ replace "%f" f sp
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

------------------------------------------------------------------------------

readGuild :: String -> Maybe MGGuild
readGuild "abenteurer"  = Just MGGuildAbenteurer
readGuild "tanjian"     = Just MGGuildTanjian
readGuild "zauberer"    = Just MGGuildZauberer
readGuild _             = Nothing

------------------------------------------------------------------------------

mkMGTanjianWidgets :: MB MGTanjianStats -> [UIWidget]
mkMGTanjianWidgets statfun =
    [ UIWidgetText $ return "--- Tanjian ---"
    , UIWidgetTable $ do
            stats <- statfun
            return
                [ [ "Meditation:",  showMeditation $ stats ^. mgTanjianStatM ]
                , [ "Kokoro:",      showBool $ stats ^. mgTanjianStatKO ]
                , [ "Tegatana:",    showTegatana $ stats ^. mgTanjianStatTE ]
                , [ "Omamori:",     showOmamori $ stats ^. mgTanjianStatTE ]
                , [ "Hayai:",       showBool $ stats ^. mgTanjianStatHA ]
                , [ "Akshara:",     showAkshara $ stats ^. mgTanjianStatAK ]
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

    showAkshara On = "Ja"
    showAkshara Off = "Nein"
    showAkshara Between = "Busy"

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

------------------------------------------------------------------------------

mkMGZaubererWidgets :: MB MGZaubererStats -> [UIWidget]
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
