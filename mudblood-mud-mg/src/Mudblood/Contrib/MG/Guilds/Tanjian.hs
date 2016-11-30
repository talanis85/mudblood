{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleContexts, Rank2Types, DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Mudblood.Contrib.MG.Guilds.Tanjian
    ( R
    , component
    , stM, stKO, stTE, stHA, stAK, stClan
    , autofight
    ) where

import Data.Carte
import Data.Monoid

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Lens

import Text.Printf

import Mudblood
import Mudblood.Trigger.Regex
import qualified Mudblood.Contrib.MG.Char as Char
import qualified Mudblood.Contrib.MG.Combat as Combat
import Mudblood.Contrib.MG.Event

------------------------------------------------------------------------------

data TriState = On
              | Off
              | Between

data Quality = Bad | Medium | Good

data Akshara = Akshara (Maybe Quality) Int
data AksharaStatus = AksharaOn Quality Int | AksharaBusy Int | AksharaReady

data R a = R
    { _stM  :: TriState
    , _stKO :: Bool
    , _stTE :: TriState
    , _stHA :: Bool
    , _stAK :: Akshara
    , _stClan :: Bool
    }
  deriving (Functor)

mkSt = R
    { _stM  = Off
    , _stKO = False
    , _stTE = Off
    , _stHA = False
    , _stAK = Akshara Nothing 0
    , _stClan = False
    }

------------------------------------------------------------------------------

makeLenses ''R

{-
stM    :: Lens' St TriState
stM    = lens _stM $ \s v -> s { _stM = v }

stKO   :: Lens' St Bool
stKO   = lens _stKO $ \s v -> s { _stKO = v }

stTE   :: Lens' St TriState
stTE   = lens _stTE $ \s v -> s { _stTE = v }

stHA   :: Lens' St Bool
stHA   = lens _stHA $ \s v -> s { _stHA = v }

stAK   :: Lens' St Akshara
stAK   = lens _stAK $ \s v -> s { _stAK = v }

stClan :: Lens' St Bool
stClan = lens _stClan $ \s v -> s { _stClan = v }
-}

------------------------------------------------------------------------------

aksharaStatus :: Akshara -> Int -> AksharaStatus
aksharaStatus (Akshara q t) t' =
    let qtime (Just Good) = (90, Good)
        qtime (Just Medium) = (90, Medium)
        qtime (Just Bad) = (60, Bad)
        qtime Nothing = (0, Bad)
        tdiff = t' - t
        (qt, q') = qtime q
        rest = qt - tdiff
    in if rest >= 0
        then AksharaOn q' rest
        else if tdiff > 150
                then AksharaReady
                else AksharaBusy (150 - tdiff)

------------------------------------------------------------------------------

component :: (Screen m, Functor l, Char.R :@: l, Combat.R :@: l, MGEvent e) => MBComponent m e (Fix l) (Fix (R :*: l))
component = describe "Mudblood.MG.Guilds.Tanjian" $
      stateC mkSt
  >>> statusC status
  >>> triggerC 0 (permanent triggerTanjianReport)
  >>> triggerC 50 (permanent triggerAkshara)
  >>> triggerC 50 spellStateTriggers
  >>> triggerC 50 triggerClanspells
  >>> triggerC 50 autofightTrigger

------------------------------------------------------------------------------

status :: (Screen s, R :@: r) => MB (Fix r) s String
status = do
  tstat <- use rec
  t <- time
  let te = case tstat ^. stTE of
        On -> "TE"
        Between -> "OM"
        Off -> "  "
      ha = if tstat ^. stHA then "HA" else "  "
      ko = if tstat ^. stKO then "KO" else "  "
      m = case tstat ^. stM of
        On -> "M"
        Between -> "m"
        Off -> " "
      showAkshara ak@(AksharaOn _ t') = "AK(" ++ show t' ++ ")"
      showAkshara AksharaReady = ""
      showAkshara ak@(AksharaBusy t') = "ak(" ++ show t' ++ ")"
      ak = showAkshara $ aksharaStatus (tstat ^. stAK) t
      cs = if tstat ^. stClan then "CS" else "  "
  return $ printf "%s%s%s%s%s%s" ha te ko m ak cs

------------------------------------------------------------------------------

triggerTanjianReport = do
    let reportRegex = "^\\$REPORT\\$ ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) '(.+)' ([JN])([JN])([JN])([JN]) ([[:word:]]+) (\\+|-| ) ([[:word:]]+) ([[:word:]]+) ([[:word:]]+) ([JjN]) ([[:digit:]]+)"
    r <- parse $ fetchLine >>= \x -> (reportRegex ~~= (fromAS x))

    lift $ do
        rec . Char.lp         .= (read $ r !! 1)
        rec . Char.mlp        .= (read $ r !! 2)
        rec . Char.kp         .= (read $ r !! 3)
        rec . Char.mkp        .= (read $ r !! 4)
        rec . Char.wimpy      .= (read $ r !! 5)
        rec . Char.wimpyDir   .= (r !! 6)
        rec . Char.poison     .= (if (r !! 7) == "J" then 1 else 0)
        rec . Char.blind      .= (if (r !! 8) == "J" then True else False)
        rec . Char.deaf       .= (if (r !! 9) == "J" then True else False)
        rec . Char.frog       .= (if (r !! 10) == "J" then True else False)

        let readTegatana x = case x of
               "+" -> On
               "-" -> Between
               _   -> Off
            readMeditation x = case x of
               "J" -> On
               "j" -> Between
               _   -> Off

        rec . stKO  .= (if (r !! 11) == "ja" then True else False)
        rec . stTE  .= (readTegatana $ r !! 12)
        rec . stHA  .= (if (r !! 13) == "ja" then True else False)
        rec . stM   .= (readMeditation $ r !! 16)

setupTanjianReport = send "$REPORT$ %la %lm %ka %km %vo '%fl' %gi%bl%ta%fr %ko %te %ha %ak %CA %me %ep%lf"

------------------------------------------------------------------------------

fetchAkshara = ak1 <|> ak2 <|> ak3 <|> akOff
    where
        ak1 = fetchLineRegex "^Deine Haende fangen ploetzlich an, leicht zu leuchten\\." >> return (Just Bad)
        ak2 = fetchLineRegex "^Deine Haende fangen ploetzlich an, hell zu leuchten\\." >> return (Just Medium)
        ak3 = fetchLineRegex "^Deine Haende fangen ploetzlich an, sehr hell zu leuchten\\." >> return (Just Good)
        akOff = fetchLineRegex "^Du verlaesst den Pfad des Lichtes\\." >> return Nothing

triggerAkshara = do
    ak <- parse' $ fetchAkshara
    case ak of
        Nothing -> lift aksharaOff
        Just qu -> lift $ aksharaSetup qu

aksharaSetup quality = time >>= \t -> rec . stAK .= Akshara (Just quality) t

aksharaOff = rec . stAK %= (\(Akshara _ t) -> Akshara Nothing t)

------------------------------------------------------------------------------

a >>? b = \x -> a x >> b

spellStateTrigger setter states = mconcat $ map stateTrigger states
  where stateTrigger st = permanent $ do
            r <- parse' $ fetch >>= guardLine >>= \x -> case st x of
                Nothing -> mzero
                Just x' -> return x'
            lift $ rec . setter .= r

spellStateTriggers = mconcat
    [ spellStateTrigger stKO
        [ regex "^Die Dunkelheit loest sich von Deinem Geist\\.$" >>? return True
        , regex "^Die Energien des Kokoro versiegen\\.$" >>? return False
        ]
    , spellStateTrigger stM
        [ regex "^Du beendest Deine Meditation\\.$" >>? return On
        , regex "^Deine Konzentrationsfaehigkeit laesst langsam nach\\.$" >>? return Between
        ]
    , spellStateTrigger stTE
        [ regex "^Du konzentrierst Dich auf den Kampf\\.$" >>? return On
        , regex "^Du konzentrierst Dich auf die Abwehr\\.$" >>? return Between
        , regex "^Deine Kampf-Konzentration laesst nach\\.$" >>? return Off
        , regex "^Deine Abwehr-Konzentration laesst nach\\.$" >>? return Off
        ]
    , spellStateTrigger stHA
        [ regex "^Du konzentrierst Dich auf den Fluss der Zeit\\.$" >>? return True
        , regex "^Die Kontrolle ueber den Zeitfluss entgleitet Dir\\.$" >>? return False
        ]
    ]

-------------------------------------------------------------------------------

-- CLANSPELLS

triggerClanspells = permanent triggerKageodori

fetchKageodori = kageOn <|> kageOff
  where kageOn = fetchLineRegex "^Du gibst Dich (.+) dem Schattentanz hin\\.$" >> return True
        kageOff = fetchLineRegex "^Dein Schattentanz naehert sich seinem Ende\\.$" >> return False

triggerKageodori = parse' fetchKageodori >>= lift . (rec . stClan .=)

-------------------------------------------------------------------------------

skillLevels = [
        "aeusserst uebel",
        "sehr uebel",
        "uebel",
        "noch uebel",
        "aeusserst miserabel",
        "sehr miserabel",
        "miserabel",
        "noch miserabel",
        "aeusserst schlecht",
        "sehr schlecht",
        "reichlich schlecht",
        "ziemlich schlecht",
        "schlecht",
        "gerade noch schlecht",
        "aeusserst ungenuegend",
        "ungenuegend",
        "noch ungenuegend",
        "aeusserst mangelhaft",
        "sehr mangelhaft",
        "mangelhaft",
        "noch mangelhaft",
        "fast ausreichend",
        "ausreichend",
        "gut ausreichend",
        "fast befriedigend",
        "befriedigend",
        "sehr befriedigend",
        "fast gut",
        "gut",
        "ziemlich gut",
        "wirklich gut",
        "sehr gut",
        "aussergewoehnlich gut",
        "unglaublich gut",
        "besser als gut",
        "viel besser als gut",
        "beinahe perfekt",
        "so gut wie perfekt",
        "wirklich fast perfekt"
        ]

-------------------------------------------------------------------------------

meditation = do
    cur <- lift $ use $ rec . stM
    case cur of
        On -> return ()
        _  -> do
            yieldSend "meditation"
            parse' $ fetchLineRegex "^Du beendest Deine Meditation"
            return ()

kokoro = do
    cur <- lift $ use $ rec . stKO
    if cur
       then return ()
       else do
          yieldSend "kokoro"
          parse' $ fetchLineRegex "^Die Dunkelheit loest sich von Deinem Geist"
          return ()

hayai = do
    cur <- lift $ use $ rec . stHA
    if cur
       then return ()
       else do
          yieldSend "hayai"
          parse' $ fetchLineRegex "^Der Zeitfluss veraendert sich"
          return ()

autofightTrigger = permanent $ commandTrigger "autofight" $ do
    npc <- getStringOption 0
    case npc of
      Just npc -> lift $ void $ autofight npc
      Nothing  -> return ()

autofight :: (R :@: r, Screen s, MGEvent e) => String -> Iteration (Ev e) (MB (Fix r) s) Bool
autofight npc = do
    meditation
    kokoro

    yieldSend $ "toete " ++ npc

    keepUpKokoro `chainIteration` escape `chainIteration` waitForDeath
  where
    keepUpKokoro = forever $ do
        parse' $ fetchLineRegex "^Die Energien des Kokoro versiegen"
        kokoro
    escape = parse' $ do
        lp <- Char.fetchLP
        guard (lp < 40)
        return False
    waitForDeath = parse' $ do
        Combat.fetchDeath
        return True
