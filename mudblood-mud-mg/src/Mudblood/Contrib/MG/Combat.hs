{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mudblood.Contrib.MG.Combat
  ( R, component, menu
  , stFocus, stShieldName
  , spell, hands, unhands

  , fetchFitness
  , fetchEscape
  , fetchDeath
  -- * Commands
  , schildCmd
  , autofightCmd
  ) where

import Mudblood
import Mudblood.Contrib.MG.Class
import Mudblood.Contrib.MG.Communication
import Mudblood.Contrib.MG.Event
import qualified Mudblood.Contrib.MG.Char as Char

import Control.Lens
import Control.Monad
import Control.Monad.Morph

import Data.Carte
import Data.Maybe
import Data.Monoid
import Data.String.Utils

------------------------------------------------------------------------------

data R a = R
    { _stFocus      :: Maybe String
    , _stShieldName :: String
    }
  deriving (Functor)

mkSt = R
    { _stFocus = Nothing
    , _stShieldName = "schild"
    }

makeLenses ''R

------------------------------------------------------------------------------

component :: forall l m. (Functor l, Screen m, Char.R :@: l) => MBComponent m MGEventType (Fix l) (Fix (R :*: l))
component = describe "Mudblood.MG.Combat" $ component'
  where
    component' :: MBComponent m MGEventType (Fix l) (Fix (R :*: l))
    component' = stateC mkSt
             >>> triggerC 10 fitnessHint
             >>> triggerC 10 smartEscape
             -- >>> triggerC 5 extendedEscape
             >>> triggerCombatC defaultCombatTrigger
             >>> commandC schildCmd
             >>> commandC autofightCmd

menu = describe "Combat" $ mconcat
    [ bindArg (KAscii 'f') "Focus" $ \f -> rec . stFocus .= (if f == "" then Nothing else Just f)
    , bindArg (KAscii 's') "Schild" $ assign (rec . stShieldName)
    ]

schildCmd = mkCommand "schild" "Setzt die ID des zu benutzenden Schildes." $
            f <$> arg stringParser "schildname" "ID des Schildes"
  where f name = do
          rec . stShieldName .= name
          echo $ toAS $ "Schild gesetzt: " ++ name

autofightCmd = mkCommand "autofight" "Autokampf gegen einen NPC" $
               f <$> arg stringParser "npc" "ID des Gegners"
  where f npc = raise $ mkEv $ CommandEvent ("autofight", [npc])

smartEscape = permanent $ do
    parse' $ fetchLineRegex "^Die Angst ist staerker als Du \\.\\.\\. Du willst nur noch weg hier\\.$"
    flush
    old <- lift $ use $ rec . Char.wimpy
    yieldWarning "Vorsicht aus"
    yieldSend "vorsicht 0"
    parse' $ do lp <- Char.fetchLP
                guard (lp > old)
    yieldWarning "Vorsicht ein"
    yieldSend $ "vorsicht " ++ show old

extendedEscape = permanent doTheEscape <> permanent gagOutgoing
  where doTheEscape = do
                      cmd <- parse $ do (_, msg) <- fetchIncomingMessage
                                        regex1 "^!fliehe (.+)$" msg
                      yieldSend cmd
        gagOutgoing = parse $ do
                              (_, msg) <- fetchOutgoingMessage
                              regex "^!fliehe (.+)$" msg
                              return ()

------------------------------------------------------------------------------

spell sp = do
    focus <- use $ rec . stFocus
    let final = replace "%f" (fromMaybe "" focus)  sp
    echo $ (toAS "> ") <> (setFg Yellow $ toAS final)
    send final

hands n = do
    shield <- use $ rec . stShieldName
    case n of
        1 -> send "steck waffe weg"
        2 -> send $ "zieh " ++ shield ++ " aus\nsteck waffe weg"
        _ -> return ()

unhands = do
    shield <- use $ rec . stShieldName
    send "zueck waffe"
    send $ "trage " ++ shield

------------------------------------------------------------------------------

-- | Compiled regexes for attack messages
attackMap :: (MonadPlus m, MonadFail m) => [AttrString -> m (String, Int, Int)]
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
    where f (r,a,b,c) = regex1 ("^  Du " ++ r ++ "\\.") >=> \name -> return (name, a, b)

-- | Compiled regexes for defend messages
defenseMap :: (MonadPlus m, MonadFail m) => [AttrString -> m (String, Int, Int)]
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
    where f (r,a,b,c) = regex1 ("^  (.+) " ++ r ++ "\\.") >=> \name -> return (name, a, b)

-- | Compiled regexes for fitness descriptions
fitnessMap :: (MonadPlus m, MonadFail m) => [AttrString -> m (String, Int)]
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
    where f (r,v) = regex1 ("^(.+) " ++ r ++ "\\.$") >=> \name -> return (name, v)

fetchFitness = fetchLine >>= stack (guardFirstOf fitnessMap)

fetchAttackLine = fetchLine >>= stack (guardFirstOf attackMap)

fetchDefenseLine = fetchLine >>= stack (guardFirstOf defenseMap)

fetchWeaponLine :: (Monad m, MonadFail m, LineEvent :<: a) => Parser (Ev a) m (String, Maybe String, String)
fetchWeaponLine = fetchLine >>= regex2 "^  (.+) greift Dich (.+) an\\.$" >>= \(who, weapon) -> return (last (words who), Nothing, weapon)

fetchEscape = fetchLineRegex "^Die Angst ist staerker als Du \\.\\.\\. Du willst nur noch weg hier\\.$"

fetchDeath = msum $ map fetchLineRegex
  [ "faellt tot zu Boden\\.$"
  , "^Das Kampfblech faellt laut scheppernd um\\.$"
  , "^Der Blubbertropfen blubbert noch ein letztes Mal\\.$"
  , "^Die Titanwalze zerfaellt in ihre Bestandteile\\.$"
  , "^Das Tentakelmonster stirbt mit einem leisen Rascheln\\.$"
  , "^Der Schlabbermurks verspritzt noch etwas Schleim und stirbt\\.$"
  ]

data FightEvent = AttackEvent Int Int | DefendEvent Int Int

triggerCombatC :: (Monad m, MonadFail m, LineEvent :<: e) => (AttrString -> FightEvent -> Iteration (Ev e) (MB u m) ()) -> MBComponent m e u u
triggerCombatC formatTrigger = attackC >>> defendC
  where
    attackC = triggerC 100 $ permanent $ do
        (l, (name, min, max)) <- parse fetchAttackLine
        formatTrigger l (AttackEvent min max)
    defendC = triggerC 100 $ permanent $ do
        (l, (name, min, max)) <- parse fetchDefenseLine
        formatTrigger l (DefendEvent min max)

defaultCombatTrigger l ev = case ev of
    AttackEvent min max -> do
        yieldInfo $ "(" ++ show min ++ "-" ++ show max ++ ")"
        yieldLine $ setFg Green l
    DefendEvent min max -> do
        yieldInfo $ "(" ++ show min ++ "-" ++ show max ++ ")"
        yieldLine $ setFg Red l

fitnessHint = permanent $ do
    (l, (name, v)) <- parse fetchFitness
    yieldInfo $ "(" ++ show v ++ "%)"
    yieldLine l
