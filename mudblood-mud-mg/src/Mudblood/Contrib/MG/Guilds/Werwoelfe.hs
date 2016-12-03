{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}

module Mudblood.Contrib.MG.Guilds.Werwoelfe
    ( R, component
    , stForm

    , autofight
    ) where

import Data.Carte
import Data.Monoid
import Data.Maybe
import Data.Char

import Control.Monad
import Control.Monad.Trans
import Control.Lens
import Control.Lens.TH

import Text.Printf

import Mudblood hiding (queryStatus)
import Mudblood.Contrib.MG.Event
import Mudblood.Contrib.MG.GMCP
import Mudblood.Contrib.MG.Cooldown

import qualified Mudblood.Contrib.MG.Char as Char
import qualified Mudblood.Contrib.MG.Combat as Combat
import qualified Mudblood.Contrib.MG.SkillDb as SkillDb

------------------------------------------------------------------------------

data Form = Normal | Wolf | Ghourdal | Horpas | Galbrag

data R a = R
    { _stForm :: Form
    , _stHowlCD :: Cooldown
    }
  deriving (Functor)

mkSt = R
    { _stForm = Normal
    , _stHowlCD = noCooldown
    }

makeLenses ''R

------------------------------------------------------------------------------

component :: (SkillDb.R :@: r, Screen m, MonadIO m, MBEvent e) => MBComponent m e (Fix r) (Fix (R :*: r))
component =
      stateC mkSt
  >>> triggerC 50 fitness
  >>> triggerC 50 resistance
  >>> triggerC 50 formTriggers
  >>> triggerC 50 howlCDTrigger
  >>> statusC status
  >>> SkillDb.skillC querySkills
  >>> triggerC 50 autofightTrigger

------------------------------------------------------------------------------

status = do
    wst <- use rec
    howlCD <- cooldown 600 (wst ^. stHowlCD)
    return $ printf "%s | heulen: %d"
        (showForm $ wst ^. stForm)
        howlCD
  where
    showForm Normal = ""
    showForm Ghourdal = "ghourdal"
    showForm Wolf = "wolf"
    showForm Horpas = "horpas"
    showForm Galbrag = "galbrag"

------------------------------------------------------------------------------

fitnessMap :: (MonadPlus m) => [AttrString -> m Int]
fitnessMap = map f
      [ ("sieht noch ganz frisch aus\\.", 100)
      , ("hat schon den ein oder anderen Schlag abgekriegt\\.", 90)
      , ("hat schon was abgekriegt\\.", 80)
      , ("blutet schon ganz gut, lecker\\.", 60)
      , ("hat ordentlich was ueber den Schaedel gekriegt\\.", 50)
      , ("ist\\.\\.\\. wie wuerde Mondheuler sagen\\? Bald faellig\\.\\.\\.", 30)
      , ("sieht nach baldigem Umkippen aus\\.", 10)
      ]
    where f (r,v) = \x -> if (r ++ "$") ~= x then return v else mzero

fetchWerwolfFitness = fetchLine >>= stack (guardFirstOf fitnessMap)

fitness = permanent $ parse fetchWerwolfFitness >>= \(str, val) -> do
    yieldInfo $ "(" ++ show val ++ "%)"
    yieldLine str

------------------------------------------------------------------------------

resistanceMap = map f
    [ ("ist gegen .+ geschuetzt\\.$", 50)
    , ("ist gegen .+ voellig immun\\.$", 100)
    , ("ist gegen .+ anfaellig\\.$", -100)
    ]
  where f (r,v) = \x -> if (r ++ "$") ~= x then return v else mzero

fetchWerwolfResistance = fetchLine >>= stack (guardFirstOf resistanceMap)

resistance = permanent $ do
  (str, val) <- parse fetchWerwolfResistance
  if val > 100
     then yieldLine $ setFg (RGB (150+val) 100 100) str
     else yieldLine $ setFg (RGB 100 (150+val) 100) str

------------------------------------------------------------------------------

formTriggers = permanent (parse' fetchForm >>= lift . assign (rec . stForm))

fetchForm = msum
  [ fetchLineRegex "^Du bist jetzt ein Wolf\\.$" >> return Wolf
  , fetchLineRegex "^Du bist jetzt ein Halbwolf\\.$" >> return Ghourdal
  , fetchLineRegex "^Du bist jetzt ein Wolfmensch\\.$" >> return Horpas
  , fetchLineRegex "^Du bist jetzt ein Menschwolf\\.$" >> return Galbrag
  , fetchLineRegex "^Du bemerkst, wie Du Dich wieder in Deine urspruengliche Form verwandelst" >> return Normal
  ]

------------------------------------------------------------------------------

howlCDTrigger = permanent $ do
  parse' $ do
    fetchLineRegex "^Du streckst Dich auf die Hinterbeine und heulst\\.$"
    fetchLineRegex "^(Ein Wolf|Eine Woelfin) kommt hereingelaufen\\.$"
  cd <- lift startCooldown
  lift $ assign (rec . stHowlCD) cd

------------------------------------------------------------------------------

querySkills = yieldSend "teile mondheuler mit status" >> readSkills
  where
    readSkills = parse $ do
      fetchLineRegex "^Folgendes ist von Deinen Faehigkeiten zu halten"
      fmap (map percentSkills) $ many $ fetchLineRegex2 "^([[:word:]]+) +(.+)$"
    percentSkills (k, v) = (k, fromMaybe 0 $ SkillDb.skillToPercent skillLevels v)

skillLevels =
    [ "quasi gar nicht"
    , "das kann jeder Welpe besser"
    , "eines Werwolfs unwuerdig"
    , "zum schreien"
    , "zum heulen"
    , "wirklich richtig schlecht"
    , "richtig schlecht"
    , "schlecht"
    , "etwas besser als schlecht"
    , "langsam besser werdend"
    , "immer noch nicht wirklich gut"
    , "noch nicht gut"
    , "fast gut"
    , "gut"
    , "schon besser als nur gut"
    , "immer besser werdend"
    , "richtig gut"
    , "sehr gut"
    , "hervorragend"
    , "geradezu phantastisch"
    , "fast perfekt"
    , "absolut perfekt"
    ]

------------------------------------------------------------------------------

autofightTrigger = permanent $ commandTrigger $ mkCommand "autofight" "autofight" $
  f <$> arg stringParser "npc" "npc"
  where f npc = void $ autofight 1 npc

autofight :: (MBEvent a, Screen s) => Int -> String -> Iteration (Ev a) (MB u s) Bool
autofight 1 npc = do
    yieldSend "ghourdal"
    yieldSend $ "toete " ++ npc
    r <- parse' $ msum
        [ do Combat.fetchDeath
             return True
        , do Char.fetchLP >>= guard . (< 40)
             return False
        ]
    if r then return () else yieldSend "vorsicht 40"
    return r
autofight 2 npc = do
    yieldSend "ghourdal"
    yieldSend $ "toete " ++ npc
    yieldSend "rage"
    let keepRage  = periodically 10 $ yieldSend "rage"
        stopFight = parse' $ msum
          [ do Combat.fetchDeath
               return True
          , do Char.fetchLP >>= guard . (< 40)
               return False
          ]
    r <- keepRage `chainIteration` stopFight
    if r then return () else yieldSend "vorsicht 40"
    return r
autofight _ npc = return False
