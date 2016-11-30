{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Mudblood.Contrib.MG.Team
    ( R
    , triggers

    , showTeam
    , teamOverview
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans

import qualified Data.Map as M
import Data.Vinyl.Open
import Data.Maybe
import Data.Monoid

import Mudblood
import Mudblood.Contrib.MG

import Text.Printf

------------------------------------------------------------------------------

data R = R
type instance TypeOf R = St

base = olens (Proxy :: Proxy R)

------------------------------------------------------------------------------

type St = M.Map String TeamMember

data TeamMember = TeamMember
    { teamLV    :: Int
    , teamGLV   :: Int
    , teamLP    :: Int
    , teamMLP   :: Int
    , teamKP    :: Int
    , teamMKP   :: Int
    , teamV     :: Int
    , teamGR    :: Int
    , teamAR    :: Int
    , teamReady :: Bool
    }

mkSt = M.empty

mkTeamMember = TeamMember
    { teamLV    = 0
    , teamGLV   = 0
    , teamLP    = 0
    , teamMLP   = 0
    , teamKP    = 0
    , teamMKP   = 0
    , teamV     = 0
    , teamGR    = 0
    , teamAR    = 0
    , teamReady = False
    }

-----------------------------------------------------------------------------

echoTeam s = echo $ setFg Yellow $ toAS $ "[TEAM] " ++ s

-- displayReady :: (MB src m, MG u m, Has Rec u) => m ()
displayReady = do
    t <- use base
    let ready = M.size $ M.filter teamReady t
        all   = M.size t
    echoTeam $ printf "%d von %d bereit." ready all

-----------------------------------------------------------------------------

-- showTeam :: (MB src m, MG u m, Has Rec u) => m ()
showTeam = do
    t <- use base
    echoTeam $ "Aktuelles team: " ++ show (M.keys t)

-----------------------------------------------------------------------------

-- triggers :: (MB scr m, MG u m, Has Rec u) => Trigger (Ev MGEvent) m ()
triggers = chain
  [ permanent triggerReady
  , permanent triggerTeamList
  , permanent triggerAutoinfo
  , permanent triggerAttack
  ]

-- triggerReady :: (MB scr m, MG u m, Has Rec u) => Iteration (Ev a) m ()
triggerReady = do
    name <- parseU'  $ fetchLineRegex1 "^([[:word:]]+) nickt"
    t <- lift $ use base
    when (M.member name t) $ lift $ do
        base %= M.adjust (\x -> x { teamReady = True }) name
        displayReady

-- triggerTeamList :: (MB scr m, MG u m, Has Rec u) => Iteration (Ev a) m ()
triggerTeamList = do
    parseU $ fetchSendRegex "^g$"
    yieldSend "g"
    newteam <- fmap (catMaybes . map filterMember) $ parseU' $ fetchBlock
    lift $ base .= M.fromList newteam

-- triggerAttack :: (MG u m, Has Rec u) => Iteration (Ev MGEvent) m ()
triggerAttack = do
    parseU' $ fetchLineRegex "(Du startest den Angriff\\.$|startet den Angriff\\.$)"
    signal "team-attack"
    lift $ base %= M.map (\x -> x { teamReady = False })

-- triggerAutoinfo :: (MG u m, Has Rec u) => Iteration (Ev a) m ()
triggerAutoinfo = do
    r <- parseU $ fetchLine >>= matchAllAutoinfo
    forM_ r $ \(name, lp) -> lift $ base %= M.adjust (\x -> x { teamLP = read lp }) name
  where
    matchAllAutoinfo x = do
        [_, name, lp, rest] <- "([[:word:]]+): ([[:digit:]]+) LP(.*)$" ~~= x
        matchAllAutoinfo' [(name, lp)] rest
    matchAllAutoinfo' acc x = do
        case ", ([[:word:]]+): ([[:digit:]]+) LP(.*)$" ~~= x of
            Nothing -> return acc
            Just [_, name, lp, rest] -> matchAllAutoinfo' ((name, lp) : acc) rest

filterMember line = do
    [_, name, lv, glv, lp, mlp, kp, mkp, v, gr, ar] <-
        "[ \\*] ([[:word:]]+) +[[:word:]]+ +([[:digit:]]+) +([[:digit:]]+) +([[:digit:]]+) +\\(([[:digit:]]+)\\) +([[:digit:]]+) +\\(([[:digit:]]+)\\) +([[:digit:]]+) +([[:digit:]]+) +([[:digit:]]+)" ~~= line
    return (name, TeamMember
        { teamLV = read lv
        , teamGLV = read glv
        , teamLP = read lp
        , teamMLP = read mlp
        , teamKP = read kp
        , teamMKP = read mkp
        , teamV = read v
        , teamGR = read gr
        , teamAR = read ar
        , teamReady = False
        })

-- teamOverview :: (MG u m, Has Rec u) => m String
teamOverview = do
    t <- use base >>= return . M.toList
    if null t then return $ "-- NO TEAM --\n"
               else return $ "-- TEAM --\n\n" ++ unlines (map showMember t)
  where
    showMember (name, tm) = printf "* %8s: %3d (%3d) LP\n            %3d (%3d) KP - %s"
        name (teamLP tm) (teamMLP tm) (teamKP tm) (teamMKP tm) (if teamReady tm then "bereit" else "")
