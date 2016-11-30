{-# LANGUAGE TypeFamilies,TypeOperators,FlexibleContexts #-}

module Mudblood.Contrib.MG.Guilds.Kaempfer
    ( R, base, St
    , Action (..)
    , mkSt
    , triggers
    , queryStatus
    , kspell
    , querySkills

    , kaempferWurfwaffe
    ) where

import qualified Data.Map as M
import Data.Maybe
import Data.Vinyl.Open

import Control.Monad
import Control.Monad.Trans
import Control.Lens hiding (Action)

import Text.Printf

import Mudblood
import Mudblood.Contrib.MG.Guilds.Common

import qualified Mudblood.Contrib.MG.Char as Char

------------------------------------------------------------------------------

data R = R
type instance TypeOf R = St

base = olens (Proxy :: Proxy R)

------------------------------------------------------------------------------

data St = St
    { _kaempferCooldowns :: Action -> Ticks
    , _kaempferWurfwaffe :: Maybe String
    }

mkSt = St
    { _kaempferCooldowns = const 0
    , _kaempferWurfwaffe = Nothing
    }

data Action =
    Kampftritt
  | Kniestoss
  | Schildstoss
  | Waffenschlag
  | Ellbogenschlag
  | Parade
  | Schildparade
  | Finte
  | Waffentrick
    deriving (Eq, Show)

------------------------------------------------------------------------------

kaempferCooldowns :: Lens' St (Action -> Ticks)
kaempferCooldowns = lens _kaempferCooldowns $ \s v -> s { _kaempferCooldowns = v }

kaempferWurfwaffe :: Lens' St (Maybe String)
kaempferWurfwaffe = lens _kaempferWurfwaffe $ \s v -> s { _kaempferWurfwaffe = v }

------------------------------------------------------------------------------

cooldownModifiers action = case action of
    Kampftritt      -> [ (Kampftritt, 10)     ]
    Kniestoss       -> [ (Kniestoss, 10)      ]
    Schildstoss     -> [ (Schildstoss, 4)     ]
    Waffenschlag    -> [ (Waffenschlag, 4)    ]
    Ellbogenschlag  -> [ (Ellbogenschlag, 10) ]
    Parade          -> [ (Parade, 6)
                       , (Waffentrick, 6)
                       , (Finte, 6)
                       , (Waffenschlag, 2)    ]
    Schildparade    -> [ (Schildparade, 6)
                       , (Schildstoss, 8)     ]
    Finte           -> [ (Finte, 4)
                       , (Parade, 4)          ]
    Waffentrick     -> [ (Waffentrick, 4)
                       , (Parade, 4)          ]

modifyCooldowns :: (MB scr m, MG u m, Has Rec u) => Action -> m ()
modifyCooldowns action = do
    t <- time
    modifyMG Rec $ kaempferCooldowns %~ modifyCooldownsWith t (cooldownModifiers action)
  where
    modifyCooldownsWith t [] f action = f action
    modifyCooldownsWith t ((x,t'):xs) f action = if action == x
                                                    then t + t'
                                                    else modifyCooldownsWith t xs f action

{-
actionRegexes =
    [ (Kampftritt,     True,  "^Du versetzt (.+) einen heimtueckischen Kampftritt\\.$")
    -- , (Kampftritt,     False, "^Der Kampftritt ging daneben\\.%")
    -- , (Kampftritt,     False, "Hatte es irgendwas mit den Beinen zu tun\\?")
    , (Kniestoss,      True,  "^Du rammst (.+) das Knie in den Koerper\\.$")
    -- , (Kniestoss,      False, "^Der Kniestoss ging daneben\\.%")
    , (Schildstoss,    True,  "^Du machst einen gelungenen Schildstoss gegen")
    , (Waffenschlag,   True,  "^Du schlaegst (.+) fies mit")
    , (Ellbogenschlag, True,  "^Du schlaegst (.+) mit Deinem Ellbogen")
    , (Parade,         True,  "^Du parierst die naechsten Angriffe mit")
    , (Finte,          True,  "^Du machst eine erfolgreiche Finte gegen")
    , (Waffentrick,    True,  "^Du machst einen Waffentrick gegen")
    ]
-}

{-
triggerAction :: (MB scr m, MG u m, Has Rec u) => Trigger (Ev a) m ()
triggerAction = ftk $ guardLine >=> \x -> do
    msum $ map (handleAction x) actionRegexes
  where
    handleAction x (action, re) = do
        guard $ re ~= x
        lift $ lift $ modifyCooldowns action
-}

-- triggers :: (MB scr m, MG u m, Has Rec u) => Trigger' ()
triggers = permanent $ await >>= yield -- triggerAction

kspell :: (MB scr m, MG u m, Has Rec u, Has R_Common u) => Action -> m ()
kspell action = do
    spell $ (show action) ++ " %f"
    modifyCooldowns action

kaempferStatus :: (MB scr m, MG u m, Has R_Common u, Has Rec u) => m String
kaempferStatus = do
    stat <- getMG R_Common
    kstat <- getMG Rec
    t <- time
    return $ printf "%d / %d | %d / %d | v:%d (%s) | g:%d | %s%s%s | %s %s %s %s %s %s %s %s %s"
        (stat ^. mgStatLP)
        (stat ^. mgStatMLP)
        (stat ^. mgStatKP)
        (stat ^. mgStatMKP)
        (stat ^. mgStatVO)
        (stat ^. mgStatFR)
        (stat ^. mgStatG)
        (if stat ^. mgStatB then "B" else " ")
        (if stat ^. mgStatT then "T" else " ")
        (if stat ^. mgStatF then "F" else " ")
        (if stat ^. mgStatLP < (stat ^. mgStatMLP) `div` 2 then "!!" else "  ")
        (showCooldown t (kstat ^. kaempferCooldowns) "KT" Kampftritt)
        (showCooldown t (kstat ^. kaempferCooldowns) "KS" Kniestoss)
        (showCooldown t (kstat ^. kaempferCooldowns) "SS" Schildstoss)
        (showCooldown t (kstat ^. kaempferCooldowns) "WS" Waffenschlag)
        (showCooldown t (kstat ^. kaempferCooldowns) "ES" Ellbogenschlag)
        (showCooldown t (kstat ^. kaempferCooldowns) "PA" Parade)
        (showCooldown t (kstat ^. kaempferCooldowns) "FI" Finte)
        (showCooldown t (kstat ^. kaempferCooldowns) "WT" Waffentrick)
  where
    showCooldown t f short action = if t >= f action then short else "  "

------------------------------------------------------------------------------

{-
querySkills :: (Monad m) => Trigger (Ev a) m [(String, Int)]
querySkills = yieldSend "schaetz" >> readSkills
    where
        readSkills = awaitBlockGag >>= return . mapMaybe parseSkill
        parseSkill l = case regex2 "^   (.+) +: (.+)$" l of
            Nothing -> Nothing
            Just (s, v) -> Just (s, fromMaybe 0 $ skillToPercent skillLevels v)
-}

-- querySkills :: (Monad m) => Iteration' [(String, Int)]
querySkills = yieldSend "schaetz" >> readSkills
    where
        readSkills = parseU $ do
            fmap (map percentSkills) $ many $ fetchLineRegex2 "^   (.+) +: (.+)$"
        percentSkills (k, v) = (k, fromMaybe 0 $ skillToPercent skillLevels v)

position :: (Eq a) => a -> [a] -> Maybe Int
position v l = position' 0 v l
    where position' i v [] = Nothing
          position' i v (x:xs) = if x == v then Just i else position' (i+1) v xs

skillToPercent :: [String] -> String -> Maybe Int
skillToPercent levels val = fmap calcpercent $ position val levels
    where calcpercent x = ((x+1) * 100) `div` length levels

skillLevels = concat $ map (\x -> map ($ x) $ map (++) fein) grob
    where grob =
            [ "absolut superuebel"
            , "superuebel"
            , "sehr sehr uebel"
            , "sehr uebel"
            , "uebel"
            , "sehr sehr schlecht"
            , "sehr schlecht"
            , "schlecht"
            , "maessig"
            , "durchschnittlich"
            , "befriedigend"
            , "sehr befriedigend"
            , "recht gut"
            , "ganz gut"
            , "gut"
            , "sehr gut"
            , "ausgezeichnet"
            , "hervorragend"
            , "perfekt"
            , "absolut perfekt"
            ]
          fein =
            [ "weit entfernt von "
            , "noch laengst nicht "
            , "bald schon "
            , "fast "
            , ""
            ]
