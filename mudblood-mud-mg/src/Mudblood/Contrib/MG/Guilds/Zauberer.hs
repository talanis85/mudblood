{-# LANGUAGE TypeFamilies,TypeOperators,FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Mudblood.Contrib.MG.Guilds.Zauberer
    ( R
    , mkSt
    , triggers
    , status
    , setup
    , querySkills

    , Hand (..), Schutz (..)
    , stSP, stSPMax, stH, stXH
    , stS, stW, stB, stE
    , stWandMode

    , zHands
    ) where

import Data.Carte
import Data.Maybe
import Data.Monoid

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State

import Text.Printf

import Mudblood hiding (queryStatus)
import qualified Mudblood.Contrib.MG.Char as Char
import Mudblood.Contrib.MG.Event

------------------------------------------------------------------------------

data Hand = HandAus
          | HandNormal
          | HandFeuer
          | HandEis
          | HandSaeure

data Schutz = SchutzAus
            | SchutzSchutz
            | SchutzSchutzhuelle

data R a = R
    { _stSP    :: Int
    , _stSPMax :: Int
    , _stH     :: Hand
    , _stS     :: Schutz
    , _stXH    :: Bool
    , _stW     :: Bool
    , _stB     :: Bool
    , _stE     :: Bool
    , _stWandMode   :: Bool
    }
  deriving (Functor)

mkSt = R
    { _stSP      = 0
    , _stSPMax   = 0
    , _stH       = HandAus
    , _stS       = SchutzAus
    , _stXH      = False
    , _stW       = False
    , _stB       = False
    , _stE       = False
    , _stWandMode     = False
    }

------------------------------------------------------------------------------

makeLenses ''R

{-
stSP      :: Lens' St Int
stSP      = lens _stSP      $ \s v -> s { _stSP = v }

stSPMax   :: Lens' St Int
stSPMax   = lens _stSPMax   $ \s v -> s { _stSPMax = v }

stH       :: Lens' St Hand
stH       = lens _stH       $ \s v -> s { _stH = v }

stS       :: Lens' St Schutz
stS       = lens _stS       $ \s v -> s { _stS = v }

stXH      :: Lens' St Bool
stXH      = lens _stXH      $ \s v -> s { _stXH = v }

stW       :: Lens' St Bool
stW       = lens _stW       $ \s v -> s { _stW = v }

stB       :: Lens' St Bool
stB       = lens _stB       $ \s v -> s { _stB = v }

stE       :: Lens' St Bool
stE       = lens _stE       $ \s v -> s { _stE = v }

stWandMode :: Lens' St Bool
stWandMode = lens _stWandMode     $ \s v -> s { _stWandMode = v }
-}

------------------------------------------------------------------------------

{-
zaubererWidgets :: (Has Rec u) => MB u [UIWidget]
zaubererWidgets = do
    stats <- getU Rec
    let zaubtable = UIWidgetTable
            [ [ "SP:",          (show $ stats ^. stSP) ++ " (" ++ (show $ stats ^. stSPMax) ++ ")" ]
            , [ "Schutz:",      showSchutz      $ stats ^. stS ]
            , [ "Hand:",        showHand        $ stats ^. stH ]
            , [ "Extrahand:",   showExtrahand   $ stats ^. stXH ]
            , [ "Wille:",       showWille       $ stats ^. stW ]
            ]
    return [ zaubtable ]
  where
    showSchutz SchutzAus          = "Aus"
    showSchutz SchutzSchutz       = "Mechanisch"
    showSchutz SchutzSchutzhuelle = "Magisch"

    showHand HandAus    = "Aus"
    showHand HandNormal = "Feuer (schwach)"
    showHand HandFeuer  = "Feuer"
    showHand HandEis    = "Eis"
    showHand HandSaeure = "Saeure"

    showExtrahand True  = "An"
    showExtrahand False = "Aus"

    showWille True  = "An"
    showWille False = "Aus"
-}

setup = return ()

status :: (Screen s, R :@: r) => MB (Fix r) s String
status = do
    zstat <- use rec
    return $ printf "%d / %d | %s %s %s %s"
        (zstat ^. stSP)
        (zstat ^. stSPMax)
        (showSchutz $ zstat ^. stS)
        (showHand $ zstat ^. stH)
        (showExtrahand $ zstat ^. stXH)
        (showWille $ zstat ^. stW)
  where
    showSchutz SchutzAus          = " "
    showSchutz SchutzSchutz       = "s"
    showSchutz SchutzSchutzhuelle = "S"

    showHand HandAus    = "  "
    showHand HandNormal = "Hf"
    showHand HandFeuer  = "HF"
    showHand HandEis    = "HE"
    showHand HandSaeure = "HS"

    showExtrahand True  = "XH"
    showExtrahand False = "  "

    showWille True  = "W"
    showWille False = " "

triggerZaubererReport = do
    let reportRegex = "^\\$REPORT\\$ ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) '(.+)' ([JN])([JN])([JN])([JN]) ([FES ]) ([W ]) ([X ]) ([sSVZ ]) ([B ]) ([E ]) ([[:word:]]+)"
    r <- parse $ fetch >>= guardLine >>= (reportRegex ~~=)
    let statfun =
            (Char.lp         .~ (read $ r !! 1))
          . (Char.mlp        .~ (read $ r !! 2))
          . (Char.kp         .~ (read $ r !! 3))
          . (Char.mkp        .~ (read $ r !! 4))
          . (Char.wimpy      .~ (read $ r !! 7))
          . (Char.wimpyDir   .~ r !! 8)
          . (Char.poison     .~ (if (r !! 9) == "J" then 1 else 0))
          . (Char.blind      .~ (if (r !! 10) == "J" then True else False))
          . (Char.deaf       .~ (if (r !! 11) == "J" then True else False))
          . (Char.frog       .~ (if (r !! 12) == "J" then True else False))
        zaubfun =
            (stSP     .~ (read $ r !! 5))
          . (stSPMax  .~ (read $ r !! 6))
          . (stH      .~ case (r !! 13) of
                "F" -> HandFeuer
                "E" -> HandEis
                "S" -> HandSaeure
                _   -> HandAus
            )
          . (stW      .~ (if (r !! 14) == "W" then True else False))
          . (stXH     .~ (if (r !! 15) == "X" then True else False))
          . (stS      .~ case (r !! 16) of
                "s" -> SchutzSchutz
                "S" -> SchutzSchutzhuelle
                _   -> SchutzAus
            )
          . (stB      .~ (if (r !! 17) == "W" then True else False))
          . (stE      .~ (if (r !! 18) == "X" then True else False))
    lift $ do
        rec %= statfun
        rec %= zaubfun

triggers = mconcat
    [ permanent triggerZaubererReport
    , colorizer Magenta "^Du lernst etwas aus Deinem Erfolg"
    , colorizer Magenta "^Du lernst etwas aus Deinen Fehlern"
    , colorizer Cyan "^Dein Wille laesst nach"
    , colorizer Cyan "^Deine Extrahand loest sich auf"
    , colorizer Cyan "^Die (.+) Schutzaura um Dich loest sich langsam auf"
    , colorizer Cyan "^Die Verzauberung Deiner Haende laesst langsam nach"
    ]

colorizer c re = permanent $ do
    l <- parse $ fetch >>= guardLine >>= \x -> regex re x >> return x
    yieldLine $ setFg c l

-----------------------------------------------------------------------------

zHands n action = do
    wand <- use $ rec . stWandMode
    if n > 1 && wand
        then do
             send "steck waffe weg"
             action
             send "zueck waffe"
        else action

-----------------------------------------------------------------------------

querySkills = yieldSend "teile llystrathe mit faehigkeiten" >> readSkills
    where
        readSkills = parse $ do
            fetchMessageFrom "Llystrathe" >>= regex "Folgendes ist von Deinen Fertigkeiten zu halten:"
            fmap (map percentSkills) $ many $ fetchMessageFrom "Llystrathe" >>= regex2 "^(.+[[:word:]]) +: +(.+)$"
        percentSkills (k, v) = (k, fromMaybe 0 $ skillToPercent zaubererSkillLevels v)

position :: (Eq a) => a -> [a] -> Maybe Int
position v l = position' 0 v l
    where position' i v [] = Nothing
          position' i v (x:xs) = if x == v then Just i else position' (i+1) v xs

skillToPercent :: [String] -> String -> Maybe Int
skillToPercent levels val = fmap calcpercent $ position val levels
    where calcpercent x = ((x+1) * 100) `div` length levels

zaubererSkills = [ "Insgesamt"
                 , "Zaubern allgemein"
                 , "Zauberstab"
                 , "Erzwinge"
                 , "Giftpfeil"
                 , "Hand"
                 , "Identifiziere"
                 , "Licht"
                 , "Rausch"
                 , "Schutz"
                 , "Stimme"
                 , "Wasserwandlung"
                 , "Werte"
                 , "Wille"
                 , "Zwingtanz"
                 , "Schmerzen"
                 , "Schattenkaempfer"
                 , "Illusion"
                 , "Erschaffe"
                 , "Extrahand"
                 ]

zaubererSkillLevels = [
        "unaussprechbar uebel",
        "aeusserst uebel",
        "sehr uebel",
        "miserabelst",
        "aeusserst miserabel",
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
        "aeusserst mangelhaft",
        "sehr mangelhaft",
        "mangelhaft",
        "gerade noch mangelhaft",
        "so gerade eben noch mangelhaft",
        "mit Mueh und Not ausreichend",
        "nur knapp ausreichend",
        "ausreichend",
        "gut ausreichend",
        "schon fast befriedigend",
        "befriedigend",
        "sehr befriedigend",
        "ziemlich gut",
        "wirklich gut",
        "sehr gut",
        "aussergewoehnlich gut"
        ]
