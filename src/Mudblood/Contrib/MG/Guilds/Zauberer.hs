{-# LANGUAGE TypeFamilies,TypeOperators,FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module Mudblood.Contrib.MG.Guilds.Zauberer
    ( R_Zauberer (..), MGZaubererState (..)
    , mkMGZaubererState
    , zaubererReport
    , zaubererTriggers
    , zaubererWidgets
    , zaubererStatus
    , zaubererQuerySkills
    , zaubererSkills
    , zaubererSkillLevels
    ) where

import Data.Has hiding ((^.))
import Control.Lens
import Data.Maybe

import Text.Printf

import Mudblood
import Mudblood.Contrib.Regex
import Mudblood.Contrib.MG.State

------------------------------------------------------------------------------

data R_Zauberer = R_Zauberer
type instance TypeOf R_Zauberer = MGZaubererState

data MGZaubererHand = MGZaubererHandAus
                    | MGZaubererHandNormal
                    | MGZaubererHandFeuer
                    | MGZaubererHandEis
                    | MGZaubererHandSaeure

data MGZaubererSchutz = MGZaubererSchutzAus
                      | MGZaubererSchutzSchutz
                      | MGZaubererSchutzSchutzhuelle

data MGZaubererState = MGZaubererState
    { _zaubererStateSP    :: Int
    , _zaubererStateSPMax :: Int
    , _zaubererStateH     :: MGZaubererHand
    , _zaubererStateS     :: MGZaubererSchutz
    , _zaubererStateXH    :: Bool
    , _zaubererStateW     :: Bool
    , _zaubererStateB     :: Bool
    , _zaubererStateE     :: Bool
    }

mkMGZaubererState = MGZaubererState
    { _zaubererStateSP      = 0
    , _zaubererStateSPMax   = 0
    , _zaubererStateH       = MGZaubererHandAus
    , _zaubererStateS       = MGZaubererSchutzAus
    , _zaubererStateXH      = False
    , _zaubererStateW       = False
    , _zaubererStateB       = False
    , _zaubererStateE       = False
    }

------------------------------------------------------------------------------

zaubererStateSP      :: Lens' MGZaubererState Int
zaubererStateSP      = lens _zaubererStateSP      $ \s v -> s { _zaubererStateSP = v }

zaubererStateSPMax   :: Lens' MGZaubererState Int
zaubererStateSPMax   = lens _zaubererStateSPMax   $ \s v -> s { _zaubererStateSPMax = v }

zaubererStateH       :: Lens' MGZaubererState MGZaubererHand
zaubererStateH       = lens _zaubererStateH       $ \s v -> s { _zaubererStateH = v }

zaubererStateS       :: Lens' MGZaubererState MGZaubererSchutz
zaubererStateS       = lens _zaubererStateS       $ \s v -> s { _zaubererStateS = v }

zaubererStateXH      :: Lens' MGZaubererState Bool
zaubererStateXH      = lens _zaubererStateXH      $ \s v -> s { _zaubererStateXH = v }

zaubererStateW       :: Lens' MGZaubererState Bool
zaubererStateW       = lens _zaubererStateW       $ \s v -> s { _zaubererStateW = v }

zaubererStateB       :: Lens' MGZaubererState Bool
zaubererStateB       = lens _zaubererStateB       $ \s v -> s { _zaubererStateB = v }

zaubererStateE       :: Lens' MGZaubererState Bool
zaubererStateE       = lens _zaubererStateE       $ \s v -> s { _zaubererStateE = v }

------------------------------------------------------------------------------

zaubererWidgets :: (Has R_Zauberer u) => MB u [UIWidget]
zaubererWidgets = do
    stats <- getU R_Zauberer
    let zaubtable = UIWidgetTable
            [ [ "SP:",          (show $ stats ^. zaubererStateSP) ++ " (" ++ (show $ stats ^. zaubererStateSPMax) ++ ")" ]
            , [ "Schutz:",      showSchutz      $ stats ^. zaubererStateS ]
            , [ "Hand:",        showHand        $ stats ^. zaubererStateH ]
            , [ "Extrahand:",   showExtrahand   $ stats ^. zaubererStateXH ]
            , [ "Wille:",       showWille       $ stats ^. zaubererStateW ]
            ]
    return [ zaubtable ]
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

zaubererStatus :: (Has R_Common u, Has R_Zauberer u) => MB u String
zaubererStatus = do
    stat <- getU R_Common
    zstat <- getU R_Zauberer
    return $ printf "%d / %d | %d / %d | v:%d (%s) | g:%d | %s%s%s | %d / %d | %s %s %s %s"
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
        (zstat ^. zaubererStateSP)
        (zstat ^. zaubererStateSPMax)
        (showSchutz $ zstat ^. zaubererStateS)
        (showHand $ zstat ^. zaubererStateH)
        (showExtrahand $ zstat ^. zaubererStateXH)
        (showWille $ zstat ^. zaubererStateW)
  where
    showSchutz MGZaubererSchutzAus          = " "
    showSchutz MGZaubererSchutzSchutz       = "s"
    showSchutz MGZaubererSchutzSchutzhuelle = "S"
    
    showHand MGZaubererHandAus    = "  "
    showHand MGZaubererHandNormal = "Hf"
    showHand MGZaubererHandFeuer  = "HF"
    showHand MGZaubererHandEis    = "HE"
    showHand MGZaubererHandSaeure = "HS"

    showExtrahand True  = "XH"
    showExtrahand False = "  "

    showWille True  = "W"
    showWille False = " "

zaubererReport :: (Has R_Common u, Has R_Zauberer u) => AttrString -> MBTrigger u ()
zaubererReport x =
    case matchAS "^\\$REPORT\\$ ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) ([[:digit:]]+) '(.+)' ([JN])([JN])([JN])([JN]) ([FES ]) ([W ]) ([X ]) ([sSVZ ]) ([B ]) ([E ]) ([[:word:]]+)" x of
        [] -> failT
        r  ->
            let statfun =
                    (mgStatLP        .~ (read $ r !! 1))
                  . (mgStatMLP       .~ (read $ r !! 2))
                  . (mgStatKP        .~ (read $ r !! 3))
                  . (mgStatMKP       .~ (read $ r !! 4))
                  . (mgStatVO        .~ (read $ r !! 7))
                  . (mgStatFR        .~ r !! 8)
                  . (mgStatG         .~ (if (r !! 9) == "J" then 1 else 0))
                  . (mgStatB         .~ (if (r !! 10) == "J" then True else False))
                  . (mgStatT         .~ (if (r !! 11) == "J" then True else False))
                  . (mgStatF         .~ (if (r !! 12) == "J" then True else False))
                zaubfun =
                    (zaubererStateSP     .~ (read $ r !! 5))
                  . (zaubererStateSPMax  .~ (read $ r !! 6))
                  . (zaubererStateH      .~ case (r !! 13) of
                        "F" -> MGZaubererHandFeuer
                        "E" -> MGZaubererHandEis
                        "S" -> MGZaubererHandSaeure
                        _   -> MGZaubererHandAus
                    )
                  . (zaubererStateW      .~ (if (r !! 14) == "W" then True else False))
                  . (zaubererStateXH     .~ (if (r !! 15) == "X" then True else False))
                  . (zaubererStateS      .~ case (r !! 16) of
                        "s" -> MGZaubererSchutzSchutz
                        "S" -> MGZaubererSchutzSchutzhuelle
                        _   -> MGZaubererSchutzAus
                    )
                  . (zaubererStateB      .~ (if (r !! 17) == "W" then True else False))
                  . (zaubererStateE      .~ (if (r !! 18) == "X" then True else False))
            in modifyUserData $ (R_Common ^: statfun) . (R_Zauberer ^: zaubfun)

zaubererTriggers :: (Has R_Common u, Has R_Zauberer u) => MBTriggerFlow u
zaubererTriggers = Permanent (gag $ guardLine >=> zaubererReport)
              :>>: Permanent (guardLine >=> keep1 (regexAS "^Du lernst etwas aus Deinem Erfolg") >=> returnLine . setFg Magenta)
              :>>: Permanent (guardLine >=> keep1 (regexAS "^Du lernst etwas aus Deinen Fehlern") >=> returnLine . setFg Magenta)

zaubererQuerySkills :: (Has R_Zauberer u) => MBTrigger u [(String, Int)]
zaubererQuerySkills = yieldSend "teile llystrathe mit faehigkeiten" >>= readSkills
    where
        readSkills = guardLine
                     >=> regexAS "^Llystrathe teilt Dir mit: Folgendes ist von Deinen Fertigkeiten zu halten:"
                     >=> (const $ yieldT [] >>= repeatT parseSkill)
        parseSkill = guardLine >=> regexAS "^Llystrathe teilt Dir mit: (.+[[:word:]]) +: +(.+)$" >=> \(_, [k, v]) ->
            return (k, fromMaybe 0 $ skillToPercent zaubererSkillLevels v)

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
