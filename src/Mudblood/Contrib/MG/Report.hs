module Mudblood.Contrib.MG.Report
    ( reportTrigger
    ) where

import Text.Regex.PCRE

import Mudblood
import Mudblood.Contrib.MG.State
import Mudblood.Contrib.MG.Gilden

reportTrigger :: MBTriggerFlow
reportTrigger = Permanent $ guardLine >=> \x -> do
    guild <- getU mgGuild
    case guild of
        MGGuildZauberer -> mgZaubererReport x
        MGGuildTanjian  -> mgTanjianReport x
        _               -> returnLine x


------------------------------------------------------------------------------

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
