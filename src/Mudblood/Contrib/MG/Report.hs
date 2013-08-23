module Mudblood.Contrib.MG.Report
    ( defaultStatus
    , tanjianreportTrigger, defaultTanjianStatus
    , zaubererreportTrigger, defaultZaubererStatus
    ) where

import Text.Printf (printf)

import Mudblood
import Mudblood.Contrib.MG.State
import Mudblood.Contrib.MG.Gilden

defaultStatus :: MBTrigger ()
defaultStatus = do
    st <- getState
    let s = mgStats st
    putUIString "status" $
        printf "LP: %d / %d | KP: %d / %d | Flucht: %s (%d) | %c%c%c%c"
            (mgStatLP s)
            (mgStatMLP s)
            (mgStatKP s)
            (mgStatMKP s)
            (mgStatFR s)
            (mgStatVO s)
            (if mgStatG s > 0 then 'G' else ' ')
            (if mgStatB s then 'B' else ' ')
            (if mgStatT s then 'T' else ' ')
            (if mgStatF s then 'F' else ' ')

------------------------------------------------------------------------------

tanjianreportTrigger :: MBTrigger () -> MBTriggerFlow
tanjianreportTrigger updateFunction = Permanent $ guardLineEvent >>> \x ->
    case x =~ "^\\$REPORT\\$ (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) '(.+)' ([JN])([JN])([JN])([JN]) (\\w+) ([ -+]) (\\w+) (\\w+) (\\w+) ([JjN]) (\\d+)" :: [[String]] of
        r:rs -> do
            modifyStats $ \s -> s {
                mgStatLP    = (read $ r !! 1),
                mgStatMLP   = (read $ r !! 2),
                mgStatKP    = (read $ r !! 3),
                mgStatMKP   = (read $ r !! 4),
                mgStatVO    = (read $ r !! 5),
                mgStatFR    = r !! 6,
                mgStatG     = if (r !! 7) == "J" then 1 else 0,
                mgStatB     = if (r !! 8) == "J" then True else False,
                mgStatT     = if (r !! 9) == "J" then True else False,
                mgStatF     = if (r !! 10) == "J" then True else False
                }
            modifyTanjianStats $ \s -> s {
                mgTanjianStatKO = if (r !! 11) == "ja" then True else False,
                mgTanjianStatTE = case (r !! 12) of
                                    "+" -> On
                                    "-" -> Between
                                    _   -> Off
                                    ,
                mgTanjianStatHA = if (r !! 13) == "ja" then True else False,
                mgTanjianStatAK = case (r !! 14) of
                                    "ja" -> On
                                    "busy" -> Between
                                    _   -> Off
                                    ,
                mgTanjianStatM  = case (r !! 16) of
                                    "J" -> On
                                    "j" -> Between
                                    _   -> Off
                }
            updateFunction
            return []
        [] -> return [LineTEvent x]

defaultTanjianStatus :: MBTrigger ()
defaultTanjianStatus = do
    st <- getState
    let s = mgStats st
    let ts = mgTanjianStats st
    putUIString "status" $
        printf "LP: %d / %d | KP: %d / %d | Flucht: %s (%d) | %c%c%c%c | %s%s%s%s | %s"
            (mgStatLP s)
            (mgStatMLP s)
            (mgStatKP s)
            (mgStatMKP s)
            (mgStatFR s)
            (mgStatVO s)
            (if mgStatG s > 0 then 'G' else ' ')
            (if mgStatB s then 'B' else ' ')
            (if mgStatT s then 'T' else ' ')
            (if mgStatF s then 'F' else ' ')
            (if mgTanjianStatHA ts then "HA" else "  ")
            (case mgTanjianStatTE ts of
                On -> "TE"
                Between -> "OM"
                Off -> "  "
                )
            (if mgTanjianStatKO ts then "KO" else "  ")
            (case mgTanjianStatM ts of
                On -> "M"
                Between -> "m"
                Off -> " "
                )
            (case mgTanjianStatAK ts of
                On ->       "akshara"
                Between ->  " busy  "
                Off ->      "       "
                )

------------------------------------------------------------------------------

zaubererreportTrigger :: MBTrigger () -> MBTriggerFlow
zaubererreportTrigger updateFunction = Permanent $ guardLineEvent >>> \x ->
    case x =~ "^\\$REPORT\\$ (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) (\\d+) '(.+)' ([JN])([JN])([JN])([JN]) ([FES ]) ([W ]) ([X ]) ([sSVZ ]) ([B ]) ([E ]) (\\w+)" :: [[String]] of
        r:rs -> do
            modifyStats $ \s -> s {
                mgStatLP    = (read $ r !! 1),
                mgStatMLP   = (read $ r !! 2),
                mgStatKP    = (read $ r !! 3),
                mgStatMKP   = (read $ r !! 4),
                mgStatVO    = (read $ r !! 7),
                mgStatFR    = r !! 8,
                mgStatG     = if (r !! 9) == "J" then 1 else 0,
                mgStatB     = if (r !! 10) == "J" then True else False,
                mgStatT     = if (r !! 11) == "J" then True else False,
                mgStatF     = if (r !! 12) == "J" then True else False
                }
            modifyZaubererStats $ \s -> s {
                mgZaubererStatSP    = (read $ r !! 5),
                mgZaubererStatSPMax = (read $ r !! 6),
                mgZaubererStatH     = case (r !! 13) of
                                        "F" -> MGZaubererHandFeuer
                                        "E" -> MGZaubererHandEis
                                        "S" -> MGZaubererHandSaeure
                                        _   -> MGZaubererHandAus
                                        ,
                mgZaubererStatW     = if (r !! 14) == "W" then True else False,
                mgZaubererStatXH    = if (r !! 15) == "X" then True else False,
                mgZaubererStatS     = case (r !! 16) of
                                        "s" -> MGZaubererSchutzSchutz
                                        "S" -> MGZaubererSchutzSchutzhuelle
                                        _   -> MGZaubererSchutzAus
                                        ,
                mgZaubererStatB     = if (r !! 17) == "B" then True else False,
                mgZaubererStatE     = if (r !! 18) == "E" then True else False
                }
            updateFunction
            return []
        [] -> return [LineTEvent x]

defaultZaubererStatus :: MBTrigger ()
defaultZaubererStatus = do
    st <- getState
    let s = mgStats st
    let zs = mgZaubererStats st
    putUIString "status" $
        printf "%d / %d | %d / %d | %d / %d | Flucht: %s (%d) | %c%c%c%c | %s%s%s%s | H:%s S:%s"
            (mgStatLP s)
            (mgStatMLP s)
            (mgStatKP s)
            (mgStatMKP s)
            (mgZaubererStatSP zs)
            (mgZaubererStatSPMax zs)
            (mgStatFR s)
            (mgStatVO s)
            (if mgStatG s > 0 then 'G' else ' ')
            (if mgStatB s then 'B' else ' ')
            (if mgStatT s then 'T' else ' ')
            (if mgStatF s then 'F' else ' ')
            (if mgZaubererStatXH zs then "XH" else "  ")
            (if mgZaubererStatW zs then "W" else " ")
            (if mgZaubererStatB zs then "B" else " ")
            (if mgZaubererStatE zs then "E" else " ")
            (case mgZaubererStatH zs of
                MGZaubererHandFeuer -> "F"
                MGZaubererHandEis -> "E"
                MGZaubererHandSaeure -> "S"
                MGZaubererHandAus -> " "
                )
            (case mgZaubererStatS zs of
                MGZaubererSchutzSchutz -> "s"
                MGZaubererSchutzSchutzhuelle -> "S"
                MGZaubererSchutzAus -> " "
                )
