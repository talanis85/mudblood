module Main where

import Mudblood.Screen.Vty
import Mudblood
import MG
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    userpath <- initUserPath []
    profpath <- case args of
        [profile] -> initUserPath ["mg", profile]
        _         -> initUserPath ["mg"]
    execScreen mkMBConfig (mkMBState NoTrigger mkMGState) (boot userpath profpath >> screen )
