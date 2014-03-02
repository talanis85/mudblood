module Main where

import Mudblood
import Mudblood.Screen.Gtk

import System.Environment

import MG

main :: IO ()
main = do
    args <- getArgs
    userpath <- initUserPath []
    profpath <- case args of
        [profile] -> initUserPath ["mg", profile]
        _         -> initUserPath ["mg"]
    execScreen "res/gui.glade" mkMBConfig (mkMBState NoTrigger mkMGState) (boot userpath profpath >> screen (return ()))
