module Main where

import Mudblood
import Mudblood.Screen.Vty

import System.Environment

boot host port = mb (connect host port) >> screen

main :: IO ()
main = do
    args <- getArgs
    case args of
        [host, port] -> execScreen mkMBConfig (mkMBState Nothing ()) (boot host port)
        _ -> putStrLn "Usage:\tmudblood <host> <port>"
