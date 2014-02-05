module Mudblood.Contrib.Logfile
    ( logfileTrigger
    ) where

import System.IO

import Mudblood

logfileTrigger :: String -> MBTriggerFlow u
logfileTrigger filename = Volatile $ \ev -> do
    fileHandle <- io $ openFile filename AppendMode
    logfileLoop fileHandle ev
  where
    logfileLoop h = keep (handleEvent h) >=> yieldT >=> logfileLoop h
    handleEvent h ev = case ev of
        LineTEvent x -> io $ hPutStrLn h (show x) >> hFlush h
        SendTEvent x -> io $ hPutStrLn h ("> " ++ x) >> hFlush h
        _ -> return ()
