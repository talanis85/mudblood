module Mudblood.Contrib.MG.Colors
    ( colorTriggers
    ) where

import Mudblood
import Text.Regex.PCRE

colorRegex col pat = Permanent $ guardLine >=> \s -> do
    guardT (s =~ pat :: Bool)
    returnLine $ setFg col s

colorMultilineCommunication :: Color -> String -> MBTriggerFlow
colorMultilineCommunication color regex = Permanent $ guardLine >=> \l -> do
    guardT $ l =~ regex
    l' <- yieldLine (setFg color l) >>= waitForLine
    follow l'
  where
    follow l = do
        if l =~ "^ " then do
                          l' <- yieldLine (setFg Blue l) >>= waitForLine
                          follow l'
                     else returnLine l

colorTriggers = colorMultilineCommunication Blue "^\\[[^\\]]+:[^\\]]+\\]"
           :>>: colorRegex Blue "^<Tanjian>"
           :>>: colorMultilineCommunication Blue "^.+ teilt Dir mit:"
           :>>: colorRegex Blue "^.+ aus der Ferne\\."
           :>>: colorRegex Blue "^Balance "

