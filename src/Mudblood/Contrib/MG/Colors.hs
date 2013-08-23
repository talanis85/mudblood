module Mudblood.Contrib.MG.Colors
    ( colorTriggers
    ) where

import Mudblood

colorRegex col pat = Permanent $ guardLineEvent >>> \s ->
    if s =~ pat :: Bool then returnLine $ setFg col s
                        else failT


colorMultilineCommunication :: Color -> String -> MBTriggerFlow
colorMultilineCommunication color regex = Permanent $ guardLineEvent >>> \l -> do
    guard $ l =~ regex
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

