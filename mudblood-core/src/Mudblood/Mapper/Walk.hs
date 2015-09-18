module Mudblood.Mapper.Walk
    ( walker
    , WalkerControl (..)
    ) where

import Data.List
import Data.Maybe

import Mudblood.Core
import Mudblood.Class
import Mudblood.Trigger

data WalkerControl = WalkerStop
                   | WalkerContinue
                   | WalkerPause

-- | Trigger to auto-walk from one room to another.
walker :: (MB scr m, MBEvent a)
       => (Int -> Iteration (Ev a) m WalkerControl)
       -> [(String, Int)]
       -> Trigger (Ev a) m ()

walker f [] = return ()
walker f ((x,n):xs) = do
    yieldSend x
    let walker' = do
            ret <- oneshot $ f n
            case ret of
                WalkerStop -> return ()
                WalkerContinue -> walker f xs
                WalkerPause -> walker'
    walker'
