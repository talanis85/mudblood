module Mudblood.Mapper.Walk
    ( walker
    , WalkerControl (..)
    ) where

import Data.List
import Data.Maybe

import Mudblood.Monad
import Mudblood.Trigger

data WalkerControl = WalkerStop
                   | WalkerContinue
                   | WalkerPause
                   | WalkerRetry

-- | Trigger to auto-walk from one room to another.
walker :: (MBEvent a, Monad m)
       => (Int -> Iteration (Ev a) (MB u m) WalkerControl)
       -> [(String, Int)]
       -> Trigger (Ev a) (MB u m) ()

walker f [] = return ()
walker f ((x,n):xs) = do
    yieldSend x
    let walker' = do
            ret <- oneshot $ f n
            case ret of
                WalkerStop -> return ()
                WalkerContinue -> walker f xs
                WalkerPause -> walker'
                WalkerRetry -> walker f ((x,n):xs)
    walker'
