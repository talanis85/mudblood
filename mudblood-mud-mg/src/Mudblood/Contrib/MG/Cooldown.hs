module Mudblood.Contrib.MG.Cooldown
  ( Cooldown
  , cooldown
  , startCooldown
  , noCooldown
  ) where

import Control.Applicative
import Mudblood

newtype Cooldown = Cooldown { getCooldown :: Int }

noCooldown :: Cooldown
noCooldown = Cooldown (-1000)

startCooldown :: (Screen s) => MB u s Cooldown
startCooldown = Cooldown <$> time

cooldown :: (Screen s) => Int -> Cooldown -> MB u s Int
cooldown delta c = do
  t <- time
  let diff = t - getCooldown c
  let t' = getCooldown c
  if diff > delta
     then return 0
     else return (delta - diff)
