{-# LANGUAGE FlexibleContexts #-}
module Trigger ( tests ) where

import Prelude hiding ((.), id)

import Control.Monad
import Control.Monad.Trans
import Control.Category

import Control.Trigger
import Control.Trigger.Monad

import qualified Distribution.TestSuite as TS
import qualified Test.HUnit as HU

import Debug.Trace

-----------------------------------------------------------------------------

-- wrapMaybe = wrap (id, Just)

-----------------------------------------------------------------------------

hunitTests = HU.TestList [ simpleTests ] -- , complexTests, monadplusTests, postCombiTests ]

simpleTests = HU.TestLabel "-- BASIC --" $ HU.TestList
    [ HU.TestLabel "Yield constant"   $ triggerTest [1,2,3] [1,1,1] $
        trig' $ const 1
    ]

        {-
complexTests = HU.TestLabel "-- COMPLEX --" $ HU.TestList
    [ HU.TestLabel "wrapT . forever"   $ triggerTest [Just 1, Just 2, Just 3, Nothing, Just 4] [Just 11, Just 12, Just 13, Nothing, Just 14] $
        collate $ wrapMaybe $ forever $ trig $ Right . (+10)
    , HU.TestLabel "forever . wrapT"   $ triggerTest [Just 1, Just 2, Just 3, Nothing, Just 4] [Just 11, Just 12, Just 13, Nothing, Just 14] $
        collate $ forever $ wrapMaybe $ trig $ Right . (+10)
    , HU.TestLabel "forever . wrapT (with >=?>)"   $ triggerTest [Just 1, Just 5, Just 6, Nothing, Just 3] [Just 1, Just 15, Just 16, Nothing, Just 3] $
        collate $ forever $ wrapMaybe $ try $ check (>4) >=> succeed . (+10)
    , HU.TestLabel "wrapT . forever (with >=?>)"   $ triggerTest [Just 1, Just 5, Just 6, Nothing, Just 3] [Just 1, Just 15, Just 16, Nothing, Just 3] $
        collate $ wrapMaybe $ forever $ try $ check (>4) >=> succeed . (+10)
    ]
        -}

-----------------------------------------------------------------------------

triggerTest :: (Eq a, Show a) => [a] -> [a] -> EndoTrigger a IO () -> HU.Test
triggerTest input output trigger = HU.TestCase $ do
    r <- feedTrigger trigger input
    HU.assertEqual "Input and output don't match" output r

feedTrigger t i = runTrigger t i >>= return . fst

-----------------------------------------------------------------------------

runHUnitTests :: HU.Test -> IO TS.Progress
runHUnitTests tests = do
   (HU.Counts cases tried errors failures) <- HU.runTestTT tests
   return $ if errors > 0
      then TS.Finished $ TS.Error "There were errors in the HUnit tests"
      else if failures > 0
         then TS.Finished $ TS.Fail "There were failures in the HUnit tests"
         else TS.Finished TS.Pass
 
tests :: IO [TS.Test]
tests = return [ TS.Test hunit ]
  where
    hunit = TS.TestInstance
        { TS.run = runHUnitTests hunitTests
        , TS.name = "HUnit Test Cases"
        , TS.tags = ["hunit"]
        , TS.options = []
        , TS.setOption = \_ _ -> Right hunit
        }
