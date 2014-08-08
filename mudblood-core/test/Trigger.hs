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

hunitTests = HU.TestList [ simpleTests ] -- , monadplusTests, postCombiTests ]

simpleTests = HU.TestLabel "-- BASIC --" $ HU.TestList
    [ HU.TestLabel "Yield constant"   $ triggerTest [1,2,3] [1,1,1] $
        trig' $ const 1
    ]

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
