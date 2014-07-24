{-# LANGUAGE ScopedTypeVariables #-}
module Trigger ( tests ) where

import Prelude hiding ((.), id)

import Control.Monad
import Control.Monad.Trans
import Control.Category

import Control.Trigger.Monad
import Control.Trigger.Arrow
import Control.Trigger.Aux

import qualified Distribution.TestSuite as TS
import qualified Test.HUnit as HU

import Debug.Trace

triggerTest :: (Eq a, Show a) => [a] -> [a] -> EndoTrigger a IO -> HU.Test
triggerTest input output trigger = HU.TestCase $ do
    r <- feedTrigger (Just trigger) input
    HU.assertEqual "Input and output don't match" output r

triggerTestInt :: [Int] -> [Int] -> EndoTrigger Int IO -> HU.Test
triggerTestInt = triggerTest

feedTrigger t [] = return []
feedTrigger t (i:is) = do
    case t of
        Nothing -> feedTrigger t is >>= return . (i :)
        Just t -> do
            (r, t') <- runEndoTrigger t i
            feedTrigger t' is >>= return . (r ++)

hunitTests = HU.TestList [ simpleTests, appendTests ] -- , complexTests, monadplusTests, postCombiTests ]

-- Some trigger building blocks

simpleTests = HU.TestLabel "-- BASIC --" $ HU.TestList
    [ HU.TestLabel "Yield constant"   $ triggerTest [1,2,3] [1,1,1] $ static $ trig $ const $ yield [1]
    , HU.TestLabel "Yield input"      $ triggerTest [1,2,3] [1,2,3] $ static $ trig $ yield . singleton
    , HU.TestLabel "Flop always"      $ triggerTest [1,2,3] [1,2,3] $ static $ trig $ const flop
    , HU.TestLabel "Flop on 2"        $ triggerTest [1,2,3] [0,2,0] $ static $ trig $ \x -> if x == 2 then flop else void $ yield [0]
    , HU.TestLabel "Count"            $ triggerTest [0,0,0] [1,2,3] $ let f x = yield [x] >> f (x + 1)
                                                                              in trig $ const $ f 1
    , HU.TestLabel "One shot"         $ triggerTest [1,2,3] [0,2,3] $ trig $ const $ yield [0] >> return ()
    ]
    {-
    , HU.TestLabel "Await >>= Yield"          $ triggerTest [1]             [Just 1]                    $ await >>= yield
    , HU.TestLabel "Await >>= Flop"           $ triggerTest [1]             ([Nothing] :: [Maybe Int])  $ await >> flop
    , HU.TestLabel "Await 3 times"            $ triggerTest [(),(),()]      ([] :: [Maybe ()])          $ await >> await >> await
    , HU.TestLabel "Flop on 1, Yield on 2"    $ triggerTest (bracket $ await >>= \x -> if x == 1 then yield (x+1) else flop) [2,1] [Nothing, Just 2]
    , HU.TestLabel "Endless +1"               $ triggerTest (trigger (+1)) [1,2,3,4,5] [Just 2, Just 3, Just 4, Just 5, Just 6]
    ]
    -}

detectMulti :: (Monad m, Functor m) => (Int -> Bool) -> (Int -> Bool) -> Trigger m Int Int
detectMulti a b = trig $ state1
    where state1 = \x -> do
            guard $ a x
            yield x >>= state2
          state2 = \x -> do
            let morelines = do
                    guard $ b x
                    yield x >>= state2
            morelines `mplus` state1 x

dupl = trig $ \x -> yield [x,x]
add100 = trig $ yield . singleton . (+100)

appendTests = HU.TestLabel "-- (>:>) --" $ HU.TestList
    [ HU.TestLabel "Increment twice" $ triggerTest [1,2,3] [3,4,5] $
        let inc = trig $ yield . singleton . (+1)
        in static inc >:> static inc
    , HU.TestLabel "Constant and flop" $ triggerTest [1,2,3] [9,9,9] $
        static (trig $ const $ yield $ singleton 9) >:> static (trig $ const flop)
    , HU.TestLabel "Multiline 1" $ triggerTest [1,2,3,10,9,8,1,2,3] [1,2,3,110,109,108,1,2,3] $
        -- static $ trig $ \x -> lift (putStrLn $ "MULTILINE: " ++ show x) >> flop) >:> (detectMulti (>=10) (>=5) >>> static add100)
        (detectMulti (>=10) (>=5) >>> static add100)
    , HU.TestLabel "Multiline 2" $ triggerTest [1,2,3,10,9,8,1,2,3] [1,2,3,110,109,108,1,2,3] $
        (static $ trig $ yield . singleton) >:> (detectMulti (>=10) (>=5) >>> static add100)
    , HU.TestLabel "Multiline 3" $ triggerTest [1,2,3,10,9,8,1,2,3] [1,1,2,2,3,3,110,110,109,109,108,108,1,1,2,2,3,3] $
        (static dupl) >:> (detectMulti (>=10) (>=5) >>> static add100)
    ]


{-
complexTests = HU.TestLabel "-- COMPLEX --" $ HU.TestList
    [ HU.TestLabel "Multiline 1"   $ triggerTest [0,1,2,3,0,0] [0,2,3,4,0,0] multiline1
    , HU.TestLabel "Multiline 2"   $ triggerTest [0,1,2,3,0,5,0] [0,9,9,9,0,8,0] multiline2
    ]

multiline1 = state1
    where
        state1 x = if x == 1
                   then state2 x
                   else yield [x] >>= state1
        state2 x = if x == 0
                   then yield [x] >>= state1
                   else yield [x+1] >>= state2

multiline2 = chain $ state1
    where
        {-
        state1 x | x == 1    = state2 x
                 | x == 5    = state3 x
                 | otherwise = flop
        -}
        state1 x = (if x == 1 then state2 x else flop)
                   `mplus`
                   (if x == 5 then state3 x else flop)
        state2 x = if x == 0
                   then yield [x]
                   else yield [9] >>= state2
        state3 x = if x == 0
                   then yield [x]
                   else yield [8] >>= state3
-}

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
