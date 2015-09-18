{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
import Data.Monoid

import Control.Trigger hiding ((==>))
import Control.Monad.Identity
import Control.Monad.Trans.Identity
import Control.Monad.Reader
import Control.Monad.State

import qualified Test.HUnit as HU
import Test.QuickCheck

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

-----------------------------------------------------------------------------

main = defaultMain tests

commTest = permanent $ do
  (r, rest) <- parseU $ do
    x <- fetch >>= \x -> guard (x == 5) >> return x
    (xs, rest) <- flushLA $ many $ fetch >>= \x -> guard (x > 3) >> return x
    return (x + sum xs, rest)
  mapM_ feedback rest
  yield r

tests =
  [ testGroup "Control.Trigger.Core"
    [ testProperty "Right identity"     $ \t -> aTrigger_ t <> mempty =!= aTrigger_ t
    , testProperty "Left identity"      $ \t -> mempty <> aTrigger_ t =!= aTrigger_ t
    , testProperty "Associativity"      $ \a b c -> (aTrigger_ a <> aTrigger_ b) <> aTrigger_ c
                                                =!= aTrigger_ a <> (aTrigger_ b <> aTrigger_ c)
    , testProperty "nullT yields []"    $ nullT =!= listT []
    , testProperty "nullT eats all"     $ \t -> aTrigger_ t <> nullT =!= nullT
    , testProperty "constT"             $ \t x -> aTrigger_ t <> constT x =?= constT x
    , testProperty "passthruT 1"        $ \t -> aTrigger_ t <> passthruT =?= aTrigger_ t
    , testProperty "passthruT 2"        $ \t -> passthruT <> aTrigger_ t =?= aTrigger_ t
    , testProperty "fork (return ())"   $ \t -> fork (return ()) <> aTrigger_ t =!= aTrigger_ t
    , testProperty "feedback"           $ let t1 = commTest
                                              t2 = passthruT
                                          in resultOfI (t1 >--> t2) [4,5,6,2,1] === [4,11,2,1]
    ]
  , testGroup "Control.Trigger.Parser"
    [ testProperty "Always fail"        $ alwaysFailT =!= passthruT
    , testProperty "Always pass"        $ alwaysPassT =!= nullT
    ]
  , testGroup "Control.Trigger.Lift"
    -- Pity. This only holds if we dont use 'fork'
    [ let (=!=) = strongEquiv resultOfI
      in testProperty "distribute State" $ \t1 t2 s -> withoutFork t1 && withoutFork t2 ==>
                                                (evalStateT (distribute (aTriggerS_ t1) >> distribute (aTriggerS_ t2)) s)
                                            =!= (evalStateT (distribute (aTriggerS_ t1 >> aTriggerS_ t2)) s)
    ]
  ]

withoutFork :: ([TStep], Int) -> Bool
withoutFork (l, _) = all (not . isFork) l
  where isFork (TStepFork _) = True
        isFork _ = False

-- Use the stateful version of the tests
resultOf = resultOfS
aTrigger = aTriggerS
aTrigger_ = aTriggerS_
(=!=) = strongEquiv resultOfS
(=?=) = weakEquiv resultOfS

resultOfI :: Trigger Int Identity () -> [Int] -> [Int]
resultOfI trigger input =
  let runTrigger t i = execTrigger t i >>= return . fst
  in runIdentity $ runTrigger trigger input

resultOfS :: Trigger Int (State Int) () -> [Int] -> [Int]
resultOfS trigger input =
  let runTrigger t i = execTrigger t i >>= return . fst
  in evalState (runTrigger trigger input) 0

-- | Strong equivalence of triggers (i.e. for any input, both will yield the
--   same result.
strongEquiv f t1 t2 = \xs -> f t1 xs === f t2 xs

-- | Weak equivalence of triggers (i.e. the shorter of both results will be a
--   prefix of the longer.
weakEquiv f t1 t2 = \xs -> cpfx (f t1 xs) (f t2 xs)
  where
    cpfx xs [] = True
    cpfx [] ys = True
    cpfx (x:xs) (y:ys) = x == y && cpfx xs ys

infixr 1 =!=
infixr 1 =?=

-- Some very basic triggers
passthruT     = permanent $ await >>= yield
nullT         = permanent $ await
constT x      = permanent $ await >> yield x
listT []      = return ()
listT (x:xs)  = await >> yield x >> listT xs
alwaysFailT   = permanent $ parseU $ fetch >> mzero
alwaysPassT   = permanent $ parseU fetch

-----------------------------------------------------------------------------
--- ARBITRARY TRIGGERS

data TStep =
    TStepYield
  | TStepAwait
  | TStepFeed
  | TStepFork [TStep]
  | TStepInc
  | TStepSet Int
  deriving (Show)

instance Arbitrary TStep where
  arbitrary = resize 5 $ frequency
    [ (100, elements [TStepYield, TStepAwait, TStepInc, TStepFeed])
    , (20, TStepSet <$> arbitrary)
    , (10, do l <- arbitrary
              return $ TStepFork l
      )
    ]

aTriggerS_ :: ([TStep], Int) -> T Int Int (State Int) ()
aTriggerS_ (l, i) = void $ aTriggerS (l, i)

aTriggerS :: ([TStep], Int) -> T Int Int (State Int) Int
aTriggerS (l, i) = (foldr (>=>) return $ map f l) i
  where
    f x = case x of
            TStepYield  -> \y -> yield y >> lift get
            TStepAwait  -> \_ -> await
            TStepFeed   -> \y -> feedback y >> lift get
            TStepFork l -> \y -> fork (void $ aTriggerS (l, y)) >> lift get
            TStepInc    -> \y -> lift (modify (+1)) >> lift get
            TStepSet z  -> \_ -> lift (put z) >> lift get

aTriggerI_ :: ([TStep], Int) -> T Int Int Identity ()
aTriggerI_ (l, i) = void $ aTriggerI (l, i)

aTriggerI :: ([TStep], Int) -> T Int Int Identity Int
aTriggerI (l, i) = (foldr (>=>) return $ map f l) i
  where
    f x = case x of
            TStepYield  -> \y -> yield y >> return y
            TStepAwait  -> \_ -> await
            TStepFeed   -> \y -> feedback y >> return y
            TStepFork l -> \y -> fork (void $ aTriggerI (l, y)) >> return y
            TStepInc    -> \y -> return (y+1)
            TStepSet z  -> \_ -> return z

aTriggerIT_ :: ([TStep], Int) -> T Int Int (IdentityT Identity) ()
aTriggerIT_ (l, i) = void $ aTriggerIT (l, i)

aTriggerIT :: ([TStep], Int) -> T Int Int (IdentityT Identity) Int
aTriggerIT (l, i) = (foldr (>=>) return $ map f l) i
  where
    f x = case x of
            TStepYield  -> \y -> yield y >> return y
            TStepAwait  -> \_ -> await
            TStepFeed   -> \y -> feedback y >> return y
            TStepFork l -> \y -> fork (void $ aTriggerIT (l, y)) >> return y

