{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
import Data.Monoid

import Control.Trigger
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

{-
commTest = permanent $ do
  (r, rest) <- parseU $ do
    x <- fetch >>= \x -> guard (x == 5) >> return x
    (xs, rest) <- flushLA $ many $ fetch >>= \x -> guard (x > 3) >> return x
    return (x + sum xs, rest)
  mapM_ feedback rest
  yield r
-}

{-
commTest :: (Monad m) => T Int Int m ()
commTest = forever $ do
  x <- await
  step1 x
  where
    step1 x = do
      if x > 5
         then step2 x
         else yield x
    step2 x = do
      y <- await
      if y > 10
         then step2 (x + y)
         else yield x >> step1 y

allUntilTest :: (Monad m) => T Int Int m Int
allUntilTest = do
  x <- await
  if x > 10
     then yield (x + 10) >> allUntilTest
     else return x

commTest2 :: (Monad m) => T Int Int m ()
commTest2 = forever $ do
  x <- await
  step1 x
  where
    step1 x = do
      if x > 5
         then step2 x
         else yield x
    step2 x = do
      (val, remainder) <- foldConsecutive f x
      yield val
      step1 remainder
    f acc x = if x > 10 then Just (acc + x) else Nothing

mapIn f = mapTrigger f id
mapOut f = mapTrigger id f

asMaybes :: (Monad m) => (a -> Maybe a) -> T (Maybe a, a) b m r -> T a b m r
asMaybes f = mapIn (\x -> (f x, x))

whileJustT :: (Monad m) => T (Maybe a) b m [a]
whileJustT = whileJustT' []
  where whileJustT' acc = do
          x <- await
          case x of
            Nothing -> return acc
            Just x' -> whileJustT' (acc ++ [x'])

checker1 x = if x >= 5 then Just x else Nothing

foldLikeComm = forever ((sum <$> mapIn checker1 whileJustT) >>= yield)

foldConsecutive :: (Monad m) => (acc -> a -> Maybe acc) -> acc -> T a b m (acc, a)
foldConsecutive f acc = do
  x <- await
  case f acc x of
    Nothing   -> return (acc, x)
    Just acc' -> foldConsecutive f acc'
-}

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
    ]
  , testGroup "Control.Trigger.Parser (oneshot)"
    [ testProperty "Mplus right identity" $
      \p -> oneshot (parse ((aParser_ p) `mplus` mzero)) =!= oneshot (parse (aParser_ p))
    , testProperty "Mplus left identity" $
      \p -> oneshot (parse (mzero `mplus` (aParser_ p))) =!= oneshot (parse (aParser_ p))
    , testProperty "Mplus assoc" $
      \p1 p2 p3 -> oneshot (parse ((aParser_ p1 `mplus` aParser_ p2) `mplus` aParser_ p3)) =!= oneshot (parse (aParser_ p1 `mplus` (aParser_ p2 `mplus` aParser_ p3)))
    , testProperty "<|> right identity" $
      \p -> oneshot (parse (aParser_ p <|> empty)) =!= oneshot (parse (aParser_ p))
    , testProperty "<|> left identity" $
      \p -> oneshot (parse (empty <|> aParser_ p)) =!= oneshot (parse (aParser_ p))
    , testProperty "<|> assoc" $
      \p1 p2 p3 -> oneshot (parse ((aParser_ p1 <|> aParser_ p2) <|> aParser_ p3)) =!= oneshot (parse (aParser_ p1 <|> (aParser_ p2 <|> aParser_ p3)))
    ]
  , testGroup "Control.Trigger.Parser (permanent)"
    [ testProperty "Mplus right identity" $
      \p -> permanent (parse ((aParser_ p) `mplus` mzero)) =!= permanent (parse (aParser_ p))
    , testProperty "Mplus left identity" $
      \p -> permanent (parse (mzero `mplus` (aParser_ p))) =!= permanent (parse (aParser_ p))
    , testProperty "Mplus assoc" $
      \p1 p2 p3 -> permanent (parse ((aParser_ p1 `mplus` aParser_ p2) `mplus` aParser_ p3)) =!= permanent (parse (aParser_ p1 `mplus` (aParser_ p2 `mplus` aParser_ p3)))
    , testProperty "<|> right identity" $
      \p -> permanent (parse (aParser_ p <|> empty)) =!= permanent (parse (aParser_ p))
    , testProperty "<|> left identity" $
      \p -> permanent (parse (empty <|> aParser_ p)) =!= permanent (parse (aParser_ p))
    , testProperty "<|> assoc" $
      \p1 p2 p3 -> permanent (parse ((aParser_ p1 <|> aParser_ p2) <|> aParser_ p3)) =!= permanent (parse (aParser_ p1 <|> (aParser_ p2 <|> aParser_ p3)))
    ]
  ]

tp1 :: (Monad m) => Parser Int m ()
tp1 = fetch >> fetch >> mzero

tp2 :: (Monad m) => Parser Int m ()
tp2 = fetch >>= guard . even >> fetch >> return ()

-- Use the stateful version of the tests
resultOf = resultOfS
aTrigger = aTriggerS
aTrigger_ = aTriggerS_
(=!=) = strongEquiv resultOfS
(=?=) = weakEquiv resultOfS

resultOfI :: T a a Identity () -> [a] -> [a]
resultOfI trigger input =
  let runTrigger t i = execTrigger t i >>= return . fst
  in runIdentity $ runTrigger trigger input

resultOfS :: T a a (State Int) () -> [a] -> [a]
resultOfS trigger input =
  let runTrigger t i = execTrigger t i >>= return . fst
  in evalState (runTrigger trigger input) 0

-- | Strong equivalence of triggers (i.e. for any input, both will yield the
--   same result.
strongEquiv f t1 t2 = \xs -> f t1 xs === f t2 xs

-- | Weak equivalence of triggers (i.e. the shorter of both results will be a
--   prefix of the longer.
weakEquiv f t1 t2 = \xs -> whenFail (putStrLn ("A: " ++ show (f t1 xs) ++ ", B: " ++ show (f t2 xs))) $ cpfx (f t1 xs) (f t2 xs)
  where
    cpfx xs [] = property True
    cpfx [] ys = property True
    cpfx (x:xs) (y:ys) = x === y .&&. cpfx xs ys

infixr 1 =!=
infixr 1 =?=

-- Some very basic triggers
passthruT     = forever $ await >>= yield
nullT :: (Monad m) => T Int Int m r
nullT         = forever $ await
constT x      = forever $ await >> yield x
listT []      = await >> listT []
listT (x:xs)  = await >> yield x >> listT xs
-- alwaysFailT   = forever $ parseU $ fetch >> mzero
-- alwaysPassT   = forever $ parseU fetch

-----------------------------------------------------------------------------
--- ARBITRARY TRIGGERS

data TStep =
    TStepYield
  | TStepAwait
  | TStepInc
  | TStepSet Int
  deriving (Show)

instance Arbitrary TStep where
  arbitrary = resize 5 $ frequency
    [ (100, elements [TStepYield, TStepAwait, TStepInc])
    , (20, TStepSet <$> arbitrary)
    ]

aTriggerS_ :: ([TStep], Int) -> T Int Int (State Int) ()
aTriggerS_ (l, i) = void $ aTriggerS (l, i)

aTriggerS :: ([TStep], Int) -> T Int Int (State Int) Int
aTriggerS (l, i) = (foldr (>=>) return $ map f l) i
  where
    f x = case x of
            TStepYield  -> \y -> yield y >> lift get
            TStepAwait  -> \_ -> await
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

data PStep =
    PStepFetch
  | PStepIsEven
  | PStepFail
  deriving (Show)

instance Arbitrary PStep where
  arbitrary = elements [PStepFetch, PStepIsEven, PStepFail]

aParser_ :: (Monad m) => [PStep] -> Parser Int m ()
aParser_ = void . aParser

aParser :: (Monad m) => [PStep] -> Parser Int m Int
aParser l = fetch >>= (foldr (>=>) return $ map f l)
  where
    f x = case x of
            PStepFetch  -> \y -> fetch >>= return . (+y)
            PStepIsEven -> \y -> guard (even y) >> return y
            PStepFail   -> \y -> mzero
