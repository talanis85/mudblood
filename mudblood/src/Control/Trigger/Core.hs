{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving #-}

module Control.Trigger.Core
    ( module Control.Applicative
    , module Control.Monad
    , module Control.Monad.Trans

    , Triggering (..)
    , T, Trigger, execTrigger, evalTrigger, streamTrigger, execTriggerS, mapTrigger
    , (>->)
    , distribute
    , par
    ) where

-----------------------------------------------------------------------------

import Data.Semigroup

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Morph

import Control.Applicative

-----------------------------------------------------------------------------

data TF a b x =
    TYield b x
  | TAwait (a -> x)

instance Functor (TF a b) where
  fmap f (TYield b x) = TYield b (f x)
  fmap f (TAwait g)   = TAwait (f . g)

newtype T a b m r = T { unT :: FreeT (TF a b) m r }
  deriving (Functor, Applicative, Monad, MonadFail, MonadFree (TF a b))

type Trigger a = T a a

-----------------------------------------------------------------------------

runT = runFreeT . unT

instance (MonadIO m) => MonadIO (T a b m) where
  liftIO = lift . liftIO

instance MonadTrans (T a b) where
  lift x = T $ lift x

instance MFunctor (T a b) where
  hoist f t = T $ hoist f (unT t)

instance (Functor f) => MFunctor (FreeT f) where
  hoist = hoistFreeT

execTrigger :: (Monad m) => T a a m () -> [a] -> m ([a], T a a m ())
execTrigger = execTrigger' []
  where
    execTrigger' rs t v = do
      r' <- runT t
      case r' of
          Pure () -> return (rs ++ v, return ())
          Free (TAwait f) -> case v of
              []     -> return (rs, T $ wrap (TAwait f))
              (x:xs) -> execTrigger' rs (T (f x)) xs
          Free (TYield x f) -> execTrigger' (rs ++ [x]) (T f) v

execTriggerS :: (Monad m) => T a a m () -> StateT [a] m (T a a m ())
execTriggerS t = do
  cur <- get
  (r, t') <- lift $ execTrigger t cur
  put r
  return t'

streamTrigger :: (Monad m) => T a a m () -> [a] -> m [a]
streamTrigger t i = liftM fst $ execTrigger t i

evalTrigger :: (Monad m) => T a b m r -> [a] -> m (Either r ([b], T a b m r))
evalTrigger = evalTrigger' []
  where
    evalTrigger' rs t v = do
      r' <- runT t
      case r' of
          Pure r -> return $ Left r
          Free (TAwait f) -> case v of
              []     -> return $ Right (rs, T $ wrap (TAwait f))
              (x:xs) -> evalTrigger' rs (T (f x)) xs
          Free (TYield x f) -> evalTrigger' (rs ++ [x]) (T f) v

mapTrigger :: (Monad m) => (c -> a) -> (b -> d) -> T a b m r -> T c d m r
mapTrigger back forth t =
    let mapTrigger_ t = do
          r <- lift $ runT t
          case r of
            Pure r -> return r
            Free (TAwait f) -> await >>= \x -> mapTrigger_ (T (f (back x)))
            Free (TYield x f) -> yield (forth x) >> mapTrigger_ (T f)
    in mapTrigger_ t

-----------------------------------------------------------------------------

{-
yield :: (Monad m) => b -> T a b m ()
yield x = liftF $ TYield x ()

await :: (Monad m) => T a b m a
await = liftF $ TAwait id

(>-->) :: (Monad m) => T a a m r -> T a a m r -> T a a m r
(>-->) = looseChain
-}

class (Monad t) => Triggering a b t | t -> a b where
    yield :: b -> t ()
    await :: t a

(>->) :: (Monad m) => T a b m r -> T b c m r -> T a c m r
(>->) = tightChain

-----------------------------------------------------------------------------

instance (Monad m) => Triggering a b (T a b m) where
    yield x = liftF $ TYield x ()
    await = liftF $ TAwait id

instance (Monad m) => Semigroup (T a a m r) where
    (<>) = looseChain

instance (Monad m, Monoid r) => Monoid (T a a m r) where
    mempty = return mempty
    mappend = looseChain

-----------------------------------------------------------------------------

tightChain :: (Monad m) => T a b m r -> T b c m r -> T a c m r
a `tightChain` b = T $ combine (unT a) (unT b)
  where
    combine a b = FreeT $ do
      rb <- runFreeT b
      let rrb = FreeT $ return rb
      runFreeT $ case rb of
        Pure r            -> return r
        Free (TYield x f) -> wrap $ TYield x (a `combine` f)
        Free (TAwait f)   -> FreeT $ do
          ra <- runFreeT a
          runFreeT $ case ra of
            Pure r            -> return r
            Free (TYield x g) -> g `combine` f x
            Free (TAwait g)   -> wrap $ TAwait $ \x -> g x `combine` rrb

looseChain :: (Monad m) => T a a m r -> T a a m r -> T a a m r
a `looseChain` b = T $ combine (unT a) (unT b)
  where
    combine a b = FreeT $ do
      rb <- runFreeT b
      let rrb = FreeT $ return rb
      runFreeT $ case rb of
        Pure r            -> a
        Free (TYield x f) -> wrap $ TYield x (a `combine` f)
        Free (TAwait f)   -> FreeT $ do
          ra <- runFreeT a
          runFreeT $ case ra of
            Pure r            -> wrap $ TAwait f
            Free (TYield x g) -> g `combine` f x
            Free (TAwait g)   -> wrap $ TAwait $ \x -> g x `combine` rrb

distribute :: (MonadTrans t, MFunctor t, Monad m, Monad (t m), Monad (t (T a b m))) => T a b (t m) r -> t (T a b m) r
distribute t = do
  r' <- hoist lift $ runT t
  case r' of
      Pure r            -> return r
      Free (TAwait f)   -> lift await >>= distribute . T . f
      Free (TYield x f) -> lift (yield x) >> distribute (T f)

par :: (Monad m) => T a b m r -> T a' b m r -> T (a, a') b m r
par a b = T $ combine (unT a) (unT b)
  where
    combine a b = FreeT $ do
      ra <- runFreeT a
      runFreeT $ case ra of
        Pure r -> return r
        Free (TYield x f) -> wrap $ TYield x (f `combine` b)
        Free (TAwait f) -> FreeT $ do
          rb <- runFreeT b
          runFreeT $ case rb of
            Pure r -> return r
            Free (TYield x g) -> wrap $ TYield x (a `combine` g)
            Free (TAwait g) -> wrap $ TAwait (\(x,y) -> f x `combine` g y)

{- Might have the following properties:

=!=     strong equivalence (same results for same input)
=?=     weak equivalence (same results up to a common prefix)
===     true equality

Left identity       forall t. mempty <> t =!= t                       (tested, to prove)
Right identity      forall t. t <> mempty =!= t                       (tested, to prove)
Associativity       forall a b c. a <> (b <> c) =!= (a <> b) <> c)    (tested, to prove)

...                 forall a b c. a === b  ->  a <> c === b <> c      (to prove) (whats this property called?)
Distributivity      forall a b c. a >> (b <> c) =?= (a >> b) <> (a >> c) (probably false)
-}
