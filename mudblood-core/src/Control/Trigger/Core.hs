{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving #-}

module Control.Trigger.Core
    ( module Control.Applicative
    , module Control.Monad
    , module Control.Monad.Trans

    , T, Trigger, execTrigger, streamTrigger, execTriggerS
    , tightChain, looseChain
    , chain
    , distribute
    , Triggering (..)
    , Chaining (..)
    ) where

-----------------------------------------------------------------------------

import Data.Monoid

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
  | TFeed a x

instance Functor (TF a b) where
  fmap f (TYield b x) = TYield b (f x)
  fmap f (TAwait g)   = TAwait (f . g)
  fmap f (TFeed a x)  = TFeed a (f x)

newtype T a b m r = T { unT :: FreeT (TF a b) m r }
  deriving (Functor, Applicative, Monad, MonadFree (TF a b))

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

execTrigger :: (Monad m) => T a a m () -> [a] -> m ([a], Maybe (T a a m ()))
execTrigger = execTrigger' []
  where
    execTrigger' rs t v = do
      r' <- runT t
      case r' of
          Pure () -> return (rs, Nothing)
          Free (TAwait f) -> case v of
              []     -> return (rs, Just $ T $ wrap (TAwait f))
              (x:xs) -> execTrigger' rs (T (f x)) xs
          Free (TYield x f) -> execTrigger' (rs ++ [x]) (T f) v
          Free (TFeed x f) -> execTrigger' rs (T f) (x:v)

type Trigger a = T a a

execTriggerS :: (Monad m) => T a a m () -> StateT [a] m (Maybe (T a a m ()))
execTriggerS t = do
  cur <- get
  (r, t') <- lift $ execTrigger t cur
  put r
  return t'

streamTrigger :: (Monad m) => T a a m () -> [a] -> m [a]
streamTrigger t i = liftM fst $ execTrigger t i

-----------------------------------------------------------------------------

class (Monad t) => Triggering a b t | t -> a b where
    yield :: b -> t ()
    await :: t a
    feedback :: a -> t ()

class (Monad t) => Chaining t where
    (>->) :: t r -> t r -> t r
    (>-->) :: t r -> t r -> t r

-----------------------------------------------------------------------------

instance (Monad m) => Triggering a b (T a b m) where
    yield x = liftF $ TYield x ()
    await = liftF $ TAwait id
    feedback x = liftF $ TFeed x ()

instance (Monad m) => Chaining (T a a m) where
    (>->) = tightChain
    (>-->) = looseChain

-----------------------------------------------------------------------------

tightChain :: (Monad m) => T a a m r -> T a a m r -> T a a m r
a `tightChain` b = T $ combine (unT a) (unT b)
  where
    combine a b = FreeT $ do
      rb <- runFreeT b
      let rrb = FreeT $ return rb
      runFreeT $ case rb of
        Pure r            -> return r
        Free (TYield x f) -> wrap $ TYield x (a `combine` f)
        Free (TFeed x f)  -> wrap $ TFeed x (a `combine` f)
        Free (TAwait f)   -> FreeT $ do
          ra <- runFreeT a
          runFreeT $ case ra of
            Pure r            -> return r
            Free (TYield x g) -> g `combine` f x
            Free (TFeed x g)  -> wrap $ TFeed x (g `combine` rrb)
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
        Free (TFeed x f)  -> wrap $ TFeed x (a `combine` f)
        Free (TAwait f)   -> FreeT $ do
          ra <- runFreeT a
          runFreeT $ case ra of
            Pure r            -> wrap $ TAwait f
            Free (TYield x g) -> g `combine` f x
            Free (TFeed x g)  -> wrap $ TFeed x (g `combine` rrb)
            Free (TAwait g)   -> wrap $ TAwait $ \x -> g x `combine` rrb

chain :: (Monad m) => [T a a m r] -> T a a m r
chain = mconcat

instance (Monad m) => Monoid (T a a m r) where
  mempty = forever $ await >>= yield
  mappend = (>-->)

distribute :: (MonadTrans t, MFunctor t, Monad m, Monad (t m), Monad (t (T a a m))) => T a a (t m) r -> t (T a a m) r
distribute t = do
  r' <- hoist lift $ runT t
  case r' of
      Pure r            -> return r
      Free (TAwait f)   -> lift await >>= \x -> distribute (T (f x))
      Free (TYield x f) -> lift (yield x) >> distribute (T f)
      Free (TFeed x f)  -> lift (feedback x) >> distribute (T f)

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
