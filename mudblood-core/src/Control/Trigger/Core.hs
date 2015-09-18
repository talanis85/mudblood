{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving #-}

module Control.Trigger.Core
    ( module Control.Applicative
    , module Control.Monad
    , module Control.Monad.Trans

    , T, Trigger, execTrigger, streamTrigger, execTriggerS
    , (>->)
    , (>-->), chain
    , distribute
    , Triggering (..)
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

import Control.Trigger.Forking

-----------------------------------------------------------------------------

data TF a b x =
    TYield b x
  | TAwait (a -> x)
  | TFeed a x

instance Functor (TF a b) where
  fmap f (TYield b x) = TYield b (f x)
  fmap f (TAwait g)   = TAwait (f . g)
  fmap f (TFeed a x)  = TFeed a (f x)

newtype T a b m r = T { unT :: Forking (FreeT (TF a b) m) r }
  deriving (Functor, Applicative, Monad, MonadFree (TF a b))

-----------------------------------------------------------------------------

runT = runFreeT . runForking . unT

instance (MonadIO m) => MonadIO (T a b m) where
  liftIO = lift . liftIO

instance MonadTrans (T a b) where
  lift x = T $ lift $ lift x

instance MFunctor (T a b) where
  hoist f t = T $ hoist (hoist f) (unT t)

instance (Functor f) => MFunctor (FreeT f) where
  hoist = hoistFreeT

execTrigger :: (Monad m) => T a a m () -> [a] -> m ([a], Maybe (T a a m ()))
execTrigger = execTrigger' []
  where
    execTrigger' rs t v = do
      r' <- runT t
      case r' of
          Pure (Right ()) -> return (rs, Nothing)
          Pure (Left (m, c)) -> execTrigger' rs (T m >--> T c) v
          -- Pure (Left (m, c)) -> execTrigger' rs (T c >--> T m) v
          Free (TAwait f) -> case v of
              []     -> return (rs, Just $ T $ Forking $ wrap (TAwait f))
              (x:xs) -> execTrigger' rs (T (Forking (f x))) xs
          Free (TYield x f) -> execTrigger' (rs ++ [x]) (T (Forking f)) v
          Free (TFeed x f) -> execTrigger' rs (T (Forking f)) (x:v)

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
    fork :: t () -> t ()

-----------------------------------------------------------------------------

instance (Monad m) => Triggering a b (T a b m) where
    yield x = liftF $ TYield x ()
    await = liftF $ TAwait id
    feedback x = liftF $ TFeed x ()
    fork f = T $ fork_ (unT f)

-----------------------------------------------------------------------------

(>->) :: (Monad m) => T a a m () -> T a a m () -> T a a m ()
a >-> b = T $ combine (unT a) (unT b)
  where
    combine a b = Forking $ FreeT $ do
      rb <- runFreeT $ runForking b
      let rrb = Forking $ FreeT $ return rb
      runFreeT $ case rb of
        Pure (Right ()) -> return $ Right ()
        Pure (Left (m, c)) -> runForking $ unT $ T a >-> (T m >--> T c)
        Free (TYield x f) -> wrap $ TYield x (runForking $ a `combine` (Forking f))
        Free (TFeed x f)  -> wrap $ TFeed x (runForking $ a `combine` (Forking f))
        Free (TAwait f)   -> FreeT $ do
          ra <- runFreeT $ runForking a
          runFreeT $ case ra of
            Pure (Right ()) -> return $ Right ()
            Pure (Left (m, c)) -> runForking $ unT $ (T m >--> T c) >-> T rrb
            Free (TYield x g) -> runForking $ Forking g `combine` Forking (f x)
            Free (TFeed x g)  -> wrap $ TFeed x (runForking $ Forking g `combine` rrb)
            Free (TAwait g) -> wrap $ TAwait $ \x -> runForking $ Forking (g x) `combine` rrb

(>-->) :: (Monad m) => T a a m () -> T a a m () -> T a a m ()
a >--> b = T $ combine (unT a) (unT b)
  where
    combine a b = Forking $ FreeT $ do
      rb <- runFreeT $ runForking b
      let rrb = Forking $ FreeT $ return rb
      runFreeT $ case rb of
        Pure (Right ()) -> runForking a
        Pure (Left (m, c)) -> runForking $ a `combine` (m `combine` c)
        Free (TYield x f) -> wrap $ TYield x (runForking $ a `combine` (Forking f))
        Free (TFeed x f)  -> wrap $ TFeed x (runForking $ a `combine` (Forking f))
        Free (TAwait f)   -> FreeT $ do
          ra <- runFreeT $ runForking a
          runFreeT $ case ra of
            Pure (Right ()) -> wrap $ TAwait f
            Pure (Left (m, c)) -> runForking $ (m `combine` c) `combine` rrb
            Free (TYield x g) -> runForking $ Forking g `combine` Forking (f x)
            Free (TFeed x g)  -> wrap $ TFeed x (runForking $ Forking g `combine` rrb)
            Free (TAwait g) -> wrap $ TAwait $ \x -> runForking $ Forking (g x) `combine` rrb

chain :: (Monad m) => [T a a m ()] -> T a a m ()
chain = mconcat

instance (Monad m) => Monoid (T a a m ()) where
  mempty = forever $ await >>= yield
  mappend = (>-->)

distribute :: (MonadTrans t, MFunctor t, Monad m, Monad (t m), Monad (t (T a a m))) => T a a (t m) () -> t (T a a m) ()
distribute t = do
  r' <- hoist lift $ runT t
  case r' of
      Pure (Right ()) -> return ()
      Pure (Left (m, c)) -> distribute (T m >--> T c)
      Free (TAwait f) -> lift await >>= \x -> distribute (T (Forking (f x)))
      Free (TYield x f) -> lift (yield x) >> distribute (T (Forking f))
      Free (TFeed x f) -> lift (feedback x) >> distribute (T (Forking f))

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
