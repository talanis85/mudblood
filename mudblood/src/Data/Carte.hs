{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
  , TypeFamilies, TypeOperators, OverlappingInstances
  , ScopedTypeVariables
  , DeriveFunctor
  , BangPatterns
  , Rank2Types #-}

-- | Data types a la carte

module Data.Carte
  ( Fix (..)
  , Nil (..)
  , nilLens
  , mapFix
  , (:<:) (..)
  , (:+:) (..)
  , (:*:) (..), lensL, lensR
  , (:@:) (..), clens
  , (:<@:) (..), prefixLens
  , rec
  , compUserState, decompLens
  -- , Subtype (..), subtype
  , prjM
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Applicative

newtype Fix f = Fix { unFix :: f (Fix f) }

mapFix :: (Functor f, Functor g) => (forall a. f a -> g a) -> Fix f -> Fix g
mapFix f x = Fix (f (fmap (mapFix f) (unFix x)))

data (f :+: g) e = Inl (f e) | Inr (g e)
infixr 4 :+:

data (f :*: g) e = Prod !(f e) !(g e)
infixr 4 :*:

data Nil a = Nil
  deriving (Functor)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr x) = Inr (fmap f x)

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap f (Prod x y) = Prod (fmap f x) (fmap f y)

lensL :: Lens' ((f :*: g) a) (f a)
lensL = lens getter setter
  where
    getter (Prod x y) = x
    setter (Prod x y) v = Prod v y

lensR :: Lens' ((f :*: g) a) (g a)
lensR = lens getter setter
  where
    getter (Prod x y) = y
    setter (Prod x y) v = Prod x v

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance (Functor f) => f :<: f where
  inj = id
  prj = Just . id

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  prj x = case x of
            Inl x -> Just x
            _ -> Nothing

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  prj x = case x of
            Inr x -> prj x
            _ -> Nothing

prjM :: (MonadPlus m, sub :<: sup) => sup a -> m (sub a)
prjM = maybe mzero return . prj

---

class (Functor f, Functor t) => t :@: f where
  getElem :: f a -> t a
  updateElem :: f a -> t a -> f a

clens :: (t :@: f) => Lens' (f a) (t a)
clens = lens getElem updateElem

restlens :: Lens' ((f :*: g) a) (g a)
restlens = lens getter setter
  where
    getter (Prod x y) = y
    setter (Prod x y) v = Prod x v

instance (Functor f) => Nil :@: f where
  getElem _ = Nil
  updateElem x _ = x

instance (Functor f) => f :@: f where
  getElem x = x
  updateElem _ x = x

instance (Functor f, Functor g) => f :@: (f :*: g) where
  getElem (Prod x _) = x
  updateElem (Prod a b) x = Prod x b

instance (Functor f, Functor g, Functor h, h :@: g) => h :@: (f :*: g) where
  getElem (Prod _ x) = getElem x
  updateElem (Prod a b) x = Prod a (updateElem b x)

type Test = Const Int :*: Const String :*: Const String

class (Functor f, Functor g) => f :<@: g where
  getPrefix :: g a -> f a
  updatePrefix :: g a -> f a -> g a

instance (Functor f) => f :<@: f where
  getPrefix = id
  updatePrefix = const id

{-
instance (Functor f, Functor g) => f :<@: (g :*: f) where
  getPrefix (Prod _ x) = x
  updatePrefix (Prod a b) x = Prod a x
-}

instance (Functor f, Functor g, Functor h, f :<@: h) => f :<@: (g :*: h) where
  getPrefix (Prod _ x) = getPrefix x
  updatePrefix (Prod a b) x = Prod a (updatePrefix b x)

prefixLens :: forall f g. (Functor f, Functor g, f :<@: g) => Lens' (Fix g) (Fix f)
prefixLens = lens get set
  where
    set :: Fix g -> Fix f -> Fix g
    set x y =
        let f1 :: Fix g -> f (Fix g) -> Fix g
            f1 u v = Fix (updatePrefix (unFix u) v)
        in cata (f1 x) y
    get :: Fix g -> Fix f
    get x = mapFix getPrefix x

rec :: (g :@: f) => Lens' (Fix f) (g (Fix f))
rec = fixLens . clens

nilLens :: Lens' (Fix Nil) ()
nilLens = lens (const ()) (\_ _ -> Fix Nil)

fixLens :: Lens' (Fix f) (f (Fix f))
fixLens = lens unFix (\_ x -> Fix x)

unfixLens :: Lens' (f (Fix f)) (Fix f)
unfixLens = lens Fix (\_ x -> unFix x)

compUserState :: (Functor f, Functor g) => (forall a. g a) -> Fix f -> Fix (g :*: f)
compUserState v x = mapFix (\y -> Prod v y) x

cata :: (Functor f) => (f a -> a) -> (Fix f -> a)
cata phi = self
  where
    self = phi . fmap self . unFix
{-# INLINE [0] cata #-}

decompLens :: forall f g. (Functor f, Functor g) => Lens' (Fix (f :*: g)) (Fix g)
decompLens = lens get set
  where
    set :: Fix (f :*: g) -> Fix g -> Fix (f :*: g)
    set x y =
        let f1 :: Fix (f :*: g) -> g (Fix (f :*: g)) -> Fix (f :*: g)
            f1 u v = Fix (Prod (view lensL (unFix u)) v)
        in cata (f1 x) y
    get :: Fix (f :*: g) -> Fix g
    get x = mapFix (\(Prod r rs) -> rs) x

{-
class Subtype sub sup where
  coerce :: sub a -> sup a
  reduce :: sup a -> Maybe (sub a)

instance (Functor f, Functor g) => Subtype g (f :+: g) where
  coerce sub = Inr sub
  reduce sup = case sup of
                 Inl x -> Nothing
                 Inr x -> Just x

instance (Functor f, Functor g, Functor h, Subtype f g) => Subtype f (h :+: g) where
  coerce sub = Inr (coerce sub)
  reduce sup = case sup of
                 Inl x -> Nothing
                 Inr x -> reduce x

subtype :: (Subtype f g) => Lens' (Fix g) (Fix f)
subtype = lens (mapFix coerce) (\f g -> mapFix (fromMaybe g . reduce) f)
-}
