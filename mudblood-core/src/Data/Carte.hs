{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts
  , TypeFamilies, TypeOperators, OverlappingInstances #-}

-- | Data types a la carte

module Data.Carte
  ( Fix (..)
  , (:<:) (..)
  , (:+:) (..)
  , prjM
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Applicative

newtype Fix f = Fix { unFix :: f (Fix f) }

data (f :+: g) e = Inl (f e) | Inr (g e)
infixr 4 :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr x) = Inr (fmap f x)

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
