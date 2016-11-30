{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, StandaloneDeriving, FlexibleContexts, MultiParamTypeClasses #-}
module Control.Interactive
  ( Interactive
  , current, transition
  , FocusInteractive
  , focusSet
  , mkInteractive, mkInteractive'
  ) where

import Data.Monoid
import qualified Data.ListZipper as LZ
import Control.Applicative
import Control.Monad
import Control.Monad.Zip
import Control.Comonad
import Control.Comonad.Cofree

newtype IF m e a = IF { getIF :: e -> Maybe (m a) }

instance (Monad m) => Functor (IF m e) where
  fmap f (IF x) = IF (fmap (fmap (liftM f)) x)

instance (Monad m) => Applicative (IF m e) where
  pure x = IF $ const $ Just $ return x
  f <*> p = IF $ \x -> case (getIF f x, getIF p x) of
                           -- a :: m (a -> b), b :: m a
                           {-
                           (Nothing, Nothing) -> Nothing
                           (Nothing, Just b ) -> Just $ f >>= b
                           (Just a,  Nothing) -> Just $ a >>= p
                           (Just a,  Just b ) -> Just $ liftM2 ($) a b
                           -}
                           (Nothing, _      ) -> Nothing
                           (_,       Nothing) -> Nothing
                           (Just a,  Just b ) -> Just $ liftM2 ($) a b

instance (Monad m) => Alternative (IF m e) where
  empty = IF $ const Nothing
  a <|> b = IF $ \x -> case (getIF a x, getIF b x) of
                         (Nothing, x) -> x
                         (x, y)       -> x

{-
instance (Monad m) => MonadZip (IF m e) where
  -- mzip :: IF m e a -> IF m e b -> IF m e (a, b)
  mzip a b = IF $ \x -> case (getIF a x, getIF b x) of
                          (Nothing, _) -> Nothing
                          (_, Nothing) -> Nothing
                          (Just a, Just b) -> liftM2 (,) a b
-}

{-
just :: (Monad m) => a -> Interactive m e a
-- just x = x :< IF (const $ Just $ return $ just x)
just x = x :< IF (const Nothing)

(<%>) :: (Monad m) => Interactive m e (a -> b) -> Interactive m e a -> Interactive m e b
f@(fa :< fh) <%> a@(aa :< ah) = ba :< bh
  where
    ba = fa aa
    bh = IF $ \x -> case (getIF fh x, getIF ah x) of
                           (Nothing, Nothing) -> Nothing
                           (Nothing, Just r ) -> Just $ liftM2 (<%>) (return f) r
                           (Just l,  Nothing) -> Just $ liftM2 (<%>) l (return a)
                           (Just l,  Just r ) -> Just $ liftM2 (<%>) l r
-}

{-
instance (Monad m) => Applicative (IF m e) where
  pure x = IF $ const $ Just $ return x
  -- <*> :: IF m e (a -> b) -> IF m e a -> IF m e b
  f <*> p = IF $ \x -> case (getIF f x, getIF p x) of
                           -- a :: m (a -> b), b :: m a
                           {-
                           (Nothing, Nothing) -> Nothing
                           (Nothing, Just b ) -> Just $ f >>= b
                           (Just a,  Nothing) -> Just $ a >>= p
                           (Just a,  Just b ) -> Just $ liftM2 ($) a b
                           -}
                           (Nothing, _      ) -> Nothing
                           (_,       Nothing) -> Nothing
                           (Just a,  Just b ) -> Just $ liftM2 ($) a b

instance (Monad m) => Alternative (IF m e) where
  empty = IF $ const Nothing
  a <|> b = IF $ \x -> case (getIF a x, getIF b x) of
                         (Nothing, x) -> x
                         (x, y)       -> x
-}

type Interactive m e = Cofree (IF m e)

current :: (Monad m) => Interactive m e a -> a
current = extract

transition :: (Monad m) => Interactive m e a -> e -> Maybe (m (Interactive m e a))
transition = getIF . unwrap

mkInteractive fc ft s = unfold (\x -> (fc x, IF (ft x))) s

mkInteractive' fc ft s = unfold (\x -> (fc x, IF (ft (fc x) x))) s

type FocusInteractive m e a = Interactive m e (Bool -> a)

focusSet :: (Monad m, Monoid a) => (e -> Bool) -> [Either (Interactive m e (m a)) (FocusInteractive m e (m a))] -> FocusInteractive m e (m a)
focusSet switch ws = unfold focusSet' (LZ.fromList ws)
  where focusSet' z =
          let z' = fixFocus z
              view f =
                let currentWithoutFocus (Left i)  = current i
                    currentWithoutFocus (Right i) = current i False
                    wlist = case LZ.matchRight z' of
                      Nothing             -> map currentWithoutFocus $ LZ.toList z'
                      Just (Right x, z'') -> map currentWithoutFocus (LZ.listLeft z'')
                                          ++ [current x f]
                                          ++ map currentWithoutFocus (LZ.listRight z'')
                in liftM mconcat $ sequence wlist
              trans = IF $ \e -> if switch e
                             then Just $ return $ LZ.right' z'
                             else case LZ.matchRight z' of
                                    Nothing -> Nothing
                                    Just (Right x, z'') -> case transition x e of
                                      Nothing -> Nothing
                                      Just t' -> Just $ do
                                        w' <- t'
                                        return $ LZ.insertRight (Right w') z''
              in (view, trans)
        fixFocus z = case LZ.rightFocus z of
            Just (Left _) -> fixFocus (LZ.right' z)
            Nothing       -> fixFocus (LZ.right' z)
            _             -> z
