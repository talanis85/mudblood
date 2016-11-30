module Control.Interactive
  ( Interactive (..)
  , FocusInteractive
  , focusSet
  ) where

import Data.Monoid
import qualified Data.ListZipper as LZ
import Control.Applicative
import Control.Monad

{-
data Interactive m e a = Interactive
  { current :: a
  , transition :: e -> Maybe (m (Interactive m e a))
  }
-}

type Interactive m e a = Cofree (InteractiveF m e) a

type InteractiveF m e a = e -> Maybe (m a)

type FocusInteractive m e a = Interactive m e (Bool -> a)

instance (Monad m) => Functor (Interactive m e) where
  fmap f x = Interactive
    { current    = f (current x)
    , transition = fmap (fmap (liftM (fmap f))) (transition x)
    }

{- FUNCTOR LAWS

-- identity

CIH:    forall e. fmap id (t e) = t e -> fmap id (I c t) = I c t

  fmap id (I c t)
= I (id c) (fmap (fmap (liftM (fmap id))) t)
= I c (fmap (fmap (liftM (fmap id))) t)           (id)
= I c (fmap (fmap (liftM id)) t)                  (CIH)
= I c (fmap (fmap id) t)                          (Monad m)
= I c (fmap id t)                                 (Functor Maybe)
= I c (id t)                                      (Functor (->))
= I c t                                           (id)

-- distributivity

CIH:    forall e. fmap (f . g) (t e) = fmap f (fmap g (t e)) -> fmap (f . g) (I c t) = fmap f (fmap g (I c t))

  fmap (f . g) (I c t)
= I (f (g c)) (fmap (fmap (liftM (fmap (f . g)))) t)
= I (f (g c)) (fmap (fmap (liftM (fmap f . fmap g))) t)     (CIH)

  fmap f (fmap g (I c t))
= fmap f (I (g c) (fmap (fmap (liftM (fmap g))) t)
= I (f (g c)) (fmap (fmap (liftM (fmap f))) (fmap (fmap (liftM (fmap g))) t))
  ...

-}

instance (Monad m) => Applicative (Interactive m e) where
  pure x = Interactive
    { current    = x
    , transition = const Nothing
    }
  f <*> x = Interactive
    { current    = current f (current x)
    -- , transition = \e -> liftM2 (<*>) (transition f e) (transition x e)
    , transition = \e -> case (transition f e, transition x e) of
                           (Nothing, Nothing) -> Nothing
                           (Nothing, Just b ) -> Just $ liftM2 (<*>) (return f) b
                           (Just a,  Nothing) -> Just $ liftM2 (<*>) a (return x)
                           (Just a,  Just b ) -> Just $ liftM2 (<*>) a b
    }

instance (Monad m) => Comonad (Interactive m e) where
  extract = current
  duplicate i = Interactive
    { current = i
    , transition = duplicate . transition i
    }

{- COMONAD LAWS

-- left identity

  extract (duplicate (I c t))
= extract (I (I c t) (duplicate . t))
= I c t

-- right identity

  fmap extract (duplicate (I c t))
= fmap extract (I (I c t) (duplicate . t))
= I c (fmap (fmap (liftM (fmap extract))) (\x -> duplicate (t x)))
  ...

...

-}

{-
instance (Monad m) => Monad (Interactive m e) where
  return = pure
  m >>= f = Interactive
    { current    = current (f (current m))
    , transition = \e -> case transition m e of
                           Nothing -> transition (f (current m)) e
                           Just a  -> Just $ return $ transition $ f a
    {-
    , transition = \e -> case (transition m e, transition (f (current m)) e) of
                           (Nothing, Nothing) -> Nothing
                           (Nothing, Just b ) -> Just $ liftM2 (>>=) (return m) b
                           (Just a,  Nothing) -> Just $ liftM2 (>>=) a (return (f (current m)))
                           (Just a,  Just b ) -> Just $ liftM2 (>>=) a b
    -}
    }
-}

-- type IW m = Interactive m Key (m Layout)

focusSet :: (Monad m, Monoid a) => (e -> Bool) -> [Either (Interactive m e (m a)) (FocusInteractive m e (m a))] -> FocusInteractive m e (m a)
focusSet switch ws = fixFocus (LZ.fromList ws)
  where
    fixFocus ws = case LZ.rightFocus ws of
                      Just (Left _) -> fixFocus (LZ.right' ws)
                      Nothing       -> fixFocus (LZ.right' ws)
                      _             -> focusSet' ws
    focusSet' ws = Interactive
      { current = \f ->
                      let currentWithoutFocus (Left i)  = current i
                          currentWithoutFocus (Right i) = current i False
                          wlist = case LZ.matchRight ws of
                            Nothing             -> map currentWithoutFocus $ LZ.toList ws
                            Just (Right x, ws') -> map currentWithoutFocus (LZ.listLeft ws')
                                                ++ [current x f]
                                                ++ map currentWithoutFocus (LZ.listRight ws')
                      in liftM mconcat $ sequence wlist
      , transition  = \e -> if switch e
                               then Just $ return $ fixFocus (LZ.right' ws)
                               else case LZ.matchRight ws of
                                      Nothing -> Nothing
                                      Just (Right x, ws') -> case transition x e of
                                        Nothing -> Nothing
                                        Just t' -> Just $ do
                                          w' <- t'
                                          return $ fixFocus (LZ.insertRight (Right w') ws')
      }

{-
focusSet :: (Monad m, Monoid a) => (e -> Bool) -> [(Bool, FocusInteractive m e (m a))] -> Interactive m e (m a)
focusSet switch ws = focusSet'' (LZ.fromList ws)
  where
    focusSet'' ws = case LZ.rightFocus ws of
                      Just (False, _) -> focusSet'' (LZ.right' ws)
                      _               -> focusSet' ws
    focusSet' ws = Interactive
      { current = let wlist = case LZ.matchRight ws of
                        Nothing -> map (($ False) . current . snd) (LZ.toList ws)
                        Just ((True, x), ws') -> map (($ False) . current . snd) (LZ.listLeft ws')
                                              ++ [(current x True)]
                                              ++ map (($ False) . current . snd) (LZ.listRight ws')
                  in liftM mconcat $ sequence wlist
      , transition = \e -> if switch e
                              then Just $ return $ focusSet'' (LZ.right' ws)
                              else case LZ.matchRight ws of
                                     Nothing -> Nothing
                                     Just ((True, x), ws') -> case transition x e of
                                       Nothing -> Nothing
                                       Just t' -> Just $ do
                                         w' <- t'
                                         return $ focusSet'' (LZ.insertRight (True, w') ws')
      }
-}
