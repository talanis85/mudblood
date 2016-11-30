{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}
module Mudblood.Screen.Vty.UserWidget
  ( UserWidget
  , selectList
  , tableEditor
  , tableToggle
  , UnsafeUserWidget, toUnsafeUserWidget, fromUnsafeUserWidget
  ) where

import Control.Applicative
import Control.Interactive
import Control.Comonad
import Data.Bifunctor
import qualified Data.Traversable as T
import qualified Data.ListZipper as Z
import Control.Monad
import Control.Monad.State
import Control.Lens

import Mudblood.Text
import Mudblood.Keys
import Mudblood.Screen.Vty.Layout

import Unsafe.Any

import Safe

type UserWidget m = FocusInteractive m Key (m Layout)

selector :: (Monad m) => Int -> Interactive m Key ([a] -> Z.Zipper a)
selector selection = mkInteractive view trans selection
  where view s  = \x -> if length x == 0 then Z.empty else Z.fromListAt (s `mod` length x) x
        trans s = \e -> case e of
                           KDown  -> Just $ return (s + 1)
                           KUp    -> Just $ return (s - 1)
                           _      -> Nothing

on :: (Monad m) => (e -> a -> Maybe (m ())) -> Interactive m e a -> Interactive m e a
on f i = mkInteractive' view trans i
  where view i = current i
        trans x i e = case f e x of
                         Nothing -> transition i e
                         Just act -> Just $ act >> return i

zipperAction :: (Monad m) => Interactive m Key (m (Z.Zipper (a, m ()))) -> Interactive m Key (m (Z.Zipper a))
zipperAction i = liftM (fmap fst) <$> on handler i
  where handler KEnter x = Just $ do
                              x' <- x
                              case Z.rightFocus x' of
                                Nothing -> return ()
                                Just f  -> snd f
        handler _ _ = Nothing

focusList :: (Monad m) => m (Z.Zipper AttrString) -> Bool -> m Layout
focusList z focus = do
  (up, f, down) <- liftM Z.destructRight z
  case f of
    Nothing -> return $ LayoutList $ up ++ down
    Just f' -> if focus
                  then return $ LayoutList $ up ++ [setStyle StyleReverse f'] ++ down
                  else return $ LayoutList $ up ++ [f'] ++ down

focusLabels :: (Monad m) => m (Z.Zipper (String, AttrString)) -> Bool -> m Layout
focusLabels z focus = do
  (up, f, down) <- liftM Z.destructRight z
  case f of
    Nothing -> return $ LayoutLabels $ up ++ down
    Just f' -> if focus
                  then return $ LayoutLabels $ up ++ [second (setStyle StyleReverse . (<> toAS "~")) f'] ++ down
                  else return $ LayoutLabels $ up ++ [f'] ++ down

selectList :: (Monad m) => Int -> m [(AttrString, m ())] -> UserWidget m
selectList selection getter =
  focusList <$> zipperAction (fmap liftM (selector selection) <*> pure getter)

focusHandler h k z = case Z.rightFocus z of
                       Nothing -> Nothing
                       Just f  -> h k f

-- lensEditor :: (Monad m, MonadState s m) => Interactive m Key (Z.Zipper (String, Lens' s String)) -> Interactive m Key (Z.Zipper (String, Lens' s String))
lensEditor i = on (focusHandler handler) i
  where handler (KAscii c) (_, l) = Just $ cloneLens l %= (++ [c])
        handler KBS (_, l)        = Just $ cloneLens l %= (\x -> take (length x - 1) x)
        handler KEnter (_, l)     = Just $ cloneLens l %= (++ ['\n'])
        handler _ _               = Nothing

lensToggler act i = on (focusHandler handler) i
  where handler (KAscii ' ') (_, l) = Just $ (cloneLens l %= not) >> act
        handler _ _                 = Nothing

-- lensView :: (Monad m, MonadState s m) => [(String, Lens' s String)] -> m [(String, String)]
lensStringView (name, l) = do
  l' <- use $ cloneLens l
  return (name, toAS l')

lensBoolView (name, l) = do
  l' <- use $ cloneLens l
  return (name, if l' then toAS "*" else toAS " ")

tableEditor :: (Monad m, MonadState s m) => [(String, ALens' s String)] -> UserWidget m
tableEditor items = focusLabels <$> T.sequence <$> fmap lensStringView <$> lensEditor (selector 0 <*> pure items)

tableToggle :: (Monad m, MonadState s m) => m () -> [(String, ALens' s Bool)] -> UserWidget m
tableToggle act items = focusLabels <$> T.sequence <$> fmap lensBoolView <$> lensToggler act (selector 0 <*> pure items)

newtype UnsafeUserWidget = UnsafeUserWidget { getUnsafeUserWidget :: UserWidget Any }

toUnsafeUserWidget :: UserWidget m -> UnsafeUserWidget
toUnsafeUserWidget = UnsafeUserWidget . unsafeCoerce

fromUnsafeUserWidget :: UnsafeUserWidget -> UserWidget m
fromUnsafeUserWidget = unsafeCoerce . getUnsafeUserWidget
