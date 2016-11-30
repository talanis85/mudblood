module Mudblood.Screen.Vty.UserWidget
  ( UserWidget
  , selectList
  , zipperMover
  , UnsafeUserWidget, toUnsafeUserWidget, fromUnsafeUserWidget
  ) where

import Control.Applicative
import Control.Interactive
import Data.Bifunctor
import qualified Data.ListZipper as Z
import Control.Monad

import Mudblood.Text
import Mudblood.Keys
import Mudblood.Screen.Vty.Layout

import Unsafe.Any

import Safe

{-
newtype UserWidget m = UserWidget { getUserWidget :: Interactive m Key (m Layout) }
  deriving (Functor, Applicative)
-}

type UserWidget m = FocusInteractive m Key (m Layout)

{-
listWidget :: (Monad m) => m [(AttrString, m ())] -> Interactive m Key (m Layout)
listWidget getter = pure $ liftM (LayoutList . map fst) getter
-}

type Selection a = ([a], Maybe a, [a])

selector :: (Monad m) => Int -> Interactive m Key ([a] -> Z.Zipper a)
selector selection = Interactive
  { current = \x -> if length x == 0 then Z.empty else Z.fromListAt (selection `mod` length x) x
  , transition = \e -> case e of
                         KDown  -> Just $ return $ selector (selection + 1)
                         KUp    -> Just $ return $ selector (selection - 1)
                         _      -> Nothing
  }

{-
getter :: m [a]

w1 :: Interactive m Key (m (Selection a))
w1 = (fmap liftM selector) <*> pure getter

w2 :: Interactive m Key (m (Selection a))
w2 = w1 >>= addAction

addAction :: (Monad m) => m (Selection a) -> Interctive m Key (m (Selection a))
addAction s@(up, f, down) = Interactive
  { current = s
  , transition = \e -> case e of
                         KEnter -> Just $ do
                           doActionOf f
                           return $ addAction s
                         _ -> Nothing
  }

w2 :: Interactive m Key (Bool -> m Layout)
w2 = fmap focusList w1
-}

{-
addAction :: (Monad m) => Selection (a, m ()) -> Interactive m Key (m (Selection (a, m ())))
addAction s@(_, f, _) = Interactive
  { current = return s
  , transition = \e -> case e of
                         KEnter -> case f of
                                     Nothing -> Nothing
                                     Just f' -> Just $ snd f' >> return (addAction s)
                         _ -> Nothing
  }
-}

withAction :: (Monad m) => (a -> m ()) -> Interactive m Key (m (Z.Zipper a)) -> Interactive m Key (m (Z.Zipper a))
withAction act i = Interactive
  { current = current i
  , transition = \e -> case e of
                         KEnter -> Just $ do
                           f <- liftM Z.rightFocus $ current i
                           case f of
                             Nothing -> return (withAction act i)
                             Just f' -> act f' >> return (withAction act i)
                         _ -> case transition i e of
                                Nothing -> Nothing
                                Just x  -> Just $ liftM (withAction act) x
  }

withCurrent :: (Monad m) => (e -> a -> m ()) -> Interactive m e (Z.Zipper a) -> Interactive m e (Z.Zipper a)
withCurrent editor i = Interactive
  { current = current i
  , transition = \e ->
      let gonext = do
            t <- transition i e
            case t of
              Nothing -> return (withCurrent editor i)
              Just i' -> return (withCurrent editor i')
      in case Z.matchRight (current i) of
          (Nothing, _) -> Just gonext
          (Just el, z) ->
            case editor e el of
              Nothing -> Just gonext
              Just new ->
                let i' = i { current = Z.insertRight new z }
                in Just $ return (withCurrent editor i')
  }

stringEditor :: Key -> String -> Maybe String
stringEditor k s =
  case k of
    KAscii c -> return $ s ++ [c]
    KBS      -> return $ take (length s - 1) s
    _        -> Nothing

focusList :: (Monad m) => m (Z.Zipper AttrString) -> Bool -> m Layout
focusList z focus = do
  (up, f, down) <- liftM Z.destructRight z
  case f of
    Nothing -> return $ LayoutList $ up ++ down
    Just f' -> if focus
                  then return $ LayoutList $ up ++ [setStyle StyleReverse f'] ++ down
                  else return $ LayoutList $ up ++ [f'] ++ down

selectList :: (Monad m) => Int -> m [(AttrString, m ())] -> UserWidget m
selectList selection getter =
  focusList <$> liftM (fmap fst) <$> withAction snd (fmap liftM (selector selection) <*> pure getter)

zipperMover :: (Monad m) => Int -> m (Z.Zipper AttrString) -> (Int -> m ()) -> UserWidget m
zipperMover selection getter act =
    focusList <$> liftM (fmap fst) <$> withAction (act . snd) (fmap liftM (selector selection) <*> (liftM Z.indexListRight <$> pure getter))

tableEditor :: (Monad m, MonadState s m) => [(String, Lens' s String)] -> UserWidget m
tableEditor items = fmap liftM (selector 0) <*> pure (return items)

{-
zipperMover :: (Monad m) => Int -> m (ListZipper (AttrString, a)) -> Interactive m Key (Bool -> m Layout)
zipperMover selection getter = Interactive
  { current = \focus ->
-}

newtype UnsafeUserWidget = UnsafeUserWidget { getUnsafeUserWidget :: UserWidget Any }

{-
withFocus :: (Monad m) => UserWidget m -> FocusInteractive m Key (m Layout)
withFocus = fmap addFocus
  where
    addFocus m = \f -> liftM (LayoutFocus f) m
-}

toUnsafeUserWidget :: UserWidget m -> UnsafeUserWidget
toUnsafeUserWidget = UnsafeUserWidget . unsafeCoerce

fromUnsafeUserWidget :: UnsafeUserWidget -> Maybe (UserWidget m)
fromUnsafeUserWidget = Just . unsafeCoerce . getUnsafeUserWidget
