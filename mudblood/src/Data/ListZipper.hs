{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Data.ListZipper
  ( Zipper
  , empty, length
  , fromList, fromListAt, toList
  , listLeft, listRight
  , indexListLeft, indexListRight
  , cons, cons'
  , discard
  , left, right
  , left', right'
  , lefts, rights
  , start, end
  , leftp, rightp
  , matchLeft, matchRight
  , destructLeft, destructRight
  , leftFocus, rightFocus
  , deleteLeft, deleteRight
  , insertLeft, insertRight
  ) where

import Prelude hiding (length)
import qualified Prelude

import Data.Foldable hiding (toList, length)
import Data.Traversable

data Zipper a = Zip ![a] ![a]
  deriving (Functor, Foldable, Traversable)

empty :: Zipper a
empty = Zip [] []

length :: Zipper a -> Int
length (Zip l r) = Prelude.length l + Prelude.length r

fromList :: [a] -> Zipper a
fromList xs = Zip [] xs

fromListAt :: Int -> [a] -> Zipper a
fromListAt i xs = Zip (take i xs) (drop i xs)

toList :: Zipper a -> [a]
toList (Zip l r) = reverse l ++ r

listLeft :: Zipper a -> [a]
listLeft (Zip xs _) = reverse xs

listRight :: Zipper a -> [a]
listRight (Zip _ xs) = xs

indexListLeft :: Zipper a -> [(a, Int)]
indexListLeft (Zip l r) = reverse (zip l [0, -1 ..]) ++ zip r [1..]

indexListRight :: Zipper a -> [(a, Int)]
indexListRight (Zip l r) = reverse (zip l [-1, -2 ..]) ++ zip r [0..]

cons :: a -> Zipper a -> Zipper a
cons x (Zip l r) = Zip (l ++ [x]) r

cons' :: a -> Zipper a -> Zipper a
cons' x z = if leftp z then left (cons x z) else cons x z

discard :: Zipper a -> Zipper a
discard (Zip l r) = Zip [] r

left :: Zipper a -> Zipper a
left (Zip [] r) = Zip [] r
left (Zip (l:ls) r) = Zip ls (l:r)

right :: Zipper a -> Zipper a
right (Zip l []) = Zip l []
right (Zip l (r:rs)) = Zip (r:l) rs

left' :: Zipper a -> Zipper a
left' z | leftp z = end z
        | otherwise = left z

right' :: Zipper a -> Zipper a
right' z | rightp z  = start z
         | otherwise = right z

lefts :: Int -> Zipper a -> Zipper a
lefts x z = iterate left z !! x

rights :: Int -> Zipper a -> Zipper a
rights x z = iterate right z !! x

start :: Zipper a -> Zipper a
start z | leftp z   = z
        | otherwise = start (left z)

end :: Zipper a -> Zipper a
end z | rightp z  = z
      | otherwise = end (right z)

leftp :: Zipper a -> Bool
leftp (Zip [] _) = True
leftp _ = False

rightp :: Zipper a -> Bool
rightp (Zip _ []) = True
rightp _ = False

matchLeft :: Zipper a -> Maybe (a, Zipper a)
matchLeft (Zip [] _) = Nothing
matchLeft (Zip (l:ls) r) = Just (l, Zip ls r)

matchRight :: Zipper a -> Maybe (a, Zipper a)
matchRight (Zip _ []) = Nothing
matchRight (Zip l (r:rs)) = Just (r, Zip l rs)

destructLeft :: Zipper a -> ([a], Maybe a, [a])
destructLeft (Zip [] r) = ([], Nothing, r)
destructLeft (Zip (l:ls) r) = (ls, Just l, r)

destructRight :: Zipper a -> ([a], Maybe a, [a])
destructRight (Zip l []) = (l, Nothing, [])
destructRight (Zip l (r:rs)) = (l, Just r, rs)

leftFocus :: Zipper a -> Maybe a
leftFocus = fmap fst . matchLeft

rightFocus :: Zipper a -> Maybe a
rightFocus = fmap fst . matchRight

deleteLeft :: Zipper a -> Zipper a
deleteLeft (Zip [] b) = Zip [] b
deleteLeft (Zip (x:xs) b) = Zip xs b

deleteRight :: Zipper a -> Zipper a
deleteRight (Zip a []) = Zip a []
deleteRight (Zip a (x:xs)) = Zip a xs

insertLeft :: a -> Zipper a -> Zipper a
insertLeft x (Zip a b) = Zip (x:a) b

insertRight :: a -> Zipper a -> Zipper a
insertRight x z = left $ insertLeft x z
