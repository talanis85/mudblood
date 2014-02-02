module Data.PrefixZipper
    ( PrefixZipper (..)
    , empty
    , fromList, fromList', toList
    , toParts
    , cursor
    , setPrefix
    , right
    ) where

import Data.String.Utils
import Data.Monoid

data PrefixZipper a = PrefixZipper
    { prefix :: [a]
    , backward :: [[a]]
    , forward :: [[a]]
    }

empty :: PrefixZipper a
empty = PrefixZipper mempty [] []

fromList :: [[a]] -> PrefixZipper a
fromList l = PrefixZipper [] [] l

fromList' :: [[a]] -> PrefixZipper a
fromList' l = PrefixZipper mempty (reverse l) []

toList :: (Eq a) => PrefixZipper a -> [[a]]
toList z = filter (startswith $ prefix z) $ (backward z) ++ (forward z)

toParts :: (Eq a) => PrefixZipper a -> ([[a]], Maybe [a], [[a]])
toParts z = let f = filter $ startswith $ prefix z
            in ((reverse $ f $ backward z), cursor z, (f $ drop 1 $ forward z))

cursor :: (Eq a) => PrefixZipper a -> Maybe [a]
cursor z = case forward z of
    (x:_) -> Just x
    [] -> Nothing

setPrefix :: (Eq a) => [a] -> PrefixZipper a -> PrefixZipper a
setPrefix str z = updateCursor $ z { prefix = str }

updateCursor :: (Eq a) => PrefixZipper a -> PrefixZipper a
updateCursor z = case forward z of
    [] -> right z
    (x:xs) -> if not (startswith (prefix z) x) then right z else z

{-
left :: (Eq a) => PrefixZipper a -> PrefixZipper a
left z = case toList z of
    [] -> z
    l  -> left' z
  where
    left' z = case backward a of
        [] -> left' $ PrefixZipper (reverse $ forward a) []
        (x:xs) -> let next = PrefixZipper (prefix z) xs (x:(forward a))
                  in if x `startswith` prefix z
                        then next
                        else left' next
-}

right :: (Eq a) => PrefixZipper a -> PrefixZipper a
right z = case toList z of
    [] -> z
    l  -> right' z
  where
    right' z = let next = case forward z of
                            [] -> PrefixZipper (prefix z) [] (reverse $ backward z)
                            [x] -> PrefixZipper (prefix z) [] (reverse $ x : backward z)
                            (x:x':xs) -> PrefixZipper (prefix z) (x : (backward z)) (x' : xs)
               in if startswith (prefix z) (head $ forward next)
                    then next
                    else right' next

