-- | A very basic Trie implementation that uses arbitrary lists as keys.
module Data.Trie
    ( Trie
    , empty, step, extract, insert
    , foldStep, isPrefix, lookup
    ) where

import Prelude hiding (lookup)
--import Control.Comonad
import Data.Monoid
import Control.Monad
import Data.Maybe
import Control.Applicative hiding (empty)

data Trie k v = Trie v (k -> Maybe (Trie k v))

instance (Show k, Show v) => Show (Trie k v) where
    show (Trie k v) = "Trie " ++ show k

instance Functor (Trie k) where
    fmap f (Trie v g) = Trie (f v) $ fmap (fmap f) . g

{-
instance Comonad (Trie k) where
    extract (Trie v f) = v
    extend f node@(Trie v g) = Trie (f node) (extend f . g)
    --duplicate (Trie v f) = Trie (Trie v f) (duplicate . f)
-}

empty :: (Monoid v) => Trie k v
empty = Trie mempty (const Nothing)

step :: Trie k v -> k -> Maybe (Trie k v)
step (Trie v f) k = f k

extract :: Trie k v -> v
extract (Trie v f) = v

insert :: (Eq k, Monoid v) => [k] -> v -> Trie k v -> Trie k v
insert [] newval (Trie v f) = Trie newval f
insert (k:ks) newval (Trie v f) = Trie v newfun
  where
    newfun x
        | x == k    = case f x of
                        Nothing -> Just $ insert ks newval empty
                        Just t  -> Just $ insert ks newval t
        | otherwise = f x

foldStep :: Trie k v -> [k] -> Maybe (Trie k v)
foldStep = foldM step

isPrefix :: Trie k v -> [k] -> Bool
isPrefix trie = isJust . foldStep trie

lookup :: (Monoid v) => Trie k v -> [k] -> v
lookup trie str = fromMaybe mempty $ fmap extract $ foldStep trie str
