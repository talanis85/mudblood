-- | A very basic Trie implementation that uses arbitrary lists as keys.
module Data.Trie
    ( Trie
    , empty, insert, lookup, isPrefix
    ) where

import Prelude hiding (lookup)

newtype Trie k v = Trie { getTrie :: Node k v }

data Node k v = Node (Maybe v) [(k, Node k v)]

-- | The empty trie.
empty :: Trie k v
empty = Trie $ Node Nothing []

-- | Insert a (key, value) tuple. If the key already has a value,
--   it is silently overwritten.
insert :: (Eq k) => Trie k v -> [k] -> v -> Trie k v
insert t k v = Trie $ insert' (getTrie t) k v
    where insert' (Node oldv children) [] newv = Node (Just newv) children
          insert' (Node oldv children) (k:ks) newv = Node oldv (insert'' children k ks newv)
          insert'' [] k ks v = [(k, insert' (Node Nothing []) ks v)]
          insert'' ((a,b):xs) k ks v
            | a == k    = (k, insert' b ks v) : xs
            | otherwise = (a,b) : (insert'' xs k ks v)

-- | Lookup a value for a specific key.
lookup :: (Eq k) => Trie k v -> [k] -> Maybe v
lookup t k = lookup' (getTrie t) k
    where lookup' (Node val children) [] = val
          lookup' (Node val children) (k:ks) = lookup'' children k ks
          lookup'' [] k ks = Nothing
          lookup'' ((a,b):xs) k ks
            | a == k    = lookup' b ks
            | otherwise = lookup'' xs k ks

-- | Check if the trie contains a specific prefix.
isPrefix :: (Eq k) => Trie k v -> [k] -> Bool
isPrefix t k = isPrefix' (getTrie t) k
    where isPrefix' (Node val children) [] = True
          isPrefix' (Node val children) (k:ks) = isPrefix'' children k ks
          isPrefix'' [] k ks = False
          isPrefix'' ((a,b):xs) k ks
            | a == k    = isPrefix' b ks
            | otherwise = isPrefix'' xs k ks
