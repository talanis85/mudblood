module Data.Cache where

import Control.Lens

data Cache a b = Cache (a -> b) a b

instance Functor (Cache a) where
    fmap f (Cache g a _) = cache (f . g) a

update :: (a -> a) -> Cache a b -> Cache a b
update f (Cache g a _) = cache g (f a)

cache :: (a -> b) -> a -> Cache a b
cache f x = Cache f x (f x)

grab :: Cache a b -> b
grab (Cache _ _ x) = x

uncache :: Cache a b -> a
uncache (Cache _ x _) = x

cacher :: Lens' (Cache a b) a
cacher = lens uncache (\x y -> update (const y) x)

grabber :: Getter (Cache a b) b
grabber = to grab
