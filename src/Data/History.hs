module Data.History
    ( History
    , empty, cursor
    , up, down
    , insert, rewind
    ) where

data History a = History [a] [a]

empty :: History a
empty = History [] []

cursor :: History a -> Maybe a
cursor (History [] _) = Nothing
cursor (History (x:_) _) = Just x

up :: History a -> History a
up (History l []) = History l []
up (History l (x:xs)) = History (x:l) xs

down :: History a -> History a
down (History [] r) = History [] r
down (History (x:xs) r) = History xs (x:r)

insert :: a -> History a -> History a
insert v (History l r) = History l (v:r)

rewind :: History a -> History a
rewind (History l r) = History [] (reverse l ++ r)
