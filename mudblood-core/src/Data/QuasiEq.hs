-- | Simulated equality.
module Data.QuasiEq
    ( QuasiEq, updateQuasiEq, quasiEq, unQuasiEq, quasiEqWith )
    where

data QuasiEq a = QuasiEq Int a

instance Eq (QuasiEq a) where
    (QuasiEq n a) == (QuasiEq n' a') = n == n'

updateQuasiEq :: (a -> a) -> QuasiEq a -> QuasiEq a
updateQuasiEq f (QuasiEq n a) = QuasiEq (n + 1) (f a)

quasiEq :: a -> QuasiEq a
quasiEq a = QuasiEq 0 a

unQuasiEq :: QuasiEq a -> a
unQuasiEq (QuasiEq _ a) = a

quasiEqWith :: a -> QuasiEq b -> QuasiEq a
quasiEqWith a (QuasiEq m b) = QuasiEq m a
