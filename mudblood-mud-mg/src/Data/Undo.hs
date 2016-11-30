module Data.Undo where

import Control.Lens

data Undo a = Undo Int [a] a [a]

undo :: Undo a -> Undo a
undo (Undo n as x []) = Undo n as x []
undo (Undo n as x (b:bs)) = Undo n (x:as) b bs

undoSteps :: Undo a -> Int
undoSteps (Undo _ _ _ u) = length u

redo :: Undo a -> Undo a
redo (Undo n [] x bs) = Undo n [] x bs
redo (Undo n (a:as) x bs) = Undo n as a (x:bs)

redoSteps :: Undo a -> Int
redoSteps (Undo _ r _ _) = length r

current :: Undo a -> a
current (Undo _ _ x _) = x

undoable :: (a -> a) -> Undo a -> Undo a
undoable f (Undo n _ x xs) = Undo n [] (f x) (take n (x:xs))

undoify :: Int -> a -> Undo a
undoify n x = Undo n [] x []

undoer :: Lens' (Undo a) a
undoer = lens current (\x y -> undoable (const y) x)
