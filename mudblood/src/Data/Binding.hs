module Data.Binding
  ( Binding
  , empty, step, insert
  ) where

newtype Binding k v = Binding { getBinding :: M.Map k (String, Either (Binding v) v) }

empty :: Binding k v
empty = Binding M.empty

step :: k -> Binding v -> Maybe (Either (Binding v) v)
step k b = fmap (fmap snd) $ M.lookup k $ getKeybind b

insert :: [k] -> String -> v -> Binding k v -> Binding k v
insert [] desc v b = b
insert (k:[]) desc v b = Binding (M.insert k (desc, Right v) b)
insert (k:ks) desc v b = Binding (M.alter (insertInto ks desc v) k b)
  where
    insertInto ks desc v Nothing = Just (desc, insert ks v M.empty)
    insertInto ks desc v (Just (desc', Left b)) = Just (desc', Left (insert ks desc v b))
    insertInto ks desc v (Just (desc', Right v')) = Just (desc', Right (insert ks desc v M.empty))
