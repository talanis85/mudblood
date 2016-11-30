module Data.List.Flatten
    ( flattenL, flattenL'
    , flattenR, flattenR'
    , flatten )
    where

flatten = flattenN 10

appendLimit :: Int -> a -> [a] -> Either (a, [a]) [a]
appendLimit n x l = if length l == n then Left (head l, tail l ++ [x]) else Right (l ++ [x])

flattenN :: Int -> [Either a b] -> [(Maybe a, Maybe b)]
flattenN n = flatten' [] []
  where
    flatten' ls rs [] = arrange ls rs
    flatten' [] rs ((Right x):xs) = case appendLimit n x rs of
                                  Left (x', rs') -> (Nothing, Just x') : flatten' [] rs' xs
                                  Right rs' -> flatten' [] rs' xs
    flatten' ls rs ((Left x):xs) = flatten' (ls ++ [x]) rs xs
    flatten' ls rs ((Right x):xs) = arrange ls rs ++ flatten' [] [] (Right x : xs)
    flatten' [] [] ((Left x):xs) = (Just x, Nothing) : flatten' [] [] xs
    arrange l r = reverse (arrange' (reverse l) (reverse r))
    arrange' [] [] = []
    arrange' (l:ls) [] = (Just l, Nothing) : arrange ls []
    arrange' [] (r:rs) = (Nothing, Just r) : arrange [] rs
    arrange' (l:ls) (r:rs) = (Just l, Just r) : arrange ls rs

flattenL :: [Either a b] -> [(Maybe a, Maybe b)]
flattenL l = flattenLL [] l

flattenL' :: [Either a b] -> [(Maybe a, Maybe b)]
flattenL' l = flattenLR [] l

flattenLL a [] = fillLefts a
flattenLL a ((Left x) : xs)  = flattenLL (a ++ [x]) xs
flattenLL a ((Right x) : xs) = flattenLR a ((Right x) : xs)

flattenLR ls [] = fillLefts ls
flattenLR [] ((Right x) : xs) = (Nothing, Just x) : flattenLR [] xs
flattenLR (l:ls) ((Right x) : xs) = (Just l, Just x) : flattenLR ls xs
flattenLR [] ((Left x) : xs) = flattenLL [] ((Left x) : xs)
flattenLR (l:ls) ((Left x) : xs) = (Just l, Nothing) : flattenLR ls ((Left x) : xs)

fillLefts = map (\x -> (Just x, Nothing))

flattenR :: [Either a b] -> [(Maybe a, Maybe b)]
flattenR l = flattenRR [] l

flattenR' :: [Either a b] -> [(Maybe a, Maybe b)]
flattenR' l = flattenRL [] l

flattenRR a [] = fillRights a
flattenRR a ((Right x) : xs)  = flattenRR (a ++ [x]) xs
flattenRR a ((Left x) : xs) = flattenRL a ((Left x) : xs)

flattenRL rs [] = fillRights rs
flattenRL [] ((Left x) : xs) = (Just x, Nothing) : flattenRL [] xs
flattenRL (r:rs) ((Left x) : xs) = (Just x, Just r) : flattenRL rs xs
flattenRL [] ((Right x) : xs) = flattenRR [] ((Right x) : xs)
flattenRL (r:rs) ((Right x) : xs) = (Nothing, Just r) : flattenRL rs ((Right x) : xs)

fillRights = map (\x -> (Nothing, Just x))

{-

[Left 5, Right 4, Right 3, Left 2, Left 1] -> []

[Right 4, Right 3, Left 2, Left 1] -> [(Just 5, Nothing)]

[Right 3, Left 2, Left 1] -> [(Just 5, Nothing)]          memo: [4]

[Left 2, Left 1] -> [(Just 5, Nothing)]                   memo: [4,3]

[Left 1] -> [(Just 5, Nothing), (Just 2, Just 4)]         memo: [3]

[] -> [(Just 5, Nothing), (Just 2, Just 4), (Just 1, Just 3)]
-}
