module Mudblood.Screen.Vty.Select
    ( parseSelector
    , Selector
    , selectWithSelector
    , selectWithFormula
    , isSelected
    ) where

import Text.ParserCombinators.Parsec
import qualified Data.Set as Set

import Data.Monoid

type Selector = Set.Set Int

safeIndex l i = take 1 $ drop i l

isSelected :: Int -> Set.Set Int -> Bool
isSelected = Set.member

selectWithSelector :: Selector -> [a] -> [a]
selectWithSelector s l = concat $ map (safeIndex l) $ Set.toList s

selectWithFormula :: String -> [a] -> [a]
selectWithFormula s l = case parseSelector s of
    Nothing -> []
    Just v  -> selectWithSelector v l

parseSelector :: String -> Maybe Selector
parseSelector s = case parse p_selector "" s of
    Left err -> Nothing
    Right v  -> Just v

p_selector = do
    s <- p_range `sepBy1` char ','
    eof
    return $ Set.map (\x -> x - 1) $ mconcat s
p_range = choice [ try p_multi, p_single ]
p_single = fmap (Set.singleton . read) $ many1 digit
p_multi = do
    from <- fmap read $ many1 digit
    char '-'
    to <- fmap read $ many1 digit
    if from > to
        then return $ Set.fromList [to .. from]
        else return $ Set.fromList [from .. to]
