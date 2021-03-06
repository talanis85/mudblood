{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Mudblood.Text
    (
      -- * Attributed strings
      Attr (attrStyle, attrFg, attrBg), AttrString
    , Style (StyleNormal, StyleBold, StyleUnderline)
    , Color (DefaultColor, Black, White, Cyan, Magenta, Blue, Yellow, Green, Red)
    , defaultAttr
    -- * Conversion to and from strings
    , decodeAS, toAS, fromAS
    -- * Misc transformations
    , (<++>)
    , groupAS
    , wrapAS, untabAS
    , linesAS
    -- * Setting attributes
    , setFg, setBg, setStyle
    -- * Colors
    , colorToName, intToColor, nameToColor
    ) where

import Data.Char
import Data.Monoid
import Data.Either
import Control.Monad
import Data.List.Split

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)

import Debug.Trace

data Style = StyleNormal | StyleBold | StyleUnderline
    deriving (Show, Eq)

data Color = DefaultColor
           | Black
           | White
           | Cyan
           | Magenta
           | Blue
           | Yellow
           | Green
           | Red
    deriving (Show, Eq)

intToColor :: Int -> Color
intToColor x = case x of
    0 -> Black
    1 -> White
    2 -> Cyan
    3 -> Magenta
    4 -> Blue
    5 -> Yellow
    6 -> Green
    7 -> Red
    _ -> DefaultColor

colorToName :: Color -> String
colorToName c = case c of
    Black -> "black"
    White -> "white"
    Cyan -> "cyan"
    Magenta -> "magenta"
    Blue -> "blue"
    Yellow -> "yellow"
    Green -> "green"
    Red -> "red"
    DefaultColor -> "default"

nameToColor :: String -> Maybe Color
nameToColor s = case s of
    "black" -> Just Black
    "white" -> Just White
    "cyan" -> Just Cyan
    "magenta" -> Just Magenta
    "blue" -> Just Blue
    "yellow" -> Just Yellow
    "green" -> Just Green
    "red" -> Just Red
    "default" -> Just DefaultColor
    _ -> Nothing

data Attr = Attr {
    attrStyle :: Style,
    attrFg :: Color,
    attrBg :: Color
} deriving (Show, Eq)

withFg :: Color -> Attr -> Attr
withFg c a = a { attrFg = c }

withBg :: Color -> Attr -> Attr
withBg c a = a { attrBg = c }

withStyle :: Style -> Attr -> Attr
withStyle s a = a { attrStyle = s }

newtype AttrString = AttrString { getAttrString :: [(Char, Attr)] }
    deriving (Eq)

instance Monoid AttrString where
    mempty = AttrString []
    mappend (AttrString a) (AttrString b) = AttrString (a ++ b)

mapAS :: ([(Char, Attr)] -> [(Char, Attr)]) -> AttrString -> AttrString
mapAS f (AttrString s) = AttrString $ f s

(<++>) :: AttrString -> AttrString -> AttrString
(<++>) = mappend

instance Show AttrString where
    show = fromAS

-- | Decompose an AttrString into (string, attribute) pairs.
groupAS :: AttrString -> [(String, Attr)]
groupAS s = foldr foldFun [] (getAttrString s)
    where
        foldFun (x,a) [] = [([x],a)]
        foldFun (x,a) (([],_):ys) = ([x],a):ys
        foldFun (x,a) ((y,b):ys)
            | a == b    = (x:y,a):ys
            | otherwise = ([x],a):(y,b):ys

-- | Default attributes
defaultAttr :: Attr
defaultAttr = Attr StyleNormal DefaultColor DefaultColor

-- | Convert a String to an AttrString
toAS :: String -> AttrString
toAS xs = AttrString $ map (\x -> (x, defaultAttr)) xs

-- | Convert an AttrString to a String
fromAS :: AttrString -> String
fromAS (AttrString xs) = map fst xs

-- | Set foreground color
setFg :: Color -> AttrString -> AttrString
setFg c (AttrString s) = AttrString $ map (setFg' c) s
    where setFg' c (x, a) = (x, withFg c a)

-- | Set background color
setBg :: Color -> AttrString -> AttrString
setBg c (AttrString s) = AttrString $ map (setBg' c) s
    where setBg' c (x, a) = (x, withBg c a)

-- | Set style
setStyle :: Style -> AttrString -> AttrString
setStyle c (AttrString s) = AttrString $ map (setStyle' c) s
    where setStyle' c (x, a) = (x, withStyle c a)

-- | Word wrap an AttrString
wrapAS :: Int             -- ^ Line width
       -> AttrString      -- ^ Input
       -> [AttrString]    -- ^ Output
wrapAS maxLen (AttrString line)
  | length line <= maxLen           = [AttrString line]
  | any (isSpace . fst) beforeMax   = (AttrString beforeSpace) : (wrapAS maxLen $ AttrString (afterSpace ++ afterMax))
  | otherwise                       = (AttrString beforeMax) : (wrapAS maxLen $ AttrString afterMax)
    where (beforeMax, afterMax) = splitAt maxLen line
          (beforeSpace, afterSpace) = reverseBreak (isSpace . fst) beforeMax

-- | Convert tabs to spaces
untabAS :: Int            -- ^ Tab width
        -> AttrString     -- ^ Input
        -> AttrString     -- ^ Output
untabAS width = mapAS $ untab' width 0
    where untab' width n [] = []
          untab' width n (('\t', a):cs) = let addspaces = width - (n `mod` width)
                                          in (take addspaces $ repeat (' ', a)) ++ (untab' width (n + addspaces) cs)
          untab' width n ((c, a):cs) = (c, a) : (untab' width (n+1) cs)

linesAS :: AttrString -> [AttrString]
linesAS (AttrString as) = map AttrString $ splitWhen ((== '\n') . fst) as

reverseBreak :: (a -> Bool) -> [a] -> ([a], [a])
reverseBreak f xs = (reverse before, reverse after)
  where (after, before) = break f $ reverse xs

-- | Convert a string with ANSI sequences to an AttrString.
decodeAS :: String                      -- ^ The input string - may contain ANSI
         -> Attr                        -- ^ Initial attribute settings
         -> Maybe (AttrString, Attr)    -- ^ The resulting AttrString and the final attribute settings
                                        --   or Nothing on error.
decodeAS s a = case runParser ansiParser a "" s of
    Right as -> Just as
    Left err -> Nothing

------------------------------------------------------------------------------

-- We use parsec for ANSI parsing.

lexer = P.makeTokenParser haskellDef

ansiParser = do
    r <- many (p_command <|> p_text)
    eof
    s <- getState
    return ((AttrString $ concat $ concat r), s)

p_text = many1 $ do
    c <- noneOf ['\ESC']
    s <- getState
    case c of
        '\r' -> return []
        _    -> return [(c, s)]

p_command = do
    char '\ESC'
    char '['
    args <- (P.decimal lexer) `sepBy1` (char ';')
    char 'm'
    forM_ (map fromIntegral args) interpretArg
    return []
  where interpretArg d
            | d == 0                = setState (Attr StyleNormal DefaultColor DefaultColor)
            | d == 1                = updateState (withStyle StyleBold)
            | d == 4                = updateState (withStyle StyleUnderline)
            | d >= 30 && d <= 37    = updateState (withFg (intToColor (d - 30)))
            | d >= 40 && d <= 47    = updateState (withBg (intToColor (d - 40)))
            | otherwise             = return ()

