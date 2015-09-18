{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, BangPatterns, DeriveGeneric #-}

module Mudblood.Text
    (
      -- * Attributed strings
      Attr (attrStyle, attrFg, attrBg), AttrString
    , Style (StyleNormal, StyleBold, StyleUnderline, StyleReverse)
    , Color (DefaultColor, Black, White, Cyan, Magenta, Blue, Yellow, Green, Red, RGB)
    , defaultAttr
    -- * Conversion to and from strings
    , decodeAS, toAS, fromAS
    , escapeAll
    -- * Misc transformations
    , (<>), mapAS
    , groupAS
    , wrapAS, untabAS
    , linesAS
    , appendAligned
    -- * Setting attributes
    , setFg, setBg, setStyle
    -- * Colors
    , colorToName, intToColor, nameToColor
    , parseColor
    ) where

import Data.Char
import Data.Word
import Data.Monoid
import Data.Either
import Data.Maybe
import Data.Bits.Utils
import Text.Read
import Control.Monad
import Data.List.Split

import Text.Parsec hiding (noneOf, char, oneOf, digit)
import Text.Parsec.Word8

import Debug.Trace

import GHC.Exts
import GHC.Generics

import Data.Serialize

instance Serialize Style
instance Serialize Color
instance Serialize Attr
instance Serialize AttrString

data Style = StyleNormal | StyleBold | StyleUnderline | StyleReverse
    deriving (Show, Eq, Generic)

data Color = DefaultColor
           | Black
           | White
           | Cyan
           | Magenta
           | Blue
           | Yellow
           | Green
           | Red
           | RGB Int Int Int
    deriving (Show, Eq, Generic)

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
    RGB r g b -> "rgb<" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ">"
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

parseColor :: String -> Maybe Color
parseColor ('#':hex) = parseHexColor hex
parseColor name = nameToColor name

hexdigit :: (MonadPlus m) => Char -> m Int
hexdigit x
  | ord x >= 48 && ord x <= 57    = return (ord x - 48)
  | ord x >= 65 && ord x <= 70    = return (ord x - 65 + 10)
  | ord x >= 97 && ord x <= 102   = return (ord x - 97 + 10)
  | otherwise                     = mzero

parseHexColor :: String -> Maybe Color
parseHexColor (a:b:c:d:e:f:[]) = do
  a' <- hexdigit a
  b' <- hexdigit b
  c' <- hexdigit c
  d' <- hexdigit d
  e' <- hexdigit e
  f' <- hexdigit f
  return $ RGB (a' * 16 + b') (c' * 16 + d') (e' * 16 + f')
parseHexColor (a:b:c:[]) = do
  a' <- hexdigit a
  b' <- hexdigit b
  c' <- hexdigit c
  return $ RGB (a' * 16 + a') (b' * 16 + b') (c' * 16 + c')
parseHexColor (a:[]) = do
  a' <- hexdigit a
  return $ RGB (a' * 16 + a') (a' * 16 + a') (a' * 16 + a')
parseHexColor _ = Nothing

data Attr = Attr {
    attrStyle :: Style,
    attrFg :: Color,
    attrBg :: Color
} deriving (Show, Eq, Generic)

withFg :: Color -> Attr -> Attr
withFg c a = a { attrFg = c }

withBg :: Color -> Attr -> Attr
withBg c a = a { attrBg = c }

withStyle :: Style -> Attr -> Attr
withStyle s a = a { attrStyle = s }

data AttrString = AttrString ![(String, Attr)]
  deriving (Generic)

instance Eq AttrString where
    (AttrString a) == (AttrString b) = a == b

instance IsString AttrString where
    fromString = toAS

instance Monoid AttrString where
    mempty = AttrString []
    mappend (AttrString []) (AttrString b) = AttrString b
    mappend (AttrString a) (AttrString []) = AttrString a
    mappend (AttrString a) (AttrString b) =
        let a' = last a
            b' = head b
        in if snd a' == snd b' then AttrString (init a ++ [(fst a' ++ fst b', snd a')] ++ tail b)
                               else AttrString (a ++ b)

mapAS :: ([(Char, Attr)] -> [(Char, Attr)]) -> AttrString -> AttrString
mapAS f s = packAS $ f $ unpackAS s

unpackAS :: AttrString -> [(Char, Attr)]
unpackAS (AttrString as) = concat $ map (\(s,a) -> map (\c -> (c,a)) s) as

packAS :: [(Char, Attr)] -> AttrString
packAS s = AttrString $ foldr foldFun [] s
    where
        foldFun (x,a) [] = [([x],a)]
        foldFun (x,a) (([],_):ys) = ([x],a):ys
        foldFun (x,a) ((y,b):ys)
            | a == b    = (x:y,a):ys
            | otherwise = ([x],a):(y,b):ys

instance Show AttrString where
    show = fromAS

-- | Decompose an AttrString into (string, attribute) pairs.
groupAS :: AttrString -> [(String, Attr)]
groupAS (AttrString as) = as

-- | Default attributes
defaultAttr :: Attr
defaultAttr = Attr StyleNormal DefaultColor DefaultColor

-- | Convert a String to an AttrString
toAS :: String -> AttrString
toAS xs = AttrString [(xs, defaultAttr)]

-- | Convert an AttrString to a String
fromAS :: AttrString -> String
fromAS (AttrString xs) = concat $ map fst xs

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
wrapAS n s | n > 0     = map packAS $ wrapAS' n $ unpackAS s
           | otherwise = []

wrapAS' maxLen line
  | length line <= maxLen           = [line]
  | any (isSpace . fst) beforeMax   = beforeSpace : (wrapAS' maxLen $ afterSpace ++ afterMax)
  | otherwise                       = beforeMax : (wrapAS' maxLen afterMax)
    where (beforeMax, afterMax) = splitAt maxLen line
          (beforeSpace, afterSpace) = reverseBreak (isSpace . fst) beforeMax

-- | Convert tabs to spaces
untabAS :: Int            -- ^ Tab width
        -> AttrString     -- ^ Input
        -> AttrString     -- ^ Output
untabAS n s = packAS $ untabAS' n $ unpackAS s

untabAS' width = untab' width 0
    where untab' width n [] = []
          untab' width n (('\t', a):cs) = let addspaces = width - (n `mod` width)
                                          in (take addspaces $ repeat (' ', a)) ++ (untab' width (n + addspaces) cs)
          untab' width n ((c, a):cs) = (c, a) : (untab' width (n+1) cs)

linesAS :: AttrString -> [AttrString]
linesAS as = map packAS $ splitWhen ((== '\n') . fst) $ unpackAS as

reverseBreak :: (a -> Bool) -> [a] -> ([a], [a])
reverseBreak f xs = (reverse before, reverse after)
  where (after, before) = break f $ reverse xs

appendAligned :: AttrString -> Int -> AttrString -> AttrString
appendAligned a1@(AttrString s1) n a2 = a1 <> (toAS $ take fillers $ repeat ' ') <> a2
    where fillers = max 0 $ n - length s1

escapeAll :: [Word8] -> String
escapeAll s = concat $ map (\c -> if c >= 32 && c <= 126 then [chr $ fromIntegral c] else "\\" ++ show c) s

-- | Convert a string with ANSI sequences to an AttrString.
decodeAS :: [Word8]                     -- ^ The input string - may contain ANSI
         -> Attr                        -- ^ Initial attribute settings
         -> Maybe (AttrString, Attr)    -- ^ The resulting AttrString and the final attribute settings
                                        --   or Nothing on error.
decodeAS s a = case runParser ansiParser a "" s of
    Right as -> Just as
    Left err -> Nothing

------------------------------------------------------------------------------

-- We use parsec for ANSI parsing.

-- lexer = P.makeTokenParser haskellDef

ansiParser = do
    r <- many (p_command <|> p_text)
    eof
    s <- getState
    return ((packAS $ concat $ concat r), s)

p_text = many1 $ do
    c <- noneOf ['\ESC']
    s <- getState
    case c of
        13   -> return [] -- CR
        _    -> return [(chr $ fromIntegral c, s)]

p_command = do
    char '\ESC'
    char '['
    args <- many1 digit `sepBy1` char ';'
    char 'm'
    forM_ (catMaybes $ map (readMaybe . map w82c) $ args) interpretArg
    return []
  where interpretArg d
            | d == 0                = setState (Attr StyleNormal DefaultColor DefaultColor)
            | d == 1                = updateState (withStyle StyleBold)
            | d == 4                = updateState (withStyle StyleUnderline)
            | d >= 30 && d <= 37    = updateState (withFg (intToColor (d - 30)))
            | d >= 40 && d <= 47    = updateState (withBg (intToColor (d - 40)))
            | otherwise             = return ()

