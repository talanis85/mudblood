module Mudblood.Text
    (
      -- * Attributed strings
      Attr (attrStyle, attrFg, attrBg), AttrString
    , Style (StyleNormal, StyleBold, StyleUnderline)
    , Color (DefaultColor, Black, White, Cyan, Magenta, Blue, Yellow, Green, Red)
    , defaultAttr
    -- * Conversion to and from strings
    , decode, toAttrString, fromAttrString
    -- * Misc transformations
    , groupAttrString
    , wrap, untab
    -- * Setting attributes
    , setFg, setBg, setStyle
    ) where

import Data.Char
import Data.Monoid
import Control.Monad

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)

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

instance Show AttrString where
    show (AttrString s) = map fst s

-- | Decompose an AttrString into (string, attribute) pairs.
groupAttrString :: AttrString -> [(String, Attr)]
groupAttrString s = foldr foldFun [] (getAttrString s)
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
toAttrString :: String -> AttrString
toAttrString xs = AttrString $ map (\x -> (x, defaultAttr)) xs

-- | Convert an AttrString to a String
fromAttrString :: AttrString -> String
fromAttrString (AttrString xs) = map fst xs

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
wrap :: Int             -- ^ Line width
     -> AttrString      -- ^ Input
     -> [AttrString]    -- ^ Output
wrap maxLen (AttrString line)
  | length line <= maxLen           = [AttrString line]
  | any (isSpace . fst) beforeMax   = (AttrString beforeSpace) : (wrap maxLen $ AttrString (afterSpace ++ afterMax))
  | otherwise                       = (AttrString beforeMax) : (wrap maxLen $ AttrString afterMax)
    where (beforeMax, afterMax) = splitAt maxLen line
          (beforeSpace, afterSpace) = reverseBreak (isSpace . fst) beforeMax

-- | Convert tabs to spaces
untab :: Int            -- ^ Tab width
      -> AttrString     -- ^ Input
      -> AttrString     -- ^ Output
untab width str = AttrString $ untab' width 0 $ getAttrString str
    where untab' width n [] = []
          untab' width n (('\t', a):cs) = let addspaces = width - (n `mod` width)
                                          in (take addspaces $ repeat (' ', a)) ++ (untab' width (n + addspaces) cs)
          untab' width n ((c, a):cs) = (c, a) : (untab' width (n+1) cs)

reverseBreak :: (a -> Bool) -> [a] -> ([a], [a])
reverseBreak f xs = (reverse before, reverse after)
  where (after, before) = break f $ reverse xs

-- | Convert a string with ANSI sequences to an AttrString. Errors during ANSI parsing
--   will be embedded in the resulting string.
decode :: String             -- ^ The input string - may contain ANSI
       -> Attr               -- ^ Initial attribute settings
       -> (AttrString, Attr) -- ^ The resulting AttrString and the final attribute settings
decode s a = case runParser ansiParser a "" s of
    Right as -> as
    Left err -> (toAttrString ("[ERROR:"++(show err)++"]"), a)

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

