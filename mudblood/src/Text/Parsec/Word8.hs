{-# LANGUAGE FlexibleContexts #-}
module Text.Parsec.Word8
    ( char, oneOf, noneOf, digit
    ) where

import Text.Parsec hiding (satisfy, char, oneOf, noneOf, digit)
import Text.Parsec.Prim
import Text.Parsec.Pos

import Data.Char
import Data.Word

satisfy :: (Monad m) => (Word8 -> Bool) -> ParsecT [Word8] u m Word8
satisfy f = tokenPrim (\c -> show [c])
                      (\pos c cs -> incSourceColumn pos 1)
                      (\c -> if f c then Just c else Nothing)

char :: (Monad m) => Char -> ParsecT [Word8] u m Word8
char c = satisfy (== fromIntegral (ord c))

oneOf :: (Monad m) => [Char] -> ParsecT [Word8] u m Word8
oneOf cs = satisfy (\c -> elem c $ map (fromIntegral . ord) cs)

noneOf :: (Monad m) => [Char] -> ParsecT [Word8] u m Word8
noneOf cs = satisfy (\c -> not $ elem c $ map (fromIntegral . ord) cs)

digit :: (Monad m) => ParsecT [Word8] u m Word8
digit = satisfy (\c -> c >= (fromIntegral (ord '0')) && c <= (fromIntegral (ord '9')))
