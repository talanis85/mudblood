{-# LANGUAGE FlexibleInstances #-}

module Mudblood.Contrib.Lisp.MB
    ( Value (..)
    , mkBoolValue, mkIntValue, mkFloatValue, mkStringValue
    , mkAttrStringValue
    , typeBool, typeInt, typeFloat, typeString
    , typeAttrString
    , parseValue
    , mbBuiltins
    -- re-exports
    , typeList, typeError, throwError, getSymbol, Exp (..), nil, liftL
    ) where

import Text.ParserCombinators.Parsec
import Mudblood.Contrib.Lisp.Core
import Mudblood.Text
import Data.Monoid
import Control.Monad
import qualified Data.Map as M

data Value = BoolValue Bool
           | IntValue Int
           | FloatValue Double
           | StringValue String
           | AttrStringValue AttrString

instance Show Value where
    show (BoolValue True) = "true"
    show (BoolValue False) = "false"
    show (IntValue x) = show x
    show (FloatValue x) = show x
    show (StringValue x) = x
    show (AttrStringValue v) = show v

mkBoolValue = Value . BoolValue
mkIntValue = Value . IntValue
mkFloatValue = Value . FloatValue
mkStringValue = Value . StringValue
mkAttrStringValue = Value . AttrStringValue

typeBool x = case x of
    Value (BoolValue v) -> return v
    _ -> typeError "bool"

typeInt x = case x of
    Value (IntValue v) -> return v
    _ -> typeError "int"

typeFloat x = case x of
    Value (FloatValue v) -> return v
    _ -> typeError "float"

typeString x = case x of
    Value (StringValue v) -> return v
    _ -> typeError "string"

typeAttrString x = case x of
    Value (AttrStringValue v) -> return v
    _ -> typeError "attrstring"

class ValueClass v where
    valueEq     :: v -> v -> Maybe v
    valueOrd    :: v -> v -> Maybe v
    valuePlus   :: v -> v -> Maybe v
    valueMinus  :: v -> v -> Maybe v
    valueMult   :: v -> v -> Maybe v
    valueDiv    :: v -> v -> Maybe v
    valueNeg    :: v -> Maybe v

    valueEq _ _     = Nothing
    valueOrd _ _    = Nothing
    valuePlus _ _   = Nothing
    valueMinus _ _  = Nothing
    valueMult _ _   = Nothing
    valueDiv _ _    = Nothing
    valueNeg _      = Nothing

instance ValueClass (Exp m Value) where
    valueEq (Value a) (Value b) = fmap Value $ valueEq a b
    valueEq (List a) (List b) =
        let zipper a b = case (valueEq a b) of
                            Just (Value (BoolValue True)) -> True
                            _ -> False
        in if (length a == length b) && (and (zipWith zipper a b))
            then Just (Value $ BoolValue True)
            else Just (Value $ BoolValue False)
    valueEq _ _ = Nothing

    valueOrd (Value a) (Value b) = fmap Value $ valueOrd a b
    valueOrd _ _ = Nothing

    valuePlus (Value a) (Value b) = fmap Value $ valuePlus a b
    valuePlus (List a) (List b) = Just $ List $ a ++ b
    valuePlus _ _ = Nothing

    valueMinus (Value a) (Value b) = fmap Value $ valueMinus a b
    valueMinus _ _ = Nothing

    valueMult (Value a) (Value b) = fmap Value $ valueMult a b
    valueMult _ _ = Nothing

    valueDiv (Value a) (Value b) = fmap Value $ valueDiv a b
    valueDiv _ _ = Nothing

    valueNeg (Value a) = fmap Value $ valueNeg a
    valueNeg _ = Nothing

instance ValueClass Value where
    valueEq (IntValue a) (IntValue b) = Just $ BoolValue $ a == b
    valueEq (StringValue a) (StringValue b) = Just $ BoolValue $ a == b
    valueEq (AttrStringValue a) (AttrStringValue b) = Just $ BoolValue $ a == b
    valueEq (AttrStringValue a) (AttrStringValue b) = Just $ AttrStringValue $ a `mappend` b
    valueEq (AttrStringValue a) (StringValue b) = Just $ BoolValue $ (fromAS a) == b
    valueEq (StringValue a) (AttrStringValue b) = Just $ BoolValue $ a == (fromAS b)
    valueEq _ _ = Nothing

    valueOrd (IntValue a) (IntValue b) = Just $ BoolValue $ a >= b
    valueOrd _ _ = Nothing

    valuePlus (IntValue a) (IntValue b) = Just $ IntValue $ a + b
    valuePlus (StringValue a) (StringValue b) = Just $ StringValue $ a ++ b
    valuePlus _ _ = Nothing

    valueMinus (IntValue a) (IntValue b) = Just $ IntValue $ a - b
    valueMinus _ _ = Nothing

    valueMult (IntValue a) (IntValue b) = Just $ IntValue $ a * b
    valueMult _ _ = Nothing

    valueDiv (IntValue a) (IntValue b) = Just $ IntValue $ a `div` b
    valueDiv _ _ = Nothing

    valueNeg (BoolValue a) = Just $ BoolValue $ not a
    valueNeg _ = Nothing

mbBuiltins :: (Monad m) => Context m Value
mbBuiltins = mkContext $
    [ ("if", dlispIf)
    , ("=", dlispStdBinary valueEq)
    , ("+", dlispStdBinary valuePlus)
    , ("-", dlispStdBinary valueMinus)
    , ("*", dlispStdBinary valueMult)
    , ("/", dlispStdBinary valueDiv)
    , ("ord", dlispStdBinary valueOrd)
    , ("not", dlispStdUnary valueNeg)
    , ("set-fg", dlispAttrStringColor setFg)
    , ("set-bg", dlispAttrStringColor setBg)
    , ("repeat", dlispRepeat)
    ]

dlispIf :: (Monad m) => Exp m Value
dlispIf = Function ["condition", "iftrue", "iffalse"] $ do
            arg1 <- getSymbol "condition" >>= typeBool
            arg2 <- getSymbol "iftrue"
            arg3 <- getSymbol "iffalse"
            if arg1 then eval arg2 else eval arg3

dlispStdBinary :: (Monad m) => (Exp m Value -> Exp m Value -> Maybe (Exp m Value)) -> Exp m Value
dlispStdBinary f = Function ["a", "b"] $ do
            a <- getSymbol "a"
            b <- getSymbol "b"
            case f a b of
                Nothing -> throwError "Undefined binary relation"
                Just v -> return v

dlispStdUnary :: (Monad m) => (Exp m Value -> Maybe (Exp m Value)) -> Exp m Value
dlispStdUnary f = Function ["a"] $ do
            a <- getSymbol "a"
            case f a of
                Nothing -> throwError "Undefined unary relation"
                Just v -> return v

dlispAttrStringColor :: (Monad m) => (Color -> AttrString -> AttrString) -> Exp m Value
dlispAttrStringColor f = Function ["color", "string"] $ do
            arg1 <- getSymbol "color" >>= typeString
            arg2 <- getSymbol "string" >>= typeAttrString
            case nameToColor arg1 of
                Nothing -> throwError "Invalid color"
                Just c  -> return $ mkAttrStringValue $ f c arg2

dlispRepeat :: (Monad m) => Exp m Value
dlispRepeat = Special ["count", "arg"] $ do
    count <- getSymbol "count" >>= typeInt
    action <- getSymbol "arg"
    forM_ [1..count] $ \_ -> eval action
    return nil

parseValue = do
    v <- parseStdValue
    return v

parseStdValue =
    (try (parseBool >>= return . BoolValue))
    <|>
    (try (parseInteger >>= return . IntValue))
    <|>
    (try (parseString >>= return . StringValue))

parseBool = try (string "true" >> return True)
            <|>
            try (string "false" >> return False)

parseInteger = many1 digit >>= return . read

parseString = (try $ between (char '"') (char '"') (many $ noneOf "\""))
              <|>
              (try $ between (char '{') (char '}') (many $ noneOf "}"))
