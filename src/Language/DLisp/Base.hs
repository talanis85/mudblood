{-# LANGUAGE FlexibleInstances #-}

module Language.DLisp.Base
    ( Value (..)
    , mkBoolValue, mkIntValue, mkFloatValue, mkStringValue
    , typeBool, typeInt, typeFloat, typeString
    -- Some re-exports
    , Exp (..), throwError, getSymbol, nil, liftL
    , typeList
    , run
    ) where

import Data.Monoid
import Data.Maybe
import qualified Data.Map as M

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (haskellDef)
import qualified Text.ParserCombinators.Parsec.Token as T
import Language.DLisp.Core hiding (run, dummyParser)
import qualified Language.DLisp.Core as Core

data Value v = BoolValue Bool
             | IntValue Int
             | FloatValue Double
             | StringValue String
             | UserValue v
    deriving (Show)

instance ValueClass ()

mkBoolValue = Value . BoolValue
mkIntValue = Value . IntValue
mkFloatValue = Value . FloatValue
mkStringValue = Value . StringValue

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

run :: (Monad m, ValueClass v) => Parser v -> Context m (Value v) -> String -> m (Either String (Exp m (Value v)))
run valp ctx src = Core.run (mkValp valp) (M.union ctx (mkContext builtins)) src

-- Builtins

builtins :: (Monad m, ValueClass v) => [(String, Exp m (Value v))]
builtins =
    [ ("if", dlispIf)
    , ("=", dlispStdBinary valueEq)
    , ("+", dlispStdBinary valuePlus)
    , ("-", dlispStdBinary valueMinus)
    , ("*", dlispStdBinary valueMult)
    , ("/", dlispStdBinary valueDiv)
    , ("ord", dlispStdBinary valueOrd)
    , ("not", dlispStdUnary valueNeg)
    ]

dlispIf :: (Monad m) => Exp m (Value v)
dlispIf = Function ["condition", "iftrue", "iffalse"] $ do
            arg1 <- getSymbol "condition" >>= typeBool
            arg2 <- getSymbol "iftrue"
            arg3 <- getSymbol "iffalse"
            if arg1 then eval arg2 else eval arg3

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

instance (ValueClass v) => ValueClass (Exp m (Value v)) where
    valueEq (Value a) (Value b) = fmap Value $ valueEq a b
    valueEq (List a) (List b) =
        let zipper a b = case (valueEq a b) of
                            Just (Value (BoolValue True)) -> True
                            _ -> False
        in if (length a == length b) && (and (zipWith zipper a b))
            then Just (Value $ BoolValue True)
            else Just (Value $ BoolValue False)

instance (ValueClass v) => ValueClass (Value v) where
    valueEq (UserValue a) (UserValue b) = fmap UserValue $ valueEq a b
    valueEq (IntValue a) (IntValue b) = Just $ BoolValue $ a == b

dlispStdBinary :: (Monad m, ValueClass v) => (Exp m v -> Exp m v -> Maybe (Exp m v)) -> Exp m v
dlispStdBinary f = Function ["a", "b"] $ do
            a <- getSymbol "a"
            b <- getSymbol "b"
            case f a b of
                Nothing -> throwError "Undefined binary relation"
                Just v -> return v

dlispStdUnary :: (Monad m, ValueClass v) => (Exp m v -> Maybe (Exp m v)) -> Exp m v
dlispStdUnary f = Function ["a"] $ do
            a <- getSymbol "a"
            case f a of
                Nothing -> throwError "Undefined unary relation"
                Just v -> return v

-- Value parser

mkValp valp =
    (try (parseBool >>= return . BoolValue))
    <|>
    (try (parseInteger >>= return . IntValue))
    <|>
    (try (parseString >>= return . StringValue))
    <|>
    (try (valp >>= return . UserValue))

parseBool = try (string "true" >> return True)
            <|>
            try (string "false" >> return False)

parseInteger = many1 digit >>= return . read

parseString = between (char '"') (char '"') (many $ noneOf "\"")
