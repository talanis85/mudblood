{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.DLisp.Core
    ( Exp (..)
    , Context
    , Result
    , mkContext
    , mergeContext
    , parse, run, eval
    , throwError
    , typeError
    , getSymbol
    , dummyParser
    , nil
    , liftL
    , typeList
    , typeFunction
    ) where

import qualified Data.Map as M

import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P

import Data.Maybe
import Data.Monoid
import Control.Monad.State
import Control.Monad.Error

-- EXPRESSIONS

data Exp m v = Symbol String
             | Function FunctionSignature (Result m v)
             | Special FunctionSignature (Result m v)
             | List [Exp m v]
             | Value v

type ErrorStateT m v = ErrorT String (StateT (Context m v) m)
type Result m v = ErrorStateT m v (Exp m v)
type FunctionSignature = [String]
type Context m v = M.Map String (Exp m v)

mergeContext = M.union

instance (Show v) => Show (Exp m v) where
    show (Value v) = show v
    show (Symbol s) = s
    show (Function _ _) = "<function>"
    show (Special _ _) = "<special>"
    show (List []) = "nil"
    show (List x) = "(" ++ unwords (map show x) ++ ")"

-- EVALUATION

eval :: (Monad m, Show v) => Exp m v -> Result m v
eval (Value v)          = return $ Value v
eval (Function args f)  = return $ Function args f
eval (Special args f)   = return $ Special args f
eval (Symbol s)         = get >>= \ctx -> case M.lookup s ctx of
    Nothing -> throwError ("Symbol " ++ s ++ " is unbound.")
    Just v  -> return v
eval (List [])          = return $ List []
eval (List (x:xs))      = eval x >>= apply
    where
        apply (Special exArgs f)    = apply' exArgs xs f
        apply (Function exArgs f)   = mapM eval xs >>= \args -> apply' exArgs args f
        apply x                     = throwError $ "Function expected. Got '" ++ (show x) ++ "' instead"

        apply' exArgs args f = do
            oldctx <- get
            applyArgsToContext exArgs args
            result <- f
            put oldctx
            return result

        applyArgsToContext ("...":_) args           = updateSymbol "..." (List args)
        applyArgsToContext (earg:exArgs) (arg:args) = do updateSymbol earg arg
                                                         applyArgsToContext exArgs args
        applyArgsToContext (earg:exArgs) []         = throwError $ "Too few arguments. Expected '" ++ earg ++ "'"
        applyArgsToContext [] _                     = return ()

        updateSymbol s eval_e = modify $ \ctx -> M.insert s eval_e ctx

-- PARSING

parseValue valp = do
    v <- valp
    return $ Value v

parseSymbol = do
    f <- firstAllowed
    r <- many (firstAllowed <|> digit)
    return $ Symbol (f:r)
  where
    firstAllowed = oneOf "+-*/=><!.$" <|> letter

parseExprAux valp = (try $ parseNil) <|> (try $ parseValue valp) <|> (try $ parseSymbol) <|> (try $ parseList valp)

parseNil = string "nil" >> return (List [])

parseList valp = do
    char '('
    skipMany space
    x <- (parseExprAux valp) `sepEndBy` (many1 space)
    (char ')' >> return ()) <|> eof
    return $ List x

parseExpr valp = do
    skipMany space
    x <- parseExprAux valp
    skipMany space ; eof
    return x

parse valp source =
    case P.parse (parseExpr valp) "" source of
        Right x -> Right x
        Left e  -> Left $ show e

dummyParser :: Parser ()
dummyParser = fail ""

-- RUNNING

mkContext :: (Monad m) => [(String, Exp m v)] -> Context m v
mkContext = M.fromList

run :: (Monad m, Show v) => Context m v -> Exp m v -> m (Either String (Exp m v))
run ctx exp = evalStateT (runErrorT (eval exp)) (M.union ctx (mkContext builtins)) >>= return

-- BUILTINS

builtins :: (Monad m, Show v) => [(String, Exp m v)]
builtins =
    [ ("list", Function ["..."] dlispList)
    , ("fn", Special ["args", "..."] dlispFn)
    , ("begin", Special ["..."] dlispBegin)
    , ("for", Function ["list", "fun"] dlispFor)
    ]

-- Helper
getSymbol sym = eval $ (Symbol sym)

typeSymbol x = case x of
    Symbol v -> return v
    _ -> throwError "Type error: Expected symbol"

typeList x = case x of
    List x -> return x
    _ -> throwError "Type error: Expected list"

typeFunction x = case x of
    Function sig r -> return (sig, r)
    _ -> throwError "Type error: Expected function"

typeSpecial x = case x of
    Special sig r -> return (sig, r)
    _ -> throwError "Type error: Expected special"

typeValue x = case x of
    Value v -> return v
    _ -> throwError "Type error: Expected value"

typeError t = throwError $ "Type error: Expected " ++ t

dlispList :: (Monad m, Show v) => Result m v
dlispList = do
    l <- getSymbol "..." >>= typeList
    return $ List l

dlispFn :: (Monad m, Show v) => Result m v
dlispFn = do
    args <- getSymbol "args" >>= typeList
    body <- getSymbol "..." >>= typeList
    let newFn = do evalBody <- mapM eval body
                   return $ last evalBody
    args' <- mapM typeSymbol args
    return $ Function args' newFn

dlispBegin :: (Monad m, Show v) => Result m v
dlispBegin = do
    steps <- getSymbol "..." >>= typeList
    res <- mapM eval steps
    case res of
        [] -> return $ List []
        l  -> return $ last l

dlispFor :: (Monad m, Show v) => Result m v
dlispFor = do
    list <- getSymbol "list" >>= typeList
    fun <- getSymbol "fun"
    ret <- forM list $ \x -> eval (List [fun, x])
    return $ List ret

{-
dlispLet :: (Monad m, Show v) => Result m v
dlispLet = do
    bindings <- getSymbol "bindings" >>= typeList
    fun <- getSymbol "fun"
    case tryfun of
        List [] -> return nil
        x -> eval (List [fun, x])
        -}

-- Convenience functions

nil = List []

liftL :: (Monad m) => m a -> ErrorStateT m v a
liftL = lift . lift
