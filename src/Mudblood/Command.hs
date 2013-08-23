module Mudblood.Command
    ( ArgType (IntType, StringType)
    , Arg (IntArg, StringArg)
    , CommandMonad
    , Command (Command, cmdArgNames, cmdAction)
    , MonadTrans (lift)
    , getIntParam, getStringParam, getIdParam
    , runCommand
    , parseCommand
    ) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Trans.Either

data ArgType = IntType | StringType | IdType

data Arg = IntArg Int
         | StringArg String
         | IdArg String
    deriving (Show)

data Command m = Command {
    cmdArgNames :: [String],
    cmdAction :: CommandMonad m ()
}

data CommandMonad m a = CommandMonad (EitherT String (ReaderT [Arg] m) a)

instance MonadTrans CommandMonad where
    lift x = CommandMonad (lift $ lift x)

instance (Monad m) => Monad (CommandMonad m) where
    return x = CommandMonad $ right x
    fail x = CommandMonad $ left x
    (CommandMonad m) >>= f = CommandMonad $ m >>= (unwrap f)
      where
        unwrap f a = let (CommandMonad x) = f a
                     in x

runCommand :: (Monad m) => CommandMonad m a -> [Arg] -> m (Either String ())
runCommand (CommandMonad cmd) args = do
    r <- runReaderT (runEitherT cmd) args
    case r of
        Left s -> return $ Left s
        Right _ -> return $ Right ()

getIntParam :: (Monad m) => Int -> CommandMonad m Int
getIntParam index = do
    r <- (CommandMonad $ lift ask)
    if length r <= index then fail ("Too few arguments")
                         else case (r !! index) of
                            IntArg x -> return x
                            _        -> fail ("Argument #" ++ (show index) ++ " must be integer")

getStringParam :: (Monad m) => Int -> CommandMonad m String
getStringParam index = do
    r <- (CommandMonad $ lift ask)
    if length r <= index then fail ("Too few arguments")
                         else case (r !! index) of
                            StringArg x -> return x
                            _           -> fail ("Argument #" ++ (show index) ++ " must be string")

getIdParam :: (Monad m) => Int -> CommandMonad m String
getIdParam index = do
    r <- (CommandMonad $ lift ask)
    if length r <= index then fail ("Too few arguments")
                         else case (r !! index) of
                            IdArg x -> return x
                            _       -> fail ("Argument #" ++ (show index) ++ " must be identifier")

parseCommand :: String -> Either String (String, [Arg])
parseCommand s = case parse p_command "" s of
    Left e -> Left (show e)
    Right r -> Right r

-- The command parser

lexer = P.makeTokenParser haskellDef

p_command = do
    command <- p_commandName
    args <- option [] $ skipMany1 space >> p_args
    eof
    return (command, args)

p_commandName = many1 letter

--p_args = p_arg `sepBy1` space
p_args = many1 p_arg

p_arg = p_id <|> p_integer <|> p_string

p_integer =
    do
    d <- (P.integer lexer)
    return $ IntArg (fromIntegral d)

p_string =
    do
    s <- (P.stringLiteral lexer)
    return $ StringArg s

p_id =
    do
    s <- (P.identifier lexer)
    return $ IdArg s
