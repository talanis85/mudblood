module Mudblood.Command
    ( CommandMonad
    , Command (Command, cmdArgNames, cmdAction)
    , MonadTrans (lift)
    , popIntParam, popStringParam, popBoolParam
    , runCommand
    , parseCommand
    ) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellDef)

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Trans.Either

data Command m = Command {
    cmdArgNames :: [String],
    cmdAction :: CommandMonad m ()
}

data CommandMonad m a = CommandMonad (EitherT String (StateT [String] m) a)

instance MonadTrans CommandMonad where
    lift x = CommandMonad (lift $ lift x)

instance (Monad m) => Monad (CommandMonad m) where
    return x = CommandMonad $ right x
    fail x = CommandMonad $ left x
    (CommandMonad m) >>= f = CommandMonad $ m >>= (unwrap f)
      where
        unwrap f a = let (CommandMonad x) = f a
                     in x

parseCommand :: String -> Either String (String, [String])
parseCommand s = case parse pCommand "" s of
    Left e -> Left (show e)
    Right r -> Right r

runCommand :: (Monad m) => CommandMonad m a -> [String] -> m (Either String ())
runCommand (CommandMonad cmd) args = do
    (r, _) <- runStateT (runEitherT cmd) args
    case r of
        Left s -> return $ Left s
        Right _ -> return $ Right ()

popIntParam :: (Monad m) => CommandMonad m Int
popIntParam = do
    r <- CommandMonad $ lift get
    case r of
        []   -> fail "Too few arguments"
        x:xs -> CommandMonad (lift $ put xs) >> parseArg pInteger x

popStringParam :: (Monad m) => CommandMonad m String
popStringParam = do
    r <- CommandMonad $ lift get
    case r of
        []   -> fail "Too few arguments"
        x:xs -> CommandMonad (lift $ put xs) >> parseArg pString x

popBoolParam :: (Monad m) => CommandMonad m Bool
popBoolParam = do
    r <- CommandMonad $ lift get
    case r of
        []   -> fail "Too few arguments"
        x:xs -> CommandMonad (lift $ put xs) >> parseArg pBool x

lexer = P.makeTokenParser haskellDef

parseArg parser str = either (fail . show) return $ parse parser' "" str
    where parser' = do result <- parser
                       eof
                       return result

pInteger = P.integer lexer >>= return . fromIntegral
pString = many anyChar
pBool = ((    (try $ string "yes")
         <|> (try $ string "ja")
         <|> (try $ string "true")
        ) >> return True)
        <|>
        ((    (try $ string "no")
         <|> (try $ string "nein")
         <|> (try $ string "false")
        ) >> return False)

pCommand = do
    name <- many letter
    args <- (many1 space >> pArgList) <|> return []
    eof
    return (name, args)

pArgList = pArg `sepBy` (many1 (oneOf " \t\n"))

pArg = try (between (char '"') (char '"') (many $ noneOf "\""))
       <|>
       many (noneOf " \t\n")
