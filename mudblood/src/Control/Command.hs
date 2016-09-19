{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Command
  ( Command, CommandM
  , Arg (..)
  , parseCommand
  , runCommand, getCommandDoc
  , runCommandM
  , mkCommand
  , mapCommand
  , getStringArg, getStringOption
  , getIntArg, getIntOption
  ) where

import Control.Applicative hiding (many)
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Morph

import Text.Parsec
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T

data Command m r = Command
    { commandAction :: CommandM m r
    , commandDoc :: String
    }

newtype CommandM m r = CommandM { getCommandM :: ReaderT [Arg] m r }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadError e, MFunctor)

data Arg = IntArg Int | StringArg String | IdentifierArg String

getIntArg = argAt coerceInt
getIntOption = optionAt coerceInt

coerceInt (IntArg x) = return x
coerceInt _ = throwError $ strMsg "Expected integer"

getStringArg = argAt coerceString
getStringOption = optionAt coerceString

coerceString (StringArg x) = return x
coerceString (IdentifierArg x) = return x
coerceString (IntArg x) = return $ show x

argAt :: (Error e, MonadError e m) => (Arg -> CommandM m a) -> Int -> CommandM m a
argAt f n = do
    r <- optionAt f n
    case r of
        Nothing -> lift $ throwError $ strMsg "Not enough arguments"
        Just r  -> return r

optionAt :: (Monad m) => (Arg -> CommandM m a) -> Int -> CommandM m (Maybe a)
optionAt f n = do
    args <- CommandM $ ask
    if length args > n
        then liftM Just (f (args !! n))
        else return Nothing

parseCommand :: (Error e, MonadError e m) => String -> m (String, [Arg])
parseCommand s = case parse p_cmd "" s of
                   Left err -> throwError $ strMsg $ show err
                   Right v -> return v

runCommandM :: CommandM m r -> [Arg] -> m r
runCommandM cmd args = runReaderT (getCommandM cmd) args

runCommand :: Command m r -> [Arg] -> m r
runCommand cmd args = runCommandM (commandAction cmd) args

mapCommand :: (CommandM m r -> CommandM m' r') -> Command m r -> Command m' r'
mapCommand f cmd = cmd { commandAction = f (commandAction cmd) }

getCommandDoc :: Command m r -> String
getCommandDoc = commandDoc

mkCommand :: String -> CommandM m r -> Command m r
mkCommand doc cmd = Command
    { commandAction = cmd
    , commandDoc = doc
    }

tok = T.makeTokenParser L.haskellDef

identifier = T.identifier tok
stringLiteral = T.stringLiteral tok
integer = T.integer tok

p_cmd = do
    cmd <- identifier
    args <- many p_arg
    return (cmd, args)

p_arg = choice [ p_stringArg, p_intArg, p_identifierArg ]

p_stringArg = StringArg <$> stringLiteral
p_intArg = IntArg . fromIntegral <$> integer
p_identifierArg = IdentifierArg <$> identifier
