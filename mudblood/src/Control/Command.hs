{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
module Control.Command
  ( Command
  , CommandParser
  , mkCommand
  , CommandInfo (..)
  , Arg
  , arg
  , execCommandParser
  , popArgumentFromState
  , intParser
  , stringParser
  , enumParser
  , tokenize
  ) where

import Control.Applicative hiding (many)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Morph
import Data.List

import Text.Parsec
import qualified Text.Parsec.Language as L
import qualified Text.Parsec.Token as T

type Command m r = CommandInfo (m r)

data Arg a = Arg
  { argParser :: ArgParser a
  , argInfo :: ArgInfo
  }
  deriving (Functor)

data ArgParser a = ArgParser
  { argpParser :: String -> Maybe a
  , argpType :: String
  }
  deriving (Functor)

data ArgInfo = ArgInfo
  { argName :: String
  , argDescription :: String
  }

arg :: ArgParser a -> String -> String -> CommandParser a
arg p n d = ArgP Arg
  { argParser = p
  , argInfo = ArgInfo
    { argName = n
    , argDescription = d
    }
  }

data CommandInfo a = CommandInfo
  { cmdParser :: CommandParser a
  , cmdDescription :: String
  , cmdName :: String
  }
  deriving (Functor)

mkCommand :: String -> String -> CommandParser a -> CommandInfo a
mkCommand name desc p = CommandInfo
  { cmdName = name
  , cmdParser = p
  , cmdDescription = desc
  }

data CommandParser a
  = NilP a
  | ArgP (Arg a)
  | forall x . MultP (CommandParser (x -> a)) (CommandParser x)

instance Functor CommandParser where
  fmap f (NilP x) = NilP (f x)
  fmap f (ArgP arg) = ArgP (fmap f arg)
  fmap f (MultP p1 p2) = MultP (fmap (f.) p1) p2

instance Applicative CommandParser where
  pure = NilP
  (<*>) = MultP

popArgumentFromState :: (MonadState [a] m) => m (Maybe a)
popArgumentFromState = do
  args <- get
  case args of
    [] -> return Nothing
    (x:xs) -> put xs >> return (Just x)

execCommandParser :: (MonadError e m, Error e)
                  => (ArgInfo -> m (Maybe String)) -> CommandParser a -> m a
execCommandParser getter p = case p of
  NilP x -> return x
  ArgP arg -> do
    value <- getter (argInfo arg)
    case value of
      Nothing -> throwError $ strMsg $ "Missing arguments. Expecting " ++ argName (argInfo arg) ++ " :: " ++ argpType (argParser arg)
      Just x -> case argpParser (argParser arg) x of
        Just x' -> return x'
        Nothing -> throwError $ strMsg $ "Parse error. Expecting " ++ argName (argInfo arg) ++ " :: " ++ argpType (argParser arg)
  MultP f p' -> do
    f' <- execCommandParser getter f
    x <- execCommandParser getter p'
    return (f' x)

parseOrNothing p s = case parse p "" s of
                       Left err -> Nothing
                       Right v -> Just v

tok = T.makeTokenParser L.haskellDef
integer = T.integer tok

intParser = ArgParser
  { argpParser = parseOrNothing (fromIntegral <$> integer)
  , argpType = "int"
  }
stringParser = ArgParser
  { argpParser = Just
  , argpType = "string"
  }
enumParser enum = ArgParser
  { argpParser = \x -> if x `elem` enum then Just x else Nothing
  , argpType = intercalate "|" enum
  }

tokenize :: String -> Maybe [String]
tokenize s = case parse (many tokenP <* eof) "" s of
               Left err -> error (show err) >> Nothing
               Right v -> Just v

tokenP = choice [try quotedP, try unquotedP]
quotedP = spaces >> char '"' *> many (noneOf "\"") <* char '"'
unquotedP = spaces >> many1 (noneOf " ")
