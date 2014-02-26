{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mudblood.Error
    ( StackTrace
    , stackTrace, stackError, justError
    , maybeError, eitherError, mapLeft
    , ignoreError
    ) where

import Data.Monoid
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Error

newtype StackTrace = StackTrace [(String, String)]
    deriving (Monoid)

instance Error StackTrace where
    noMsg = StackTrace [("", "")]
    strMsg str = StackTrace [("", str)]

instance Show StackTrace where
    show (StackTrace l) = unlines $ map (\(a,b) -> "[" ++ a ++ "] " ++ b) l

stackTrace :: String -> String -> StackTrace
stackTrace subsys msg = StackTrace [(subsys, msg)]

stackError :: (MonadError e m, Error e, Monoid e) => e -> m a -> m a
stackError e m = catchError m $ \e' -> throwError $ e <> e'

justError :: (MonadError e m, Error e, Show a) => Maybe a -> m ()
justError m = case m of
    Just err -> throwError $ strMsg $ show err
    Nothing -> return ()

maybeError :: (MonadError e m, Error e) => e -> Maybe a -> m a
maybeError err m = case m of
    Just x -> return x
    Nothing -> throwError err

eitherError :: (MonadError e m, Error e) => Either e a -> m a
eitherError e = case e of
    Left err -> throwError err
    Right x -> return x

ignoreError :: (MonadError e m, Error e) => m a -> m ()
ignoreError m = catchError (m >> return ()) (const $ return ())

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f e = case e of
    Left x -> Left $ f x
    Right x -> Right x

