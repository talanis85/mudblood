module Mudblood.Error
    ( Error, StackTrace
    , failErr, tryErr, eitherErr, maybeErr
    , failErrT, tryErrT, eitherErrT, maybeErrT
    , runErrorT
    ) where

import Control.Monad.Trans.Either

type ErrorT = EitherT StackTrace
type Error = Either StackTrace

newtype StackTrace = StackTrace { getStackTrace :: [(String, String)] }

instance Show StackTrace where
    show st =
        let show' (n, (subsys, msg)) = show n ++ ". [" ++ subsys ++ "] " ++ msg
        in unlines $ map show' (zip [1..] (getStackTrace st))

failErr :: String -> String -> Error a
failErr subsys msg = Left $ StackTrace [(subsys, msg)]

tryErr :: String -> String -> Error a -> Error a
tryErr subsys msg err = case err of
    Left trace -> Left $ StackTrace $ (subsys, msg) : (getStackTrace trace)
    Right v -> Right v

eitherErr :: String -> Either String a -> Error a
eitherErr subsys e = case e of
    Left err -> Left $ StackTrace [(subsys, err)]
    Right r -> Right r

maybeErr :: String -> Maybe String -> Error ()
maybeErr subsys e = case e of
    Just err -> Left $ StackTrace [(subsys, err)]
    Nothing -> Right ()


runErrorT :: (Monad m) => ErrorT m a -> m (Error a)
runErrorT = runEitherT

failErrT :: (Monad m) => String -> String -> ErrorT m a
failErrT subsys msg = hoistEither $ failErr subsys msg

tryErrT :: (Monad m) => String -> String -> ErrorT m a -> ErrorT m a
tryErrT subsys msg err = EitherT $ runErrorT err >>= return . tryErr subsys msg

eitherErrT :: (Monad m) => String -> Either String a -> ErrorT m a
eitherErrT subsys e = hoistEither $ eitherErr subsys e

maybeErrT :: (Monad m) => String -> Maybe String -> ErrorT m ()
maybeErrT subsys e = hoistEither $ maybeErr subsys e
