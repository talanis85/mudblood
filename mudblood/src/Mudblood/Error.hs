{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mudblood.Error
    ( StackTrace
    , stackTrace

    , module Control.Monad.Except
    ) where

import Data.Monoid
import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except

import Data.Error

newtype StackTrace = StackTrace [(String, String)]
    deriving (Semigroup, Monoid)

instance Error StackTrace where
    noMsg = StackTrace [("", "")]
    strMsg str = StackTrace [("", str)]

instance Show StackTrace where
    show (StackTrace l) =
        let indent i [] = []
            indent i ((s,e):xs) = (i ++ "[" ++ s ++ "] " ++ e) : (indent (i ++ " ") xs)
        in concat $ intersperse "\n" $ indent "" l

stackTrace :: String -> String -> StackTrace
stackTrace subsys msg = StackTrace [(subsys, msg)]
