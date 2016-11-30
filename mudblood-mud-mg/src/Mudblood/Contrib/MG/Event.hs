{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
module Mudblood.Contrib.MG.Event
    ( guardSignal, guardBlocker
    , fetchSignal, fetchBlocker
    , yieldBlocker
    , feedbackBlocker
    , fetchMessageFrom

    , fetchSignalName
    , signal

    , yieldNotify
    , yieldWarning

    , module Mudblood.Contrib.MG.Class
    ) where

import Mudblood

import Data.Monoid
import Data.Maybe
import Control.Trigger
import Text.Printf
import Mudblood.Contrib.MG.Class
import Mudblood.Contrib.MG.Communication
import Mudblood.Contrib.MG.GMCP
import Control.Monad
import Control.Monad.State

import qualified Data.Map.Strict as Map

yieldBlocker name = yield $ mkEv $ BlockerEvent name
feedbackBlocker name = yieldFeedback $ mkEv $ BlockerEvent name

fetchMessageFrom name = do
  (a,b,c,d) <- fetchMessage
  case (c,d) of
    (True, False) -> if a == name then return b else mzero
    _ -> mzero

fetchSignalName sig = fetchSignal >>= guard . (== sig)

fetchManualSignal = fetchSend >>= isSignal
  where isSignal x = case x of
            ('/':signal) -> return signal
            _ -> mzero

signal sig = yieldFeedback $ mkEv $ SignalEvent sig

yieldNotify x  = do
    yieldInfo "NOTE"
    yieldLine $ setFg Yellow $ toAS $ "--- " ++ x ++ " ---"
yieldWarning x = do
    yieldInfo "WARN"
    yieldLine $ setFg Red $ toAS $ "--- " ++ x ++ " ---"
