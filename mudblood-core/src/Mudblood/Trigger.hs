{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts, DeriveFunctor, RankNTypes #-}

module Mudblood.Trigger
    ( module Control.Trigger
    , module Data.Carte
    -- * The trigger event type
    , Ev, MBEvent, mkEv
    , MBEventType
    , LineEvent (..)
    , SendEvent (..)
    , PromptEvent (..)
    , InfoEvent (..)
    , TelnetEvent (..)
    , GMCPEvent (..)
    , TimeEvent (..)
    , FeedbackEvent (..)
    , BellEvent (..)
    , NilEvent (..)
    , handleEvent
    -- * Trigger functions
    -- ** Debugging
    -- , traceT
    -- ** Guards
    , guardFix, guardLine, guardSend, guardPrompt, guardTime, guardTelnet, guardFeedback, guardGMCP, guardGMCPModule, guardInfo, guardEOR
    -- ** Fetches
    , fetchLine, fetchSend, fetchPrompt, fetchTime, fetchTelnet, fetchFeedback, fetchGMCP, fetchGMCPModule, fetchEOR, fetchBlock
    , joinBlock, joinBlockMultiline
    -- ** Yielding
    , yieldLine, yieldLines, yieldSend, yieldPrompt, yieldTelnet, yieldGMCP, yieldTime, yieldFeedback, yieldInfo
    -- ** Waiting
    , wait
    ) where

import Control.Trigger
import Control.Monad
import Control.Monad.Trans.Maybe

import Data.Dynamic
import Data.Monoid
import Data.Maybe
import Data.List

import Mudblood.Class
import Mudblood.Core
import Mudblood.Telnet (TelnetNeg (..), TelnetCommand (..))
import Mudblood.Text
import Data.GMCP
import Data.Carte

import Text.Printf

-----------------------------------------------------------------------------

type Ev = Fix

mkEv :: (a :<: b) => a (Ev b) -> Ev b
mkEv = Fix . inj

newtype LineEvent e     = LineEvent       { unLineEvent     :: AttrString } deriving Functor
newtype SendEvent e     = SendEvent       { unSendEvent     :: String     } deriving Functor
newtype PromptEvent e   = PromptEvent     { unPromptEvent   :: String     } deriving Functor
newtype InfoEvent e     = InfoEvent       { unInfoEvent     :: String     } deriving Functor
newtype TelnetEvent e   = TelnetEvent     { unTelnetEvent   :: TelnetNeg  } deriving Functor
newtype GMCPEvent e     = GMCPEvent       { unGMCPEvent     :: GMCP       } deriving Functor
newtype TimeEvent e     = TimeEvent       { unTimeEvent     :: Int        } deriving Functor
newtype FeedbackEvent e = FeedbackEvent   { unFeedbackEvent :: e          } deriving Functor
data BellEvent e     = BellEvent                                         deriving Functor
data NilEvent e      = NilEvent                                          deriving Functor

class ( LineEvent :<: f, SendEvent :<: f, PromptEvent :<: f, InfoEvent :<: f, TelnetEvent :<: f
      , GMCPEvent :<: f, TimeEvent :<: f, FeedbackEvent :<: f, BellEvent :<: f
      , NilEvent :<: f )
      => MBEvent f

type MBEventType = LineEvent :+: SendEvent :+: PromptEvent :+: InfoEvent :+: TelnetEvent :+: GMCPEvent :+: TimeEvent :+: FeedbackEvent :+: BellEvent :+: NilEvent

-----------------------------------------------------------------------------

handleEvent :: (Game s m, LineEvent :<: f, SendEvent :<: f, PromptEvent :<: f, InfoEvent :<: f, TelnetEvent :<: f) => Ev f -> m ()
handleEvent x = void $ runMaybeT $ msum
  [ guardLine x >>= lift . handleLine
  , guardSend x >>= lift . handleSend
  , guardPrompt x >>= lift . handlePrompt
  , guardInfo x >>= lift . handleInfo
  , guardTelnet x >>= lift . handleTelnet
  ]

-----------------------------------------------------------------------------

{-
traceT :: (MB scr m, Show a) => String -> Trigger a m () -> Trigger a m ()
traceT name p = traceIn >-> p >-> traceOut
  where traceIn = permanent $ do
            x <- await
            lift $ echo $ toAS $ printf "--- TRACE : %20s <- %20s" name (show x)
            yield x
        traceOut = permanent $ do
            x <- await
            lift $ echo $ toAS $ printf "--- TRACE : %20s -> %20s" name (show x)
            yield x
-}

-----------------------------------------------------------------------------

fetchLine :: (Monad m, LineEvent :<: a) => Parser (Ev a) m AttrString
fetchLine = fetch >>= guardLine

fetchSend :: (Monad m, SendEvent :<: a) => Parser (Ev a) m String
fetchSend = fetch >>= guardSend

fetchPrompt :: (Monad m, PromptEvent :<: a) => Parser (Ev a) m String
fetchPrompt = fetch >>= guardPrompt

fetchTime :: (Monad m, TimeEvent :<: a) => Parser (Ev a) m Int
fetchTime = fetch >>= guardTime

fetchTelnet :: (Monad m, TelnetEvent :<: a) => Parser (Ev a) m TelnetNeg
fetchTelnet = fetch >>= guardTelnet

fetchFeedback :: (Monad m, FeedbackEvent :<: a) => Parser (Ev a) m (Ev a)
fetchFeedback = fetch >>= guardFeedback

fetchGMCP :: (Monad m, GMCPEvent :<: a) => Parser (Ev a) m GMCP
fetchGMCP = fetch >>= guardGMCP

fetchGMCPModule :: (Monad m, GMCPEvent :<: a) => String -> Parser (Ev a) m GMCP
fetchGMCPModule mod = fetchGMCP >>= \x -> if gmcpModule x == mod then return x else mzero

fetchEOR :: (Monad m, TelnetEvent :<: a) => Parser (Ev a) m ()
fetchEOR = fetch >>= guardEOR

guardFix :: (MonadPlus m, a :<: c) => (a (Ev c) -> b) -> Ev c -> m b
guardFix f x = liftM f $ prjM (unFix x)

guardLine :: (MonadPlus m, LineEvent :<: a) => Ev a -> m AttrString
guardLine = guardFix unLineEvent

guardSend :: (MonadPlus m, SendEvent :<: a) => Ev a -> m String
guardSend = guardFix unSendEvent

guardPrompt :: (MonadPlus m, PromptEvent :<: a) => Ev a -> m String
guardPrompt = guardFix unPromptEvent

guardTime :: (MonadPlus m, TimeEvent :<: a) => Ev a -> m Int
guardTime = guardFix unTimeEvent

guardTelnet :: (MonadPlus m, TelnetEvent :<: a) => Ev a -> m TelnetNeg
guardTelnet = guardFix unTelnetEvent

guardFeedback :: (MonadPlus m, FeedbackEvent :<: a) => Ev a -> m (Ev a)
guardFeedback = guardFix unFeedbackEvent

guardGMCP :: (MonadPlus m, GMCPEvent :<: a) => Ev a -> m GMCP
guardGMCP = guardFix unGMCPEvent

guardGMCPModule :: (MonadPlus m, GMCPEvent :<: a) => String -> Ev a -> m GMCP
guardGMCPModule m ev = guardGMCP ev >>= \x -> guard (gmcpModule x == m) >> return x

guardInfo :: (MonadPlus m, InfoEvent :<: a) => Ev a -> m String
guardInfo = guardFix unInfoEvent

guardEOR :: (MonadPlus m, TelnetEvent :<: a) => Ev a -> m ()
guardEOR = guardTelnet >=> \x -> case x of
                                   TelnetNeg (Just CMD_EOR) Nothing [] -> return ()
                                   _ -> mzero

-- | Be careful: This will discard ALL events that are not LineEvents until the
--   next EOR.
fetchBlock :: (Monad m, LineEvent :<: a, TelnetEvent :<: a) => Parser (Ev a) m [AttrString]
fetchBlock = fmap (catMaybes . map guardLine) $ fetch `manyTill` fetchEOR

-----------------------------------------------------------------------------

joinBlock :: [AttrString] -> AttrString
joinBlock [] = mempty
joinBlock [a] = a
joinBlock (x:xs) = foldl joinBlock' x xs
    where
        joinBlock' x a = x <> (toAS " ") <> a

joinBlockMultiline :: [AttrString] -> AttrString
joinBlockMultiline = mconcat . intersperse (toAS "\n")

-----------------------------------------------------------------------------

yieldLine x     = yield $ mkEv $ LineEvent x
yieldLines x    = mapM_ (yield . mkEv . LineEvent) x
yieldSend x     = yield $ mkEv $ SendEvent x
yieldPrompt x   = yield $ mkEv $ PromptEvent x
yieldTime x     = yield $ mkEv $ TimeEvent x
yieldGMCP x     = yield $ mkEv $ GMCPEvent x
yieldTelnet x   = yield $ mkEv $ TelnetEvent x
yieldFeedback x = yield $ mkEv $ FeedbackEvent x
yieldInfo x     = yield $ mkEv $ InfoEvent x

-----------------------------------------------------------------------------

wait :: (TimeEvent :<: a, MB scr m) => Int -> Iteration (Ev a) m ()
wait sec = do
    t <- lift time
    parseU' $ fetchTime >>= guard . (>= t + sec)
