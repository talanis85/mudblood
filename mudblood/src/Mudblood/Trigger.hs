{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts, DeriveFunctor, RankNTypes #-}

module Mudblood.Trigger
    ( module Control.Trigger
    , module Data.Carte
    , module Mudblood.Trigger.Event
    -- * Trigger functions
    , defaultHandler
    -- ** Debugging
    -- , traceT
    -- ** Guards
    , guardFix, guardLine, guardSend, guardPrompt, guardTime, guardTelnet, guardFeedback
    , guardGMCP, guardGMCPModule, guardConnect, guardInfo, guardCommand, guardEOR
    -- ** Fetches
    , fetchLine, fetchSend, fetchPrompt, fetchTime, fetchTelnet, fetchFeedback, fetchGMCP
    , fetchGMCPModule, fetchConnect, fetchEOR, fetchCommand, fetchBlock
    , commandTrigger
    , joinBlock, joinBlockMultiline
    -- ** Yielding
    , yieldLine, yieldLines, yieldSend, yieldPrompt, yieldTelnet, yieldGMCP, yieldTime, yieldFeedback, yieldInfo, yieldError
    -- ** Waiting
    , wait, periodically, periodically'
    ) where

import Control.Trigger
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Command

import Data.Dynamic
import Data.Monoid
import Data.Maybe
import Data.List

import Mudblood.Class
import Mudblood.Monad
import Mudblood.Core
import Mudblood.Telnet (TelnetNeg (..), TelnetCommand (..))
import Mudblood.Trigger.Event
import Mudblood.Text
import Mudblood.Screen
import Data.GMCP
import Data.Carte

import Text.Printf

-----------------------------------------------------------------------------

defaultHandler :: (Screen m, MBEvent e) => Ev e -> MBR s e u m ()
defaultHandler x = void $ runMaybeT $ msum
  [ guardLine x >>= lift . liftMBR . echo
  , guardSend x >>= lift . liftMBR . send
  , guardPrompt x >>= lift . liftMBR . setPrompt
  , guardInfo x >>= lift . liftMBR . echoInfo
  , guardTelnet x >> return ()
  , guardFeedback x >>= lift . triggerWithDefault defaultHandler
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

fetchConnect :: (Monad m, NetworkEvent :<: a) => Parser (Ev a) m ()
fetchConnect = fetch >>= guardConnect

fetchGMCPModule :: (Monad m, GMCPEvent :<: a) => String -> Parser (Ev a) m GMCP
fetchGMCPModule mod = fetchGMCP >>= \x -> if gmcpModule x == mod then return x else mzero

fetchCommand :: (Monad m, CommandEvent :<: a) => Parser (Ev a) m (String, [String])
fetchCommand = fetch >>= guardCommand

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

guardConnect :: (MonadPlus m, NetworkEvent :<: a) => Ev a -> m ()
guardConnect ev = do
    ne <- guardFix unNetworkEvent ev
    case ne of
        NetworkConnect -> return ()
        _              -> mzero

guardGMCPModule :: (MonadPlus m, GMCPEvent :<: a) => String -> Ev a -> m GMCP
guardGMCPModule m ev = guardGMCP ev >>= \x -> guard (gmcpModule x == m) >> return x

guardInfo :: (MonadPlus m, InfoEvent :<: a) => Ev a -> m String
guardInfo = guardFix unInfoEvent

guardCommand :: (MonadPlus m, CommandEvent :<: a) => Ev a -> m (String, [String])
guardCommand = guardFix unCommandEvent

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

commandTrigger :: (MonadError e m, Error e, CommandEvent :<: a) => Command (Iteration (Ev a) m) r -> Iteration (Ev a) m r
commandTrigger cmd = do
    args <- parse' $ do
        (name', args) <- fetchCommand
        guard (name' == cmdName cmd)
        return args
    let result = runStateT (execCommandParser (const popArgumentFromState) (cmdParser cmd)) args
    case result of
      Left err -> lift $ throwError $ strMsg err
      Right (cmd'', _) -> cmd''

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
yieldError x    = yield $ mkEv $ LineEvent $ toAS $ show x

-----------------------------------------------------------------------------

wait :: (Screen s, TimeEvent :<: a) => Int -> Iteration (Ev a) (MB u s) ()
wait sec = do
    t <- lift time
    parse' $ fetchTime >>= guard . (>= t + sec)

periodically :: (Screen s, TimeEvent :<: a) => Int -> Iteration (Ev a) (MB u s) () -> Iteration (Ev a) (MB u s) r
periodically sec act = forever (wait sec >> act)

periodically' :: (Screen s, TimeEvent :<: a) => Int -> Iteration (Ev a) (MB u s) () -> Trigger (Ev a) (MB u s) r
periodically' sec act = permanent (wait sec >> act)
