{-# LANGUAGE NoMonomorphismRestriction #-}
module Mudblood.Contrib.MG.Communication
    ( fetchIncomingMessage, fetchOutgoingMessage
    , messagesC
    ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict

import Mudblood
import Mudblood.Contrib.MG.GMCP
import Mudblood.Contrib.MG.Class

import Text.Printf

messagesC color =
      triggerC 10 (stateful [] $ permanent $ parseS fetchInlineChannel >>= yield . mkEv . ChannelEvent)
  >>> triggerC 10 (stateful [] $ permanent $ parseS fetchInlineMessage >>= yield . mkEv . MessageEvent)
  >>> triggerC 10 (permanent $ parse fetchGMCPChannel >>= yield . mkEv . ChannelEvent)
  >>> triggerC 10000 (permanent $ parse fetchChannel >>= mapM_ (yieldLine . setFg color) . formatChannel)
  >>> triggerC 10000 (permanent $ parse fetchMessage >>= mapM_ (yieldLine . setFg color) . formatMessage)
  where
    formatChannel (a, b, c) = wrapAS 78 $ toAS $ printf "[%s:%s] %s" a b c
    formatMessage (a, b, True,  False) = wrapAS 78 $ toAS $ printf "<von %s> %s" a b
    formatMessage (a, b, False, False) = wrapAS 78 $ toAS $ printf "<an %s> %s" a b
    formatMessage (a, b, True,  True ) = wrapAS 78 $ toAS $ printf "<von %s an Freunde> %s" a b
    formatMessage (a, b, False, True ) = wrapAS 78 $ toAS $ printf "<an Freunde> %s" b

fetchInlineChannel :: (MonadState [Ev e] m, MonadFail m, LineEvent :<: e)
                   => Parser (Ev e) m (String, String, String)
fetchInlineChannel = do
    (chan, name, text1) <- regex3 "^\\[([^]]+):([^]]+)] *(.+|$)" =<< fetchLine
    textRest <- many $ regex1 "^ (.+)$" =<< fetchLine
    return (chan, name, text1 ++ concat textRest)

fetchInlineMessage :: (MonadState [Ev e] m, MonadFail m, LineEvent :<: e)
                   => Parser (Ev e) m (String, String, Bool, Bool)
fetchInlineMessage = do
    (name, text1, incoming, friend) <- fetchLine >>= \x -> friendIn x <|> friendOut x <|> tmIn x <|> tmOut x
    textRest <- many $ regex1 "^ (.+)$" =<< fetchLine
    return (name, text1 ++ concat textRest, incoming, friend)
  where
    friendIn x = do
        [_, _, n, m] <- "^(Dein Freund|Deine Freundin) (.+) teilt Dir mit: *(.+|$)" ~~= x
        return (n, m, True, True)
    friendOut x = do
        [_, m] <- "^Du teilst Deinen Freunden mit: *(.+|$)" ~~= x
        return ("", m, False, True)
    tmIn x = do
        [_, n, m] <- "^(.+) teilt Dir mit: *(.+|$)" ~~= x
        return (n, m, True, False)
    tmOut x = do
        [_, n, m] <- "^Du teilst (.+) mit: *(.+|$)" ~~= x
        return (n, m, False, False)

fetchIncomingMessage = do
    (name, text, incoming, friend) <- fetchMessage
    guard (incoming && not friend)
    return (name, text)

fetchOutgoingMessage = do
    (name, text, incoming, friend) <- fetchMessage
    guard (not incoming && not friend)
    return (name, text)
