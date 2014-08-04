{-# LANGUAGE FlexibleInstances, UndecidableInstances, FlexibleContexts #-}

module Mudblood.Trigger
    ( module Control.Trigger
    -- * The trigger event type
    , TriggerEvent (..)
    -- * Trigger functions
    -- ** Guards
    , guardLine, guardSend, guardTime, guardTelnet, guardGMCP
    , guardBlock, guardBlockGag, joinBlock
    -- ** Wraps
    , wrapLine, wrapSend
    -- ** Yielding
    , yieldLine, yieldSend, yieldTime
    -- ** Common triggers
    , colorize
    ) where

import Control.Trigger
import Control.Monad

import Data.Dynamic
import Data.Monoid

import Mudblood.Class
import Mudblood.Telnet
import Mudblood.Text
import Data.GMCP

-----------------------------------------------------------------------------

data TriggerEvent = LineEvent AttrString   -- ^ Emitted when a line was received from the host
                  | SendEvent String       -- ^ Emitted when the user wants to send a line of input
                  | TelnetEvent TelnetNeg  -- ^ Emitted when a telnet negotiation is received
                  | GMCPEvent GMCP         -- ^ Emitted when a GMCP telneg is received
                  | TimeEvent Int          -- ^ Emitted every second. Argument is current POSIX timestamp.
                  | BellEvent              -- ^ Emitted on bell character.
                  | NilEvent               -- ^ Dummy event type
                  | CustomEvent String     -- ^ User defined events
    deriving (Eq, Show)

-----------------------------------------------------------------------------

guardLine :: (MonadPlus m) => TriggerEvent -> m AttrString
guardLine ev = case ev of
    LineEvent s -> return s
    _           -> mzero

guardSend :: (MonadPlus m) => TriggerEvent -> m String
guardSend ev = case ev of
    SendEvent s -> return s
    _           -> mzero

guardTime :: (MonadPlus m) => TriggerEvent -> m Int
guardTime ev = case ev of
    TimeEvent s -> return s
    _           -> mzero

guardTelnet :: (MonadPlus m) => TriggerEvent -> m TelnetNeg
guardTelnet ev = case ev of
    TelnetEvent s -> return s
    _             -> mzero

guardGMCP :: (MonadPlus m) => TriggerEvent -> m GMCP
guardGMCP ev = case ev of
    GMCPEvent gmcp -> return gmcp
    _              -> mzero

guardBlock :: (Monad m) => TriggerEvent -> Fallible (EndoTrigger TriggerEvent m) [AttrString]
guardBlock ev = readBlock [] ev
    where
        readBlock acc ev = case ev of
            TelnetEvent (TelnetNeg (Just CMD_EOR) Nothing []) -> return acc
            LineEvent s -> yield ev >> (await >>= readBlock (acc ++ [s]))
            _ -> flop

guardBlockGag :: (Monad m) => TriggerEvent -> Fallible (EndoTrigger TriggerEvent m) [AttrString]
guardBlockGag ev = readBlock [] ev
    where
        readBlock acc ev = case ev of
            TelnetEvent (TelnetNeg (Just CMD_EOR) Nothing []) -> return acc
            LineEvent s -> await >>= readBlock (acc ++ [s])
            _ -> flop

-----------------------------------------------------------------------------

joinBlock :: [AttrString] -> AttrString
joinBlock [] = mempty
joinBlock [a] = a
joinBlock (x:xs) = foldr joinBlock' x xs
    where
        joinBlock' x a = a <> (toAS " ") <> x

-----------------------------------------------------------------------------

wrapLine :: (Monad m) => FailingEndoTrigger AttrString m () -> FailingEndoTrigger TriggerEvent m ()
wrapLine = wrap (LineEvent, guardLine)

wrapSend :: (Monad m) => FailingEndoTrigger String m () -> FailingEndoTrigger TriggerEvent m ()
wrapSend = wrap (SendEvent, guardSend)

-----------------------------------------------------------------------------

-- | Yield a line event
yieldLine :: (MonadTrigger a TriggerEvent m) => AttrString -> m ()
yieldLine x = yield $ LineEvent x

-- | Yield a send event
yieldSend :: (MonadTrigger a TriggerEvent m) => String -> m ()
yieldSend x = yield $ SendEvent x

-- | Yield a timer event
yieldTime :: (MonadTrigger a TriggerEvent m) => Int -> m ()
yieldTime x = yield $ TimeEvent x

-----------------------------------------------------------------------------

colorize :: (Monad m) => Color -> EndoTrigger AttrString m ()
colorize c = trig $ setFg c
