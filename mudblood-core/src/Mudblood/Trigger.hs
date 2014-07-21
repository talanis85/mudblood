{-# LANGUAGE ExistentialQuantification #-}

module Mudblood.Trigger
    ( module Control.Trigger
    -- * The trigger event type
    , TriggerEvent (..)
    -- * Trigger functions
    -- ** Guards
    , guardLine, guardSend, guardTime, guardTelneg, guardGMCP
    , guardBlock, guardBlockGag, joinBlock
    -- ** Yielding
    , yieldLine, yieldSend, yieldTime
    -- ** Returning
    , returnLine, returnSend, returnTime
    -- ** Trigger combinators
    , gag, keep, keep1, pass
    -- * Common triggers
    , on
    -- ** Coloring
    ) where

import Control.Trigger
import Control.Monad

import Data.Dynamic
import Data.Monoid

import Mudblood.Telnet
import Mudblood.Text
import Data.GMCP

data TriggerEvent = LineEvent AttrString   -- ^ Emitted when a line was received from the host
                  | SendEvent String       -- ^ Emitted when the user wants to send a line of input
                  | TelnetEvent TelnetNeg  -- ^ Emitted when a telnet negotiation is received
                  | GMCPEvent GMCP         -- ^ Emitted when a GMCP telneg is received
                  | TimeEvent Int          -- ^ Emitted every second. Argument is current POSIX timestamp.
                  | BellEvent              -- ^ Emitted on bell character.
                  | NilEvent               -- ^ Dummy event type
                  | CustomEvent String     -- ^ User defined events
    deriving (Eq, Show)

guardLine :: (MonadPlus m) => TriggerEvent -> m AttrString
guardLine ev = case ev of
    LineEvent s -> return s
    _            -> mzero

guardSend :: (MonadPlus m) => TriggerEvent -> m String
guardSend ev = case ev of
    SendEvent s -> return s
    _            -> mzero

guardTime :: (MonadPlus m) => TriggerEvent -> m Int
guardTime ev = case ev of
    TimeEvent s -> return s
    _            -> mzero

guardTelneg :: (MonadPlus m) => TriggerEvent -> m TelnetNeg
guardTelneg ev = case ev of
    TelnetEvent s -> return s
    _              -> mzero

guardGMCP :: (MonadPlus m) => TriggerEvent -> m GMCP
guardGMCP ev = case ev of
    GMCPEvent gmcp -> return gmcp
    _               -> mzero

guardBlock :: (Monad m) => TriggerEvent -> TriggerM TriggerEvent [TriggerEvent] m [AttrString]
guardBlock ev = readBlock [] ev
    where
        readBlock acc ev = case ev of
            TelnetEvent (TelnetNeg (Just CMD_EOR) Nothing []) -> return acc
            LineEvent s -> yield [ev] >>= readBlock (acc ++ [s])
            _ -> flop

guardBlockGag :: (Monad m) => TriggerEvent -> TriggerM TriggerEvent [TriggerEvent] m [AttrString]
guardBlockGag ev = readBlock [] ev
    where
        readBlock acc ev = case ev of
            TelnetEvent (TelnetNeg (Just CMD_EOR) Nothing []) -> return acc
            LineEvent s -> yield [] >>= readBlock (acc ++ [s])
            _ -> flop

joinBlock :: [AttrString] -> AttrString
joinBlock [] = mempty
joinBlock [a] = a
joinBlock (x:xs) = foldr joinBlock' x xs
    where
        joinBlock' x a = a <> (toAS " ") <> x

-- | Yield a line event
yieldLine :: (Monad m) => AttrString -> TriggerM i [TriggerEvent] m i
yieldLine x = yield [LineEvent x]

-- | Yield a send event
yieldSend :: (Monad m) => String -> TriggerM i [TriggerEvent] m i
yieldSend x = yield [SendEvent x]

-- | Yield a timer event
yieldTime :: (Monad m) => Int -> TriggerM i [TriggerEvent] m i
yieldTime x = yield [TimeEvent x]

-- | Return a line event
returnLine :: (Monad m) => AttrString -> m [TriggerEvent]
returnLine x = return [LineEvent x]
-- | Return a send event
returnSend :: (Monad m) => String -> m [TriggerEvent]
returnSend x = return [SendEvent x]
-- | Return a timer event
returnTime :: (Monad m) => Int -> m [TriggerEvent]
returnTime x = return [TimeEvent x]

-- | Discard the result of a trigger
gag :: (Monad m) => (a -> m b) -> (a -> m [c])
gag a ev = a ev >> return []

-- | Discard the result of a trigger and return its input as a list
keep :: (Monad m) => (a -> m b) -> (a -> m [a])
keep a ev = a ev >> return [ev]

-- | Discard the result of a trigger and return its input
keep1 :: (Monad m) => (a -> m b) -> (a -> m a)
keep1 a ev = a ev >> return ev

pass :: (Monad m) => m () -> a -> m a
pass m x = m >> return x

{-
-- | If the first trigger succeeds, subsequent events will run the second trigger
--   until one of these fails.
loopT :: (Monad m)
      => (a -> Trigger a [a] m [a])
      -> (a -> Trigger a [a] m [a])
      -> (a -> Trigger a [a] m [a])
loopT startt nextt = startt >=> yield >=> loop
    where
        loop x = ((nextt >=> yield >=> loop) x) `mplus` (return [x])
-}

on :: (Monad m) => (a -> m b) -> m c -> a -> m [a]
on trig action ev = do
    trig ev
    action
    return [ev]
