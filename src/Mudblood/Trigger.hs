{-# LANGUAGE ExistentialQuantification #-}

module Mudblood.Trigger
    ( module Control.Trigger
    -- * The trigger event type
    , TriggerEvent (..)
    -- * Trigger functions
    -- ** Guards
    , guardLine, guardSend, guardTime, guardTelneg, guardGMCP
    , guardBlock, joinBlock
    -- ** Yielding
    , yieldLine, yieldSend, yieldTime
    -- ** Returning
    , returnLine, returnSend, returnTime
    -- ** Trigger combinators
    , gag, keep, keep1, pass
    -- * Common triggers
    , on
    -- ** Coloring
    , colorize
    ) where

import Control.Trigger
import Control.Monad

import Data.Dynamic
import Data.Monoid

import Mudblood.Telnet
import Mudblood.UI
import Mudblood.Text
import Mudblood.Mapper.Map
import Data.GMCP

data TriggerEvent = LineTEvent AttrString   -- ^ Emitted when a line was received from the host
                  | SendTEvent String       -- ^ Emitted when the user wants to send a line of input
                  | TelnetTEvent TelnetNeg  -- ^ Emitted when a telnet negotiation is received
                  | GMCPTEvent GMCP         -- ^ Emitted when a GMCP telneg is received
                  | TimeTEvent Int          -- ^ Emitted every second. Argument is current POSIX timestamp.
                  | BellTEvent              -- ^ Emitted on bell character.
                  | NilTEvent               -- ^ Dummy event type
                  | CustomTEvent String     -- ^ User defined events
    deriving (Eq, Show)

guardLine :: (MonadPlus m) => TriggerEvent -> m AttrString
guardLine ev = case ev of
    LineTEvent s -> return s
    _            -> mzero

guardSend :: (MonadPlus m) => TriggerEvent -> m String
guardSend ev = case ev of
    SendTEvent s -> return s
    _            -> mzero

guardTime :: (MonadPlus m) => TriggerEvent -> m Int
guardTime ev = case ev of
    TimeTEvent s -> return s
    _            -> mzero

guardTelneg :: (MonadPlus m) => TriggerEvent -> m TelnetNeg
guardTelneg ev = case ev of
    TelnetTEvent s -> return s
    _              -> mzero

guardGMCP :: (MonadPlus m) => TriggerEvent -> m GMCP
guardGMCP ev = case ev of
    GMCPTEvent gmcp -> return gmcp
    _               -> mzero

guardBlock :: (Monad m) => TriggerEvent -> TriggerM m [TriggerEvent] TriggerEvent [AttrString]
guardBlock ev = readBlock [] ev
    where
        readBlock acc ev = case ev of
            TelnetTEvent (TelnetNeg (Just CMD_EOR) Nothing []) -> return acc
            LineTEvent s -> yieldT [ev] >>= readBlock (acc ++ [s])
            _ -> failT

joinBlock :: [AttrString] -> AttrString
joinBlock [] = mempty
joinBlock [a] = a
joinBlock (x:xs) = foldr joinBlock' x xs
    where
        joinBlock' x a = a <> (toAS " ") <> x

-- | Yield a line event
yieldLine :: (Monad m) => AttrString -> TriggerM m [TriggerEvent] r r
yieldLine x = yieldT [LineTEvent x]

-- | Yield a send event
yieldSend :: (Monad m) => String -> TriggerM m [TriggerEvent] r r
yieldSend x = yieldT [SendTEvent x]

-- | Yield a timer event
yieldTime :: (Monad m) => Int -> TriggerM m [TriggerEvent] r r
yieldTime x = yieldT [TimeTEvent x]

-- | Return a line event
returnLine :: (Monad m) => AttrString -> m [TriggerEvent]
returnLine x = return [LineTEvent x]
-- | Return a send event
returnSend :: (Monad m) => String -> m [TriggerEvent]
returnSend x = return [SendTEvent x]
-- | Return a timer event
returnTime :: (Monad m) => Int -> m [TriggerEvent]
returnTime x = return [TimeTEvent x]

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

-- | Colorize an AttrString
colorize :: (Monad m) => Color -> AttrString -> TriggerM m i y [TriggerEvent]
colorize c x = returnLine $ setFg c x

on :: (Monad m) => (a -> m b) -> m c -> a -> m [a]
on trig action ev = do
    trig ev
    action
    return [ev]
