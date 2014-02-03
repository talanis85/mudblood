{-# LANGUAGE ExistentialQuantification #-}

module Mudblood.Trigger
    ( module Control.Trigger
    -- * The trigger event type
    , TriggerEvent (..)
    -- * Trigger functions
    -- ** Guards
    , guardT, guardLine, guardSend, guardTime, guardTelneg, guardGMCP
    , guardBlock, joinBlock
    -- ** Yielding
    , yieldLine, yieldSend, yieldTime
    -- ** Returning
    , returnLine, returnSend, returnTime
    -- ** Kleisli arrow
    , (>=>)
    -- ** Trigger combinators
    , gag, keep, keep1
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

-- | Fail if the condition is False.
guardT :: (Monad m) => Bool -> TriggerM m y r ()
guardT True  = return ()
guardT False = failT

guardLine :: (Monad m) => TriggerEvent -> TriggerM m y r AttrString
guardLine ev = case ev of
    LineTEvent s -> return s
    _            -> failT

guardSend :: (Monad m) => TriggerEvent -> TriggerM m y r String
guardSend ev = case ev of
    SendTEvent s -> return s
    _            -> failT

guardTime :: (Monad m) => TriggerEvent -> TriggerM m y r Int
guardTime ev = case ev of
    TimeTEvent s -> return s
    _            -> failT

guardTelneg :: (Monad m) => TriggerEvent -> TriggerM m y r TelnetNeg
guardTelneg ev = case ev of
    TelnetTEvent s -> return s
    _              -> failT

guardGMCP :: (Monad m) => TriggerEvent -> TriggerM m y r GMCP
guardGMCP ev = case ev of
    GMCPTEvent gmcp -> return gmcp
    _ -> failT

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
        joinBlock' x a = a <> (toAttrString " ") <> x

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
returnLine x = return [LineTEvent x]
-- | Return a send event
returnSend x = return [SendTEvent x]
-- | Return a timer event
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

-- | Colorize an AttrString
colorize :: (Monad m) => Color -> AttrString -> TriggerM m i y [TriggerEvent]
colorize c x = returnLine $ setFg c x

on :: (Monad m) => (a -> TriggerM m y r b) -> TriggerM m y r c -> a -> TriggerM m y r [a]
on trig action ev = do
    trig ev
    action
    return [ev]
