{-# LANGUAGE ExistentialQuantification #-}

module Mudblood.Trigger
    ( module Control.Trigger
    -- * The trigger event type
    , TriggerEvent (LineTEvent, SendTEvent, TelnetTEvent, GMCPTEvent, TimeTEvent)
    -- * Trigger functions
    -- ** Guards
    , guardT, guardLine, guardSend, guardTime
    -- ** Yielding
    , yieldLine, yieldSend, yieldTime
    -- ** Returning
    , returnLine, returnSend, returnTime
    -- ** Kleisli arrow
    , (>=>)
    -- * Common triggers
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
    deriving (Eq)

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

-- | Colorize an AttrString
colorize :: (Monad m) => Color -> AttrString -> TriggerM m i y [TriggerEvent]
colorize c x = returnLine $ setFg c x
