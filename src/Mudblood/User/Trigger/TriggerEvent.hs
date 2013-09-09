-- | Functions to handle trigger events.
module Mudblood.User.Trigger.TriggerEvent
    ( guard, guardLineEvent, guardSendEvent
    -- * Yielding
    , yieldLine, yieldSend
    -- * Waiting
    , waitForLine, waitForSend
    -- * Returning
    , returnLine, returnSend
    ) where

import Mudblood.Trigger
import Mudblood.Core
import Mudblood.Text

-- | Fail if the condition is False.
guard :: (Functor f) => Bool -> Trigger f i y ()
guard b = if b then return () else failT

guardLineEvent :: (Functor f) => TriggerEvent -> Trigger f i y [AttrString]
guardLineEvent ev = case ev of
    LineTEvent s -> return [s]
    _            -> failT

guardSendEvent :: (Functor f) => TriggerEvent -> Trigger f i y [String]
guardSendEvent ev = case ev of
    SendTEvent s -> return [s]
    _            -> failT

-- | Yield a line event
yieldLine :: (Functor f) => AttrString -> Trigger f i [TriggerEvent] i
yieldLine x = yield [LineTEvent x]

-- | Yield a send event
yieldSend :: (Functor f) => String -> Trigger f i [TriggerEvent] i
yieldSend x = yield [SendTEvent x]

-- | Wait for a line event
waitForLine :: (Functor f) => TriggerEvent -> Trigger f i y AttrString
waitForLine ev = case ev of
    LineTEvent s -> return s
    _            -> failT

-- | Wait for a send event
waitForSend :: (Functor f) => TriggerEvent -> Trigger f i y String
waitForSend ev = case ev of
    SendTEvent s -> return s
    _            -> failT

-- | Return a line event
returnLine x = return [LineTEvent x]
-- | Return a send event
returnSend x = return [SendTEvent x]
