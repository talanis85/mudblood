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
guard :: Bool -> Trigger i y ()
guard b = if b then return () else failT

guardLineEvent :: TriggerEvent -> Trigger i y [AttrString]
guardLineEvent ev = case ev of
    LineTEvent s -> return [s]
    _            -> failT

guardSendEvent :: TriggerEvent -> Trigger i y [String]
guardSendEvent ev = case ev of
    SendTEvent s -> return [s]
    _            -> failT

-- | Yield a line event
yieldLine :: AttrString -> Trigger i [TriggerEvent] i
yieldLine x = yield [LineTEvent x]

-- | Yield a send event
yieldSend :: String -> Trigger i [TriggerEvent] i
yieldSend x = yield [SendTEvent x]

-- | Wait for a line event
waitForLine :: TriggerEvent -> Trigger i y AttrString
waitForLine ev = case ev of
    LineTEvent s -> return s
    _            -> failT

-- | Wait for a send event
waitForSend :: TriggerEvent -> Trigger i y String
waitForSend ev = case ev of
    SendTEvent s -> return s
    _            -> failT

-- | Return a line event
returnLine x = return [LineTEvent x]
-- | Return a send event
returnSend x = return [SendTEvent x]
