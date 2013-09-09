{-# LANGUAGE ExistentialQuantification #-}

module Mudblood.Trigger
    ( module Control.Trigger
    , MBTriggerF (..)
    -- * The trigger event type
    , TriggerEvent (LineTEvent, SendTEvent, TelnetTEvent, GMCPTEvent)
    -- * Convenient type aliases
    , MBTrigger, MBTriggerFlow
    -- * Trigger functions
    , guard, guardLineEvent, guardSendEvent
    -- ** Yielding
    , yieldLine, yieldSend
    -- ** Waiting
    , waitForLine, waitForSend
    -- ** Returning
    , returnLine, returnSend
    ) where

import Control.Trigger

import Data.Dynamic

import Mudblood.Telnet
import Mudblood.UI
import Mudblood.Text
import Mudblood.Mapper.Map
import Data.GMCP

data MBTriggerF i o = forall a. RunIO (IO a) (a -> o)
                    | Echo String o
                    | Send Communication o
                    | GetUserData (Dynamic -> o)
                    | PutUserData Dynamic o
                    | GetMap (Map -> o)
                    | PutMap Map o
                    | PutUI UIAction o

instance Functor (MBTriggerF i) where
    fmap f (RunIO io g) = RunIO io $ f . g
    fmap f (Echo s x) = Echo s $ f x
    fmap f (Send s x) = Send s $ f x
    fmap f (GetUserData g) = GetUserData $ f . g
    fmap f (PutUserData d x) = PutUserData d $ f x
    fmap f (PutUI a x) = PutUI a $ f x
    fmap f (GetMap g) = GetMap $ f . g
    fmap f (PutMap d x) = PutMap d $ f x

type MBTrigger a = Trigger (MBTriggerF TriggerEvent) TriggerEvent [TriggerEvent] a
type MBTriggerFlow = TriggerFlow (MBTriggerF TriggerEvent) TriggerEvent

data TriggerEvent = LineTEvent AttrString   -- ^ Emitted when a line was received from the host
                  | SendTEvent String       -- ^ Emitted when the user wants to send a line of input
                  | TelnetTEvent TelnetNeg  -- ^ Emitted when a telnet negotiation is received
                  | GMCPTEvent GMCP         -- ^ Emitted when a GMCP telneg is received
    deriving (Eq)

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
