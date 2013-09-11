{-# LANGUAGE ExistentialQuantification #-}

module Mudblood.Trigger
    ( module Control.Trigger
    , MBTriggerF (..)
    -- * The trigger event type
    , TriggerEvent (LineTEvent, SendTEvent, TelnetTEvent, GMCPTEvent, TimeTEvent)
    -- * Convenient type aliases
    , MBTrigger, MBTriggerFlow
    -- * Trigger functions
    -- ** Guards
    , guardT, guardLine, guardSend, guardTime
    -- ** Yielding
    , yieldLine, yieldSend, yieldTime
    -- ** Waiting
    , waitForLine, waitForSend
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

import Mudblood.Telnet
import Mudblood.UI
import Mudblood.Text
import Mudblood.Mapper.Map
import Data.GMCP

data MBTriggerF i o = forall a. RunIO (IO a) (a -> o)
                    | Echo AttrString o
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
                  | TimeTEvent Int          -- ^ Emitted every second. Argument is current POSIX timestamp.
    deriving (Eq)

-- | Fail if the condition is False.
guardT :: (Functor f) => Bool -> Trigger f i y ()
guardT True  = return ()
guardT False = failT

guardLine :: (Functor f) => TriggerEvent -> Trigger f i y AttrString
guardLine ev = case ev of
    LineTEvent s -> return s
    _            -> failT

guardSend :: (Functor f) => TriggerEvent -> Trigger f i y String
guardSend ev = case ev of
    SendTEvent s -> return s
    _            -> failT

guardTime :: (Functor f) => TriggerEvent -> Trigger f i y Int
guardTime ev = case ev of
    TimeTEvent s -> return s
    _            -> failT

-- | Yield a line event
yieldLine :: (Functor f) => AttrString -> Trigger f i [TriggerEvent] i
yieldLine x = yield [LineTEvent x]

-- | Yield a send event
yieldSend :: (Functor f) => String -> Trigger f i [TriggerEvent] i
yieldSend x = yield [SendTEvent x]

-- | Yield a timer event
yieldTime :: (Functor f) => Int -> Trigger f i [TriggerEvent] i
yieldTime x = yield [TimeTEvent x]

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
-- | Return a timer event
returnTime x = return [TimeTEvent x]

-- | Colorize an AttrString
colorize :: (Functor f) => Color -> AttrString -> Trigger f i y [TriggerEvent]
colorize c x = returnLine $ setFg c x
