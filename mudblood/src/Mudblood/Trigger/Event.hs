{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Mudblood.Trigger.Event where

import Data.Carte
import Data.GMCP
import Mudblood.Text
import Mudblood.Telnet (TelnetNeg)

type Ev = Fix

mkEv :: (a :<: b) => a (Ev b) -> Ev b
mkEv = Fix . inj

data NetworkEventType = NetworkConnect | NetworkDisconnect

newtype LineEvent e     = LineEvent       { unLineEvent     :: AttrString } deriving Functor
newtype SendEvent e     = SendEvent       { unSendEvent     :: String     } deriving Functor
newtype PromptEvent e   = PromptEvent     { unPromptEvent   :: String     } deriving Functor
newtype InfoEvent e     = InfoEvent       { unInfoEvent     :: String     } deriving Functor
newtype TelnetEvent e   = TelnetEvent     { unTelnetEvent   :: TelnetNeg  } deriving Functor
newtype GMCPEvent e     = GMCPEvent       { unGMCPEvent     :: GMCP       } deriving Functor
newtype NetworkEvent e  = NetworkEvent    { unNetworkEvent  :: NetworkEventType } deriving Functor
newtype TimeEvent e     = TimeEvent       { unTimeEvent     :: Int        } deriving Functor
newtype CommandEvent e  = CommandEvent    { unCommandEvent  :: (String, [String])  } deriving Functor
newtype FeedbackEvent e = FeedbackEvent   { unFeedbackEvent :: e          } deriving Functor
data BellEvent e     = BellEvent                                         deriving Functor
data NilEvent e      = NilEvent                                          deriving Functor

class ( LineEvent :<: f, SendEvent :<: f, PromptEvent :<: f, InfoEvent :<: f, TelnetEvent :<: f
      , GMCPEvent :<: f, NetworkEvent :<: f, TimeEvent :<: f, FeedbackEvent :<: f, BellEvent :<: f
      , CommandEvent :<: f, NilEvent :<: f )
      => MBEvent f

type MBEventType = LineEvent :+: SendEvent :+: PromptEvent :+: InfoEvent :+: TelnetEvent
               :+: GMCPEvent :+: NetworkEvent :+: TimeEvent :+: FeedbackEvent :+: BellEvent
               :+: CommandEvent :+: NilEvent

instance MBEvent MBEventType
