{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Mudblood.Contrib.MG.Class where

import Mudblood

data ChannelEvent a = ChannelEvent { unChannelEvent :: (String, String, String) } deriving (Functor)
data MessageEvent a = MessageEvent { unMessageEvent :: (String, String, Bool, Bool) } deriving (Functor)
data CombatEvent a = CombatEvent
  { combatEventOriginal :: AttrString
  , combatEventAttack :: Bool
  , combatEventEnemy :: String
  , combatEventWeapon :: String
  , combatEventMin :: Int
  , combatEventMax :: Int
  } deriving (Functor)
data FitnessEvent a = FitnessEvent { unFitnessEvent :: (AttrString, Int) } deriving (Functor)
data SystemEvent a = SystemEvent { unSystemEvent :: (String, String) } deriving (Functor)
data SignalEvent a = SignalEvent { unSignalEvent :: String } deriving (Functor)
data BlockerEvent a = BlockerEvent { unBlockerEvent :: String } deriving (Functor)

type MGEventType = ChannelEvent :+: MessageEvent :+: CombatEvent :+: FitnessEvent
               :+: SystemEvent :+: SignalEvent :+: BlockerEvent :+: MBEventType

class ( MBEvent a, ChannelEvent :<: a, MessageEvent :<: a, CombatEvent :<: a, FitnessEvent :<: a
      , SystemEvent :<: a, SignalEvent :<: a, BlockerEvent :<: a ) => MGEvent a

instance MBEvent MGEventType
instance MGEvent MGEventType

guardMessage = guardFix unMessageEvent
fetchMessage = fetch >>= guardMessage

guardChannel = guardFix unChannelEvent
fetchChannel = fetch >>= guardChannel

guardSignal = guardFix unSignalEvent
fetchSignal = fetch >>= guardSignal

guardBlocker = guardFix unBlockerEvent
fetchBlocker = fetch >>= guardBlocker
