{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mudblood.Core
    ( Game (..)
    , defaultTrigger
    , TriggerEvent (..)
    , triggerReceive, triggerSend, triggerTime, triggerGMCP, triggerTelnet

    ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import Control.DynCallback

import Mudblood.Class
import Mudblood.Text
import Mudblood.Telnet
import Mudblood.Error
import Mudblood.Utils

import Data.Maybe
import Data.List
import Data.List.Split
import Data.String.Utils
import Data.GMCP

--------------------------------------------------------------------------------------------------

-- | The Game class. Every monad that runs a game must be an instance of 'Game'.
--   Here, we define custom behavior like triggers or key handlers.
class (MB s m) => Game s m where
    -- | This is called on every trigger-enabled event. See 'TriggerEvent' for possible events.
    trigger :: TriggerEvent -> m ()

--------------------------------------------------------------------------------------------------

data TriggerEvent = LineEvent AttrString   -- ^ Emitted when a line was received from the host
                  | SendEvent String       -- ^ Emitted when the user wants to send a line of input
                  | TelnetEvent TelnetNeg  -- ^ Emitted when a telnet negotiation is received
                  | GMCPEvent GMCP         -- ^ Emitted when a GMCP telneg is received
                  | TimeEvent Int          -- ^ Emitted every second. Argument is current POSIX timestamp.
                  | BellEvent              -- ^ Emitted on bell character.
                  | NilEvent               -- ^ Dummy event type
                  | CustomEvent String     -- ^ User defined events
    deriving (Eq, Show)

--------------------------------------------------------------------------------------------------

-- | Provides suitable defaults how TriggerEvents should be handled.
--   This should be called after all custom processing.
defaultTrigger :: (Game s m) => TriggerEvent -> m ()
defaultTrigger ev = case ev of
    LineEvent line -> echo line
    SendEvent line -> send line
    BellEvent      -> return ()
    TelnetEvent t  -> handleTelnetTEvent t
    _               -> return ()
  where
    handleTelnetTEvent t = case t of
        TelnetNeg (Just CMD_DO) (Just OPT_TIMING_MARK) _ ->
            send $ TelnetNeg (Just CMD_WILL) (Just OPT_TIMING_MARK) []
        _ ->
            echo (setFg Magenta (toAS $ show t))

--------------------------------------------------------------------------------------------------

-- | Trigger a LineEvent. Also provides line splitting and prompt handling.
--   (Might be refactored some day)
triggerReceive :: (Game s m)
               => String               -- ^ Current unprocessed data (prompt)
               -> String               -- ^ New data
               -> Attr                 -- ^ Last known attribute state
               -> m (String, Attr)     -- ^ Remaining unprocessed data and resulting attribute state
triggerReceive oldprompt input oldattr =
    let (ls, newprompt)      = splitLinesWithPrompt oldprompt input
        (eventList, newattr) = foldr decodeFold ([], oldattr) ls
    in mapM_ trigger eventList >> return (newprompt, newattr)
  where
    decodeFold cur (l, a) = let (cur', add) = processChars cur
                                (line, attr) = fromMaybe (toAS ("[ERROR]" ++ cur'), a) (decodeAS cur' a)
                            in ((LineEvent line):(add ++ l), attr)
    processChars s = foldr f ("", []) s
      where
        f c (str, add) =
            case c of
                '\a' -> (str, BellEvent : add)
                x    -> (x:str, add)

-- | Trigger a SendEvent.
triggerSend :: (Game s m) => String -> m ()
triggerSend = trigger . SendEvent

-- | Trigger a TimeEvent.
triggerTime :: (Game s m) => Int -> m ()
triggerTime = trigger . TimeEvent

-- | Trigger a TelnetEvent.
triggerTelnet :: (Game s m) => TelnetNeg -> m ()
triggerTelnet = trigger . TelnetEvent

-- | Trigger a GMCPEvent.
triggerGMCP :: (Game s m) => GMCP -> m ()
triggerGMCP = trigger . GMCPEvent
