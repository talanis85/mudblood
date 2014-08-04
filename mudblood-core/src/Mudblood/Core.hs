{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mudblood.Core
    ( Game (..)
    , defaultTrigger
    , TriggerEvent (..)
    , triggerReceive, triggerSend, triggerTime, triggerGMCP, triggerTelnet

    , gmcpHello
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
import Mudblood.Trigger

import Data.Maybe
import Data.List
import Data.List.Split
import Data.String.Utils
import Data.GMCP

import qualified Codec.Binary.UTF8.String as UTF8

--------------------------------------------------------------------------------------------------

-- | The Game class. Every monad that runs a game must be an instance of 'Game'.
--   Here, we define custom behavior like triggers or key handlers.
class (MB s m) => Game s m where
    -- | This is called on every trigger-enabled event. See 'TriggerEvent' for possible events.
    trigger :: TriggerEvent -> m ()

    -- | Used by the screen to periodically query for a status line.
    queryStatus :: m String

--------------------------------------------------------------------------------------------------

-- | Provides suitable defaults how TriggerEvents should be handled.
--   This should be called after all custom processing.
defaultTrigger :: (Game s m) => TriggerEvent -> m ()
defaultTrigger ev = case ev of
    LineEvent line -> echo line
    SendEvent line -> send line
    BellEvent      -> return ()
    TelnetEvent t  -> handleTelnetTEvent t
    GMCPEvent g    -> echo $ toAS $ show g
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
triggerTelnet neg = case neg of
    TelnetNeg (Just CMD_SB) (Just OPT_GMCP) dat ->
        case parseGMCP $ UTF8.decode dat of
            Nothing -> throwError $ stackTrace "core" "Received invalid GMCP"
            Just gmcp -> triggerGMCP gmcp
    _ -> trigger $ TelnetEvent neg

-- | Trigger a GMCPEvent.
triggerGMCP :: (Game s m) => GMCP -> m ()
triggerGMCP = trigger . GMCPEvent

--------------------------------------------------------------------------------------------------

gmcpHello :: [String]           -- ^ A list of supported GMCP modules
          -> [Communication]
gmcpHello supports =
    [ Communication $ TelnetNeg (Just CMD_DO) (Just OPT_GMCP) []
    , Communication $ GMCP "Core.Hello" $
        JSObject $ toJSObject [ ("client", JSString $ toJSString "mudblood"),
                                ("version", JSString $ toJSString "0.1") -- TODO: Configure this somehow
                              ]
    , Communication $ GMCP "Core.Supports.Set" $ JSArray $ map (JSString . toJSString) supports
    ]
