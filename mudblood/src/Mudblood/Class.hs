module Mudblood.Class
  ( MonadMB (..)
  , echo, echoInfo, echoError, echoLog
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Trigger

import Data.Menu

import Mudblood.Error
import Mudblood.Monad
import Mudblood.Keys
import Mudblood.Text
import Mudblood.Telnet
import Mudblood.Trigger.Event
import Mudblood.Screen

--------------------------------------------------------------------------------------------------

class MonadMB mb where
    output :: (Screen m) => Output -> mb m ()
    send :: (Screen m, Sendable a) => a -> mb m ()
    setPrompt :: (Screen m) => String -> mb m ()
    time :: (Screen m) => mb m Ticks
    setStatus :: (Screen m) => String -> mb m ()

instance MonadMB (MB u) where
    output x = lift $ outputS x
    send x = lift $ sendS x
    setPrompt x = lift $ setPromptS x
    time = lift timeS
    setStatus x = lift $ setStatusS x

instance MonadMB (MBX e u) where
    output x = lift $ outputS x
    send x = lift $ sendS x
    setPrompt x = lift $ setPromptS x
    time = lift timeS
    setStatus x = lift $ setStatusS x

{-
  output :: Output -> MB u m ()
  send :: (Sendable a) => a -> MB u m ()
  setPrompt :: String -> MB u m ()
  connect :: String -> String -> MB u m ()
  time :: MB u m Ticks
  setStatus :: String -> MB u m ()
-}
  {-
  prompt :: String -> (String -> MBX u m ()) -> MBX u m ()
  bind :: [Key] -> MBX u m () -> MBX u m ()
  menu :: Menu Key (MBX u m ()) -> MBX u m ()
  -}

echo = output . OutputLine
echoInfo = output . OutputInfo
echoError = output . OutputError
echoLog = output . OutputLog

