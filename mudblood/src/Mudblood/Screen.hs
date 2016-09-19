module Mudblood.Screen
  ( Screen (..)
  , Ticks, Output (..)
  ) where

import Mudblood.Error (StackTrace)
import Mudblood.Text (AttrString)
import Mudblood.Telnet (Sendable)

--------------------------------------------------------------------------------------------------

type Ticks = Int

data Output = OutputLine AttrString
            | OutputError StackTrace
            | OutputLog String
            | OutputInfo String
    deriving (Show)

class (Monad m) => Screen m where
    outputS :: Output -> m ()
    sendS :: (Sendable a) => a -> m ()
    setPromptS :: String -> m ()
    connectS :: String -> String -> m ()
    timeS :: m Ticks
    setStatusS :: String -> m ()
