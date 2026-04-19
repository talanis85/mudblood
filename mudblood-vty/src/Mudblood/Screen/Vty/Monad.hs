{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Mudblood.Screen.Vty.Monad where

import Control.Lens
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Except
import Control.Concurrent.STM
import Control.UnsafeCallback

import Mudblood.Error
import Mudblood.Telnet
import Mudblood.Keys
import Mudblood.Text

import qualified Data.ListZipper as LZ
import qualified Data.History as Hist
import qualified Data.Trie as Trie
import Data.Buffer
import Data.Menu
import Data.Word
import Data.Maybe
import Data.Time.LocalTime
import Data.Monoid

import Mudblood.Screen.Vty.Lines
import Mudblood.Screen.Vty.Layout
import Mudblood.Screen.Vty.UserWidget

import qualified Graphics.Vty as V
import Graphics.Vty.Widget

import System.IO
import System.Process

-----------------------------------------------------------------------------

newtype VtyScreen a = VtyScreen (ExceptT StackTrace (StateT VtyScreenState IO) a)
    deriving (Functor, Monad, Applicative, MonadIO, MonadFail, MonadState VtyScreenState, MonadError StackTrace)

evalVtyScreen :: VtyScreen a -> VtyScreenState -> IO (Either StackTrace a)
evalVtyScreen (VtyScreen s) state = evalStateT (runExceptT s) state

-----------------------------------------------------------------------------

data Mode = NormalMode Bool
          | PromptMode Buffer String (UnsafeCallback String)
          | InteractiveMode Buffer InteractiveHandle (UnsafeCallback ())
          | SelectMode Buffer String [(ZonedTime, (String, AttrString))] (UnsafeCallback [String])
          | WidgetMode

showMode m = case m of
               NormalMode False -> "NORMAL"
               NormalMode True  -> "PASTE"
               PromptMode _ _ _ -> "PROMPT"
               InteractiveMode _ _ _ -> "INTERACTIVE"
               SelectMode _ _ _ _ -> "SELECT"
               WidgetMode -> "WIDGET"

data InteractiveHandle = InteractiveHandle Handle Handle Handle ProcessHandle

data Event = SReceiveEvent [Word8]
           | STelnetEvent TelnetNeg
           | SCloseEvent
           | SResizeEvent
           | SKeyEvent Key [KeyMod]
           | SFifoEvent String
           | STimeEvent Int
           | SInteractiveEvent String
           | SEndInteractiveEvent

data VtyScreenState = VtyScreenState
    { _scrPrompt :: [Word8]
    , _scrMarkedPrompt :: String
    , _scrNormalKeybuffer :: [Key]
    , _scrNormalBuffer :: Buffer
    , _scrNormalHistory :: Hist.History String
    , _scrLinebuffer :: LZ.Zipper Line
    , _scrCurrentAttr :: Attr
    , _scrBindings :: Trie.Trie Key (Last (UnsafeCallback ()))
    , _scrMenu :: Maybe (Menu Key (UnsafeCallback ()))
    , _scrSidebar :: Layout
    , _scrStatus :: String
    , _scrEventChan :: TChan Event
    , _scrSocket :: Maybe TelnetSocket
    , _scrMode :: Mode
    , _scrTime :: Int
    , _scrVty :: V.Vty
    , _scrDebugLevel :: Int
    , _scrUpdate :: Bool
    , _scrQuit :: Bool
    }

makeLenses ''VtyScreenState
