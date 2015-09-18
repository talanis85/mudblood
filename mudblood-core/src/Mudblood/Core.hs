{-# LANGUAGE FlexibleContexts, FunctionalDependencies #-}

module Mudblood.Core
    ( Game (..)
    , handleLine, handleSend, handlePrompt, handleTelnet, handleInfo
    , decode
    , telnetToGMCP

    , gmcpHello
    ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Error

import Mudblood.Class
import Mudblood.Text
import Mudblood.Telnet (TelnetNeg (..), TelnetCommand (..), Communication (..), TelnetOption (..))
import Mudblood.Error
import Mudblood.Utils

import Data.Maybe
import Data.Char
import Data.Word
import Data.List
import Data.List.Split
import Data.String.Utils
import Data.GMCP

import qualified Codec.Binary.UTF8.String as UTF8

--------------------------------------------------------------------------------------------------

-- | The Game class. Every monad that runs a game must be an instance of 'Game'.
--   Here, we define custom behavior like triggers or key handlers.
class (MB s m) => Game s m | m -> s where
    -- | This is called on every trigger-enabled event. See 'TriggerEvent' for possible events.
    triggerLine :: AttrString -> m ()
    triggerSend :: String -> m ()
    triggerPrompt :: String -> m ()
    triggerTime :: Int -> m ()
    triggerTelnet :: TelnetNeg -> m ()

--------------------------------------------------------------------------------------------------

handleLine :: (Game s m) => AttrString -> m ()
handleLine = echo

handleSend :: (Game s m) => String -> m ()
handleSend = send

handlePrompt :: (Game s m) => String -> m ()
handlePrompt = setPrompt

handleTelnet :: (Game s m) => TelnetNeg -> m ()
handleTelnet t = case t of
  TelnetNeg (Just CMD_DO) (Just OPT_TIMING_MARK) _ ->
      send $ TelnetNeg (Just CMD_WILL) (Just OPT_TIMING_MARK) []
  _ -> return ()

handleInfo :: (Game s m) => String -> m ()
handleInfo = echoInfo

decode :: [Word8] -> [Word8] -> Attr -> ([AttrString], [Word8], Attr)
decode oldprompt input oldattr =
    let (ls, newprompt)   = splitLinesWithPrompt 10 oldprompt input
        (attrls, newattr) = foldl decodeFold ([], oldattr) ls
    in (attrls, newprompt, newattr)
  where decodeFold (l, a) cur =
          let (next, a') = case decodeAS cur a of
                Nothing      -> ((toAS $ "Error decoding ANSI: " ++ escapeAll cur), a)
                Just (s, a') -> (s, a')
          in (l ++ [next], a')

telnetToGMCP :: TelnetNeg -> Maybe GMCP
telnetToGMCP t = case t of
  TelnetNeg (Just CMD_SB) (Just OPT_GMCP) dat -> parseGMCP $ UTF8.decode dat
  _ -> Nothing

--------------------------------------------------------------------------------------------------

-- | Send a standard GMCP hello.
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
