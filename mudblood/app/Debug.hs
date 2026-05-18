{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Prelude hiding ((.), id)

import           Control.Category
import           Control.Trigger
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Menu
import           Development.GitRev
import           Options.Applicative
import           System.Directory

import           Mudblood
import           Mudblood.Component
import           Mudblood.Screen.Debug

data Options = Options
  { optHost :: String
  , optPort :: Int
  }

options :: ParserInfo Options
options = info (helper <*> (Options <$> hostArgument <*> portArgument))
  (  fullDesc
  <> progDesc "A MUD client (debug version)"
  <> header "mudblood"
  <> footer ("Version: " ++ version)
  )

hostArgument = argument str (metavar "HOST")
portArgument = argument auto (metavar "PORT")

version :: String
version = $(gitBranch) ++ "@" ++ $(gitHash)

main :: IO ()
main = do
  opts <- execParser options
  run $ component (optHost opts) (optPort opts)

component :: String -> Int -> MBComponent DebugScreen MBEventType () ()
component host port = bootC $ connect host (show port)

{-
componentGuest assetdir =
      mgBaseC
  >>> gmcpC
  >>> (assetsC assetdir "Gast")
  >>> Char.component
  >>> (messagesC Blue)
  >>> (Mapper.component portals)
  >>> Combat.component

componentCharacter name pass assetdir =
      mgBaseC
  >>> gmcpC
  >>> (assetsC assetdir name)
  >>> Char.component
  >>> (Char.connectC "mud.morgengrauen.info" "4711" name pass)
  >>> (messagesC Blue)
  >>> (Mapper.component portals)
  >>> Combat.component
  >>> logfileC
-}
