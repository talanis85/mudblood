{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Prelude hiding ((.), id)

import           Control.Category
import           Control.Trigger
import           Control.Lens
import           Data.Monoid
import           Data.Menu
import           Development.GitRev
import           Options.Applicative

import           Mudblood
import           Mudblood.Component.Logfile
import           Mudblood.Contrib.MG
-- import qualified Mudblood.Contrib.MG.NPCDB as NPCDB
import qualified Mudblood.Contrib.MG.Mapper as Mapper
import qualified Mudblood.Contrib.MG.Char as Char
import qualified Mudblood.Contrib.MG.Combat as Combat
import qualified Mudblood.Contrib.MG.Guilds.Kaempfer as Kaempfer
import qualified Mudblood.Contrib.MG.Guilds.Karate as Karate
import qualified Mudblood.Contrib.MG.Guilds.Klerus as Klerus
import qualified Mudblood.Contrib.MG.Guilds.Tanjian as Tanjian
import qualified Mudblood.Contrib.MG.Guilds.Werwoelfe as Werwoelfe
import qualified Mudblood.Contrib.MG.Guilds.Zauberer as Zauberer
import           Mudblood.Screen.Vty

data Options = Options
  { optCharacter :: Maybe String
  , optPassword :: Maybe String
  }

options :: ParserInfo Options
options = info (helper <*> (Options <$> characterOption <*> passwordOption))
  (  fullDesc
  <> progDesc "Ein MUD client fürs MorgenGrauen"
  <> header "mudblood - MG edition"
  <> footer ("Version: " ++ version)

characterOption = option (Just <$> str) (short 'u' <> long "user" <> value Nothing)
passwordOption = option (Just <$> str) (short 'p' <> long "password" <> value Nothing)

version :: String
version = $(gitBranch) ++ "@" ++ $(gitHash)

main :: IO ()
main = do
  opts <- execParser options

  let component = case optCharacter opts of
        Nothing -> componentGuest
        Just name -> do
          assetdir <- getXdgDirectory XdgData "mudblood"
          componentCharacter name pass assetdir

  run (Just widget) component

mgBaseC :: MBComponent VtyScreen MGEventType () (Fix Nil)
mgBaseC = nilC

componentGuest =
      mgBaseC
  >>> gmcpC
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
