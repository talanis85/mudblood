{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Prelude hiding ((.), id)

import           Control.Category
import           Control.Trigger
import           Control.Lens
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Menu
import           Development.GitRev
import           Options.Applicative
import           System.Directory

import           Mudblood
import           Mudblood.Component.Logfile
import           Mudblood.Component.Debug
import           Mudblood.Component.Assets
import           Mudblood.Contrib.MG
-- import qualified Mudblood.Contrib.MG.NPCDB as NPCDB
import qualified Mudblood.Contrib.MG.Mapper as Mapper
import qualified Mudblood.Contrib.MG.Char as Char
import qualified Mudblood.Contrib.MG.Combat as Combat
-- import qualified Mudblood.Contrib.MG.Guilds.Kaempfer as Kaempfer
-- import qualified Mudblood.Contrib.MG.Guilds.Karate as Karate
import qualified Mudblood.Contrib.MG.Guilds.Klerus as Klerus
import qualified Mudblood.Contrib.MG.Guilds.Tanjian as Tanjian
import qualified Mudblood.Contrib.MG.Guilds.Werwoelfe as Werwoelfe
import qualified Mudblood.Contrib.MG.Guilds.Zauberer as Zauberer
import           Mudblood.Screen.Simple

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
  )

characterOption = option (Just <$> str) (short 'u' <> long "user" <> value Nothing)
passwordOption = option (Just <$> str) (short 'p' <> long "password" <> value Nothing)

version :: String
version = $(gitBranch) ++ "@" ++ $(gitHash)

main :: IO ()
main = do
  opts <- execParser options

  assetdir <- getXdgDirectory XdgData "mudblood"

  putStrLn $ "Assetdir: " ++ show assetdir

  case optCharacter opts of
    Nothing -> run $ componentGuest assetdir
    Just name -> do
      run $ componentCharacter name (fromMaybe "" (optPassword opts)) assetdir

mgBaseC :: MBComponent SimpleScreen MGEventType () (Fix Nil)
mgBaseC = nilC

portals = []

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
