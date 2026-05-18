{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Mudblood.Component.Assets
  ( Assets
  , assetsC
  , getGameAssetPath
  , getCharAssetPath
  ) where

import Control.Lens
import Data.Char
import System.Directory
import System.FilePath

import Mudblood

------------------------------------------------------------------------------

data Assets a = Assets
    { _stBasePath :: FilePath
    , _stCharName :: String
    }
  deriving (Functor)

makeLenses ''Assets

------------------------------------------------------------------------------

assetsC :: (MonadIO m, Functor r) => FilePath -> String -> MBComponent m e (Fix r) (Fix (Assets :*: r))
assetsC basePath charName = s >>> b
  where
    s = stateC $ Assets
      { _stBasePath = basePath
      , _stCharName = charName
      }
    b = bootC $ liftIO $ createDirectoryIfMissing True (basePath </> map toLower charName)

------------------------------------------------------------------------------

getGameAssetPath asset = do
    basePath <- use (rec . stBasePath)
    return $ basePath </> asset

getCharAssetPath asset = do
    basePath <- use (rec . stBasePath)
    charName <- use (rec . stCharName)
    return $ basePath </> map toLower charName </> asset
