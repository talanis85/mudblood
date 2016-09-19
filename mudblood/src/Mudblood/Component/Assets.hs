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

assetsC :: (Monad m, Functor r) => FilePath -> String -> MBComponent m e (Fix r) (Fix (Assets :*: r))
assetsC basePath charName = stateC $ Assets
    { _stBasePath = basePath
    , _stCharName = charName
    }

------------------------------------------------------------------------------

getGameAssetPath asset = do
    basePath <- use (rec . stBasePath)
    return $ basePath </> asset

getCharAssetPath asset = do
    basePath <- use (rec . stBasePath)
    charName <- use (rec . stCharName)
    return $ basePath </> map toLower charName </> asset
