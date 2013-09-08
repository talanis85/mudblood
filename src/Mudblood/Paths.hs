module Mudblood.Paths
    ( initUserPath
    , existsUserPath
    , readUserFile
    , (</>)
    ) where

import System.Directory
import System.FilePath

initUserPath :: [FilePath] -> IO FilePath
initUserPath p = do
    userdir <- getAppUserDataDirectory "mudblood"
    let combined = userdir </> joinPath p
    createDirectoryIfMissing True combined
    return combined

existsUserPath :: [FilePath] -> IO Bool
existsUserPath p = do
    userdir <- getAppUserDataDirectory "mudblood"
    let combined = userdir </> joinPath p
    doesDirectoryExist combined

readUserFile :: FilePath -> IO (Maybe String)
readUserFile p = do
    ex <- doesFileExist p
    if ex then readFile p >>= return . Just
          else return Nothing
