module Mudblood.Paths
    ( initUserPath
    , readUserFile
    , (</>)
    ) where

import System.Directory
import System.FilePath

initUserPath :: [FilePath] -> IO FilePath
initUserPath p = do
    userdir <- getAppUserDataDirectory "mudblood"
    let combined = joinPath p </> userdir
    createDirectoryIfMissing True combined
    return combined

readUserFile :: FilePath -> IO (Maybe String)
readUserFile p = do
    ex <- doesFileExist p
    if ex then readFile p >>= return . Just
          else return Nothing
