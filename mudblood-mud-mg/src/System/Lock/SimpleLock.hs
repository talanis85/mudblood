module System.Lock.SimpleLock
  ( acquire
  , release
  , Lock
  ) where

import Control.Monad
import Control.Exception
import System.FileLock

type Lock = FileLock

acquire :: FilePath -> IO (Maybe FileLock)
acquire fp = do
  l <- try (tryLockFile fp Exclusive) :: IO (Either IOError (Maybe FileLock))
  case l of
    Left err -> return Nothing
    Right x -> return x

release :: FileLock -> IO ()
release l = void $ (try $ unlockFile l :: IO (Either IOError ()))
