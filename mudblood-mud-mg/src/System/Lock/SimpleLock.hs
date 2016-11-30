module System.Lock.SimpleLock
  ( acquire
  , release
  , Lock
  ) where

import Control.Monad
import Control.Exception
import System.Lock.FLock

acquire :: FilePath -> IO (Maybe Lock)
acquire fp = do
  l <- try (lock fp Exclusive NoBlock) :: IO (Either IOError Lock)
  case l of
    Left err -> return Nothing
    Right l  -> return $ Just l

release :: Lock -> IO ()
release l = void $ (try $ unlock l :: IO (Either IOError ()))
