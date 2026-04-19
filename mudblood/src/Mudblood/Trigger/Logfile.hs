module Mudblood.Trigger.Logfile
    ( logfileTrigger
    ) where

import System.IO
import Control.Monad.Trans

import Control.Monad

import Mudblood

logfileTrigger :: (MonadIO m, MonadFail m, LineEvent :<: a, SendEvent :<: a) => String -> Trigger (Ev a) m ()
logfileTrigger filename = do
    fileHandle <- lift $ liftIO $ openFile filename AppendMode
    permanent $ parse' fetchLoggedEvent >>= logIt fileHandle
  where
    fetchLoggedEvent = msum
      [ fetchLine >>= return . show
      , fetchSend >>= return . ("> " ++)
      ]
    logIt h s = lift $ liftIO $ hPutStrLn h s >> hFlush h
