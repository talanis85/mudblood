module Mudblood.Trigger.Logfile
    ( logfileTrigger
    ) where

import System.IO
import Control.Monad.Trans

import Control.Monad

import Mudblood

logfileTrigger :: (MB s m, MonadIO m, LineEvent :<: a, SendEvent :<: a) => String -> Trigger (Ev a) m ()
logfileTrigger filename = do
    fileHandle <- lift $ liftIO $ openFile filename AppendMode
    permanent $ parseU' fetchLoggedEvent >>= logIt fileHandle
  where
    fetchLoggedEvent = msum
      [ fetchLine >>= return . show
      , fetchSend >>= return . ("> " ++)
      ]
    logIt h s = lift $ liftIO $ hPutStrLn h s >> hFlush h
