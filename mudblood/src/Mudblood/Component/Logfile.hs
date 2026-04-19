module Mudblood.Component.Logfile
    ( logfileC
    ) where

import System.IO
import Control.Monad.Trans

import Control.Monad

import Mudblood
import Mudblood.Component.Assets

logfileTrigger :: (MonadIO m, MonadFail m, Assets :@: r, LineEvent :<: a, SendEvent :<: a) => Trigger (Ev a) (MB (Fix r) m) ()
logfileTrigger = do
    filename <- lift $ getCharAssetPath "log"
    fileHandle <- lift $ liftIO $ openFile filename AppendMode
    permanent $ parse' fetchLoggedEvent >>= logIt fileHandle
  where
    fetchLoggedEvent = msum
      [ fetchLine >>= return . show
      , fetchSend >>= return . ("> " ++)
      ]
    logIt h s = lift $ liftIO $ hPutStrLn h s >> hFlush h

logfileC :: (MonadIO m, MonadFail m, Assets :@: r, LineEvent :<: e, SendEvent :<: e) => MBComponent m e (Fix r) (Fix r)
logfileC = triggerC 1000 $ logfileTrigger
