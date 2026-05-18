{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module Mudblood.Trigger.EventLog
  ( eventLog
  , replayEventLog
  , readEventLog
  ) where

import Mudblood
import Mudblood.Telnet (TelnetNeg)
import Data.GMCP

import Data.Maybe

import GHC.Generics

import qualified Data.Serialize as S
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as B

import System.IO

data LoggedEvent =
    LoggedLineEvent AttrString
  | LoggedSendEvent String
  | LoggedTelnetEvent TelnetNeg
  -- | LoggedGMCPEvent GMCP
  deriving (Generic)

instance S.Serialize LoggedEvent

eventLog :: (MonadIO m, MonadFail m, LineEvent :<: a, SendEvent :<: a, TelnetEvent :<: a, GMCPEvent :<: a) => FilePath -> Trigger (Ev a) m ()
eventLog path = do
  f <- lift $ liftIO $ openFile path AppendMode
  permanent $ do
    ev <- parse' $ msum
      [ LoggedLineEvent <$> fetchLine
      , LoggedSendEvent <$> fetchSend
      , LoggedTelnetEvent <$> fetchTelnet
      -- , LoggedGMCPEvent <$> fetchGMCP
      ]
    let bs = S.encodeLazy ev
        b64 = B64.encode bs
    lift $ liftIO $ B.hPutStr f b64
    lift $ liftIO $ B.hPutStr f $ B.pack [10]

replayEventLog path = do
    f <- lift $ liftIO $ openFile path ReadMode
    c <- lift $ liftIO $ B.hGetContents f
    forM_ (B.split 10 c) $ \x -> do
      case B64.decode x >>= S.decodeLazy of
        Left err -> return ()
        Right ev -> case ev of
                      LoggedLineEvent x -> yieldLine x
                      LoggedSendEvent x -> yieldSend x
                      LoggedTelnetEvent x -> yieldTelnet x
                      -- LoggedGMCPEvent x -> yieldGMCP x

readEventLog :: (LineEvent :<: a, SendEvent :<: a, TelnetEvent :<: a, GMCPEvent :<: a) => FilePath -> IO [Ev a]
readEventLog path = do
    f <- openFile path ReadMode
    c <- B.hGetContents f
    return $ catMaybes $ flip map (B.split 10 c) $ \x ->
      case B64.decode x >>= S.decodeLazy of
        Left err -> mzero
        Right ev -> case ev of
                      LoggedLineEvent x -> return $ mkEv $ LineEvent x
                      LoggedSendEvent x -> return $ mkEv $ SendEvent x
                      LoggedTelnetEvent x -> return $ mkEv $ TelnetEvent x
                      -- LoggedGMCPEvent x -> return $ mkEv $ GMCPEvent x
