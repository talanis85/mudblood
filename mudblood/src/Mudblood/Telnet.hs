{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, DeriveGeneric #-}

module Mudblood.Telnet
    ( TelnetSocket
    , TelnetIO
    , TelnetEvent (TelnetRawEvent, TelnetNegEvent, TelnetCloseEvent)
    , Communication (..), Sendable
    -- * Telnet socket primitives
    , telnetSend
    , telnetConnect
    , telnetClose
    -- * Receive handler
    , telnetRecvHandler
    -- * Telnet negotiations
    , TelnetNeg (..)
    , TelnetCommand (..)
    , TelnetOption (..)
    , telnetSubneg
    , telnetNegNaws
) where

import Prelude hiding (getContents)

import Data.Word
import Data.GMCP

import Control.Exception
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Control.Concurrent

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Codec.Binary.UTF8.String as UTF8

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy

import Data.Binary.Put
import GHC.Generics
import Data.Serialize (Serialize)

instance Serialize TelnetCommand
instance Serialize TelnetOption
instance Serialize TelnetNeg

-- | Telneg commands
data TelnetCommand = CMD_UNKNOWN Word8
                   | CMD_EOR
                   | CMD_WILL
                   | CMD_WONT
                   | CMD_DO
                   | CMD_DONT
                   | CMD_SB
                   | CMD_SE
                   | CMD_NOP
    deriving (Show, Eq, Generic)

toTelnetCommand :: (Integral a) => a -> TelnetCommand
toTelnetCommand x = case x of
    251 -> CMD_WILL
    252 -> CMD_WONT
    253 -> CMD_DO
    254 -> CMD_DONT
    241 -> CMD_NOP
    250 -> CMD_SB
    240 -> CMD_SE
    239 -> CMD_EOR
    x -> CMD_UNKNOWN (fromIntegral x)

fromTelnetCommand :: (Integral a) => TelnetCommand -> a
fromTelnetCommand cmd = case cmd of
    CMD_WILL -> 251
    CMD_WONT -> 252
    CMD_DO   -> 253
    CMD_DONT -> 254
    CMD_NOP  -> 241
    CMD_SB   -> 250
    CMD_SE   -> 240
    CMD_EOR  -> 239
    CMD_UNKNOWN x -> (fromIntegral x)

-- | Telneg options
data TelnetOption = OPT_UNKNOWN Word8
                  | OPT_TIMING_MARK
                  | OPT_EOR
                  | OPT_NAWS
                  | OPT_GMCP
    deriving (Show, Eq, Generic)

toTelnetOption :: (Integral a) => a -> TelnetOption
toTelnetOption x = case x of
    6   -> OPT_TIMING_MARK
    25  -> OPT_EOR
    31  -> OPT_NAWS
    201 -> OPT_GMCP
    -- etc.
    x -> OPT_UNKNOWN (fromIntegral x)

fromTelnetOption :: (Integral a) => TelnetOption -> a
fromTelnetOption opt = case opt of
    OPT_TIMING_MARK -> 6
    OPT_EOR -> 25
    OPT_NAWS -> 31
    OPT_GMCP -> 201
    OPT_UNKNOWN x -> (fromIntegral x)

-- | A telnet negotiation consists of a command, an option and
--   some binary data.
data TelnetNeg = TelnetNeg {
    tnCommand :: Maybe TelnetCommand,
    tnOption :: Maybe TelnetOption,
    tnData :: [Word8]
} deriving (Eq, Generic)

instance Show TelnetNeg where
    show (TelnetNeg cmd opt dat) = "Telneg: " ++ cmd' ++ " " ++ opt' ++ " " ++ (show dat)
        where cmd' = case cmd of
                        Just x  -> show x
                        Nothing -> "CMD_NONE"
              opt' = case opt of
                        Just x  -> show x
                        Nothing -> "OPT_NONE"

telnetNegToBytes :: TelnetNeg -> [Word8]
telnetNegToBytes neg = [255] ++ c ++ o ++ (tnData neg)
    where c = case tnCommand neg of
                Just c' -> [fromTelnetCommand c']
                Nothing -> []
          o = case tnOption neg of
                Just o' -> [fromTelnetOption o']
                Nothing -> []

data TelnegState = TelnegStateOff
                 | TelnegStateCommand
                 | TelnegStateOption TelnetCommand
                 | TelnegStateSBOption
                 | TelnegStateSBData TelnetOption [Word8]
                 | TelnegStateSE TelnetOption [Word8]
    deriving (Show)

data TelnegResult = TelnegNone
                  | TelnegPartial
                  | TelnegComplete TelnetNeg
    deriving (Show)

-- | Most beautiful custom telneg parser
telnegParse :: TelnegState -> Word8 -> (TelnegState, TelnegResult)
telnegParse state ch = do
    case state of
        TelnegStateOff -> if ch == 255
                          then (TelnegStateCommand, TelnegPartial)
                          else (TelnegStateOff, TelnegNone)
        TelnegStateCommand -> if ch < 240
                              then (TelnegStateOff, TelnegComplete (TelnetNeg (Just $ toTelnetCommand ch) Nothing []))
                              else if ch == 250
                                   then (TelnegStateSBOption, TelnegPartial)
                                   else (TelnegStateOption (toTelnetCommand ch), TelnegPartial)
        TelnegStateOption com -> (TelnegStateOff, TelnegComplete (TelnetNeg (Just com) (Just $ toTelnetOption ch) []))
        TelnegStateSBOption -> (TelnegStateSBData (toTelnetOption ch) [], TelnegPartial)
        TelnegStateSBData opt dat -> if ch == 255
                                     then (TelnegStateSE opt dat, TelnegPartial)
                                     else (TelnegStateSBData opt (dat ++ [ch]), TelnegPartial)
        TelnegStateSE opt dat -> if ch == 240
                                 then (TelnegStateOff, TelnegComplete (TelnetNeg (Just CMD_SB) (Just opt) dat))
                                 else (TelnegStateOff, TelnegNone)

data TelnetState = TelnetState {
    tnParsed :: [Word8],
    tnTelnegState :: TelnegState
} deriving (Show)

data TelnetBlock = TelnetRawBlock [Word8]
                 | TelnetNegBlock TelnetNeg
    deriving (Show)

tnFold :: ([TelnetBlock], [Word8], TelnegState) -> Word8 -> ([TelnetBlock], [Word8], TelnegState)
tnFold (bs, cs, st) c = case telnegParse st c of
     (st', TelnegNone)        -> (bs, cs ++ [c], st')
     (st', TelnegPartial)     -> if cs == [] then (bs, [], st')
                                             else (bs ++ [TelnetRawBlock (filterSpecials cs)], [], st')
     (st', TelnegComplete tn) -> if cs == [] then (bs ++ [TelnetNegBlock tn], [], st')
                                             else (bs ++ [TelnetRawBlock (filterSpecials cs), TelnetNegBlock tn], [], st')

filterSpecials = filterCR . filterBackspace

filterCR = filter (/= 13)

filterBackspace [] = []
filterBackspace (x:8:r) = filterBackspace r
filterBackspace (x:xs) = x : filterBackspace xs

tnInit :: TelnetState
tnInit = TelnetState [] TelnegStateOff

type TelnetSocket = Socket

data TelnetSocketState = TelnetSocketState {
    tnSocket :: TelnetSocket,
    tnState :: TelnetState
}

newtype TelnetIO a = TelnetIO (StateT TelnetSocketState IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

data TelnetEvent = TelnetRawEvent [Word8]
                 | TelnetNegEvent TelnetNeg
                 | TelnetCloseEvent String
    deriving (Show, Eq)

-- | Open a telnet connection and fork a handler routine.
telnetConnect :: String                     -- ^ Host
              -> String                     -- ^ Port
              -> TelnetIO ()                -- ^ Handler routine
              -> IO (Either String TelnetSocket) -- ^ The resulting socket or an error

telnetConnect host port action = withSocketsDo $ do
    ret <- try $ do
        ai <- getAddrInfo Nothing (Just host) (Just port)
        case ai of
             [] -> throw $ userError "getAddrInfo: Unknown error"
             ((AddrInfo _ family socktype proto addr _):_) ->
                 do
                 sock <- socket family socktype proto
                 ret <- connect sock addr
                 let (TelnetIO action') = action
                 forkIO $ (runStateT action' (TelnetSocketState sock tnInit)) >> return ()
                 return sock
      :: IO (Either IOError TelnetSocket)
    case ret of
        Left err -> return $ Left (show err)
        Right sock -> return $ Right sock

-- | Telnet handler function that reads data from the socket and passes telnet
--   events to a user function.
telnetRecvHandler :: (TelnetEvent -> TelnetIO ())   -- ^ The user handler
                  -> TelnetIO ()

telnetRecvHandler handler = do
    blocks <- telnetRecv
    case blocks of
        Right bs -> do
            forM bs $ \b -> case b of
                TelnetRawBlock d -> handler $ TelnetRawEvent d
                TelnetNegBlock n -> handler $ TelnetNegEvent n
            telnetRecvHandler handler
        Left err -> handler $ TelnetCloseEvent err

telnetRecv :: TelnetIO (Either String [TelnetBlock])
telnetRecv =
    do
    state <- get'
    d <- liftIO $ try $ recv (tnSocket state) 1024 :: TelnetIO (Either IOException BL.ByteString)
    case d of
        Left err -> return $ Left "Socket error"
        Right d ->
            if BL.null d then return $ Left "EOF"
                         else do
                              let ts = tnState state
                                  (blocks, openBytes, negstate) = BL.foldl tnFold ([], tnParsed ts, tnTelnegState ts) d
                              put' $ state { tnState = TelnetState { tnParsed = [], tnTelnegState = negstate } }
                              if openBytes == [] then return $ Right blocks
                                                 else return $ Right $ blocks ++ [TelnetRawBlock (filterSpecials openBytes)]
  where get' = TelnetIO get
        put' s = TelnetIO $ put s

data Communication = forall a. Sendable a => Communication a

instance Show Communication where
    show (Communication x) = show $ toBinary x

class (Show a) => Sendable a where
    toBinary :: a -> [Word8]

instance Sendable Communication where
    toBinary (Communication c) = toBinary c

instance Sendable [Word8] where
    toBinary = id

instance Sendable String where
    toBinary str = UTF8.encode $ str ++ "\n"

instance Sendable TelnetNeg where
    toBinary neg = telnetNegToBytes neg

instance Sendable GMCP where
    toBinary gmcp = toBinary $ telnetSubneg OPT_GMCP $ UTF8.encode $ dumpGMCP gmcp

telnetSend :: TelnetSocket -> Communication -> IO ()
telnetSend sock (Communication dat) = send sock (BL.pack (toBinary dat)) >> return ()

telnetClose :: TelnetSocket -> IO ()
telnetClose sock = close sock

telnetSubneg :: TelnetOption -> [Word8] -> TelnetNeg
telnetSubneg opt dat = TelnetNeg (Just CMD_SB) (Just opt) (dat ++ [255, 240])

telnetNegNaws :: Int -> Int -> TelnetNeg
telnetNegNaws w h = telnetSubneg OPT_NAWS str
    where str = BL.unpack $ runPut (putWord16le (fromIntegral w) >> putWord16le (fromIntegral h))
