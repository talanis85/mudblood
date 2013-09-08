{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mudblood.Telnet
    ( TelnetSocket
    , TelnetIO
    , TelnetEvent (TelnetRawEvent, TelnetNegEvent, TelnetCloseEvent)
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
    , telnetNegToBytes
    , telnetSubneg
    , telnetNegNaws
) where

import Data.Word

import Control.Exception

import Control.Monad.Writer
import Control.Monad.State

import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary.Put

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

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
    deriving (Show, Eq)

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
                  | OPT_EOR
                  | OPT_NAWS
                  | OPT_GMCP
    deriving (Show, Eq)

toTelnetOption :: (Integral a) => a -> TelnetOption
toTelnetOption x = case x of
    25 -> OPT_EOR
    31 -> OPT_NAWS
    201 -> OPT_GMCP
    -- etc.
    x -> OPT_UNKNOWN (fromIntegral x)

fromTelnetOption :: (Integral a) => TelnetOption -> a
fromTelnetOption opt = case opt of
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
} deriving (Eq)

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
    tnRemaining :: [Word8],
    tnTelnegState :: TelnegState
} deriving (Show)

data TelnetBlock = TelnetRawBlock [Word8]
                 | TelnetNegBlock TelnetNeg
    deriving (Show)

tnFeed :: TelnetState -> [Word8] -> (TelnetState, [TelnetBlock])
tnFeed state xs = runWriter $ parse state { tnRemaining = ((tnRemaining state) ++ xs) }
    where
        parse state = 
              case tnRemaining state of
                   []   -> do
                           when ((tnParsed state) /= []) $ tell [TelnetRawBlock (tnParsed state)]
                           return state { tnParsed = [] }
                   (x:xs) -> let newstate = state { tnRemaining = xs }
                             in case telnegParse (tnTelnegState state) x of
                                     (tnstate, TelnegNone) ->
                                        parse $ newstate {
                                           tnParsed = ((tnParsed newstate) ++ [x]),
                                           tnTelnegState = tnstate
                                        }
                                     (tnstate, TelnegPartial) ->
                                        do
                                        when ((tnParsed newstate) /= []) $ tell [TelnetRawBlock (tnParsed newstate)]
                                        parse $ newstate {
                                           tnTelnegState = tnstate,
                                           tnParsed = []
                                        }
                                     (tnstate, TelnegComplete tn) ->
                                        do
                                        tell [TelnetNegBlock tn]
                                        parse $ newstate {
                                            tnTelnegState = tnstate
                                        }

tnInit :: TelnetState
tnInit = TelnetState [] [] TelnegStateOff



type TelnetSocket = Socket

data TelnetSocketState = TelnetSocketState {
    tnSocket :: TelnetSocket,
    tnState :: TelnetState
}

newtype TelnetIO a = TelnetIO (StateT TelnetSocketState IO a)
    deriving (Monad, MonadIO)

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
    d <- liftIO (recv (tnSocket state) 1024)
    if B.null d then return $ Left "EOF"
                else let (st, blocks) = tnFeed (tnState state) $ B.unpack d
                     in put' (state { tnState = st }) >> return (Right blocks)
  where get' = TelnetIO get
        put' s = TelnetIO $ put s

telnetSend :: TelnetSocket -> [Word8] -> IO ()
telnetSend sock dat = send sock (B.pack dat) >> return ()

telnetClose :: TelnetSocket -> IO ()
telnetClose sock = sClose sock

telnetSubneg :: TelnetOption -> [Word8] -> TelnetNeg
telnetSubneg opt dat = TelnetNeg (Just CMD_SB) (Just opt) (dat ++ [255, 240])

telnetNegNaws :: Int -> Int -> TelnetNeg
telnetNegNaws w h = telnetSubneg OPT_NAWS str
    where str = BL.unpack $ runPut (putWord16le (fromIntegral w) >> putWord16le (fromIntegral h))
