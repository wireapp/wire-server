{-# LANGUAGE RecordWildCards #-}

-- | Basic NATS client implementation
-- This is a minimal implementation to replace RabbitMQ (AMQP) functionality
-- 
-- NATS Protocol Documentation: https://docs.nats.io/reference/reference-protocols/nats-protocol
module Network.NATS.Client
  ( NatsConnection,
    NatsConnectionOpts (..),
    NatsChannel,
    NatsMessage (..),
    NatsEnvelope (..),
    openConnection,
    closeConnection,
    createChannel,
    closeChannel,
    publish,
    subscribe,
    unsubscribe,
    ack,
    nack,
    defaultConnectionOpts,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (bracket, throw, throwIO, try, catch, SomeException)
import Control.Monad (forever, void, when)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (recv, sendAll)
import Imports hiding (take)
import System.IO (Handle, hClose, hFlush, hGetLine, hPutStr)
import System.Random (randomIO)

-- | NATS connection options
data NatsConnectionOpts = NatsConnectionOpts
  { natsServers :: [(String, Int)],
    natsAuth :: Maybe (Text, Text), -- username, password
    natsName :: Maybe Text,
    natsVerbose :: Bool,
    natsPedantic :: Bool,
    natsToken :: Maybe Text
  }

defaultConnectionOpts :: NatsConnectionOpts
defaultConnectionOpts =
  NatsConnectionOpts
    { natsServers = [("127.0.0.1", 4222)],
      natsAuth = Nothing,
      natsName = Nothing,
      natsVerbose = False,
      natsPedantic = False,
      natsToken = Nothing
    }

-- | Represents a NATS connection
data NatsConnection = NatsConnection
  { connSocket :: Socket,
    connNextSid :: IORef Word64,
    connSubscriptions :: IORef (Map Word64 (MVar NatsMessage)),
    connClosed :: IORef Bool
  }

-- | Represents a NATS channel (for compatibility with AMQP interface)
-- In NATS, channels are lightweight and don't have the same semantics as AMQP
newtype NatsChannel = NatsChannel NatsConnection

-- | NATS message
data NatsMessage = NatsMessage
  { msgSubject :: Text,
    msgBody :: LBS.ByteString,
    msgReplyTo :: Maybe Text,
    msgHeaders :: Map Text Text
  }
  deriving (Show)

-- | Envelope for message acknowledgment (for compatibility with AMQP)
data NatsEnvelope = NatsEnvelope
  { envDeliveryTag :: Word64,
    envSubject :: Text
  }
  deriving (Show)

-- | Open a NATS connection
openConnection :: NatsConnectionOpts -> IO NatsConnection
openConnection opts = do
  -- Try to connect to first available server
  conn <- connectToServer (head opts.natsServers)
  
  -- Send CONNECT message
  sendConnect conn opts
  
  -- Read INFO and +OK from server
  receiveServerInfo conn
  
  -- Initialize connection state
  nextSid <- newIORef 0
  subs <- newIORef Map.empty
  closed <- newIORef False
  
  let natsConn = NatsConnection conn nextSid subs closed
  
  -- Start message reader thread
  void $ forkIO $ messageReader natsConn
  
  pure natsConn
  where
    connectToServer :: (String, Int) -> IO Socket
    connectToServer (host, port) = do
      addr <- resolve host (show port)
      sock <- open addr
      pure sock
    
    resolve :: String -> String -> IO AddrInfo
    resolve host port = do
      let hints = defaultHints {addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    
    open :: AddrInfo -> IO Socket
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock (addrAddress addr)
      pure sock

-- | Send CONNECT message to NATS server
sendConnect :: Socket -> NatsConnectionOpts -> IO ()
sendConnect sock opts = do
  -- Simplified CONNECT message
  -- In production, this should be JSON with all options
  let connectMsg = "CONNECT {\"verbose\":false,\"pedantic\":false,\"name\":\"wire-server\"}\r\n"
  sendAll sock (BS8.pack connectMsg)

-- | Receive server INFO
receiveServerInfo :: Socket -> IO ()
receiveServerInfo sock = do
  -- Read INFO line
  void $ recvLine sock
  -- Read +OK or -ERR
  void $ recvLine sock

-- | Receive a line from socket
recvLine :: Socket -> IO ByteString
recvLine sock = go BS.empty
  where
    go acc = do
      chunk <- recv sock 1
      if BS.null chunk
        then pure acc
        else if chunk == BS8.pack "\n"
          then pure acc
          else go (acc <> chunk)

-- | Message reader thread
messageReader :: NatsConnection -> IO ()
messageReader conn = forever $ do
  closed <- readIORef conn.connClosed
  when (not closed) $ do
    line <- recvLine conn.connSocket
    parseAndDispatch conn line
  `catch` \(_ :: SomeException) -> pure ()

-- | Parse and dispatch incoming messages
parseAndDispatch :: NatsConnection -> ByteString -> IO ()
parseAndDispatch conn line = do
  let parts = BS8.words line
  case parts of
    ("MSG" : subject : sid : msgBytes : _) -> do
      let sidNum = read (BS8.unpack sid) :: Word64
      let numBytes = read (BS8.unpack msgBytes) :: Int
      -- Read message payload
      payload <- recvExact conn.connSocket numBytes
      -- Skip \r\n after payload
      void $ recv conn.connSocket 2
      
      subs <- readIORef conn.connSubscriptions
      case Map.lookup sidNum subs of
        Just mvar -> do
          let msg = NatsMessage
                { msgSubject = Text.decodeUtf8 subject,
                  msgBody = LBS.fromStrict payload,
                  msgReplyTo = Nothing,
                  msgHeaders = Map.empty
                }
          void $ tryPutMVar mvar msg
        Nothing -> pure ()
    _ -> pure ()

-- | Receive exact number of bytes
recvExact :: Socket -> Int -> IO ByteString
recvExact sock n = go BS.empty n
  where
    go acc remaining
      | remaining <= 0 = pure acc
      | otherwise = do
          chunk <- recv sock remaining
          if BS.null chunk
            then pure acc
            else go (acc <> chunk) (remaining - BS.length chunk)

-- | Close a NATS connection
closeConnection :: NatsConnection -> IO ()
closeConnection conn = do
  writeIORef conn.connClosed True
  close conn.connSocket

-- | Create a channel (for AMQP compatibility)
createChannel :: NatsConnection -> IO NatsChannel
createChannel = pure . NatsChannel

-- | Close a channel
closeChannel :: NatsChannel -> IO ()
closeChannel _ = pure () -- NATS doesn't have channels

-- | Publish a message
publish :: NatsChannel -> Text -> ByteString -> IO ()
publish (NatsChannel conn) subject payload = do
  let msgSize = BS.length payload
  let pubMsg = BS8.pack $ "PUB " <> Text.unpack subject <> " " <> show msgSize <> "\r\n"
  sendAll conn.connSocket pubMsg
  sendAll conn.connSocket payload
  sendAll conn.connSocket (BS8.pack "\r\n")

-- | Subscribe to a subject
subscribe :: NatsChannel -> Text -> IO Word64
subscribe (NatsChannel conn) subject = do
  sid <- atomicModifyIORef' conn.connNextSid (\s -> (s + 1, s + 1))
  mvar <- newEmptyMVar
  atomicModifyIORef' conn.connSubscriptions $ \subs ->
    (Map.insert sid mvar subs, ())
  
  let subMsg = BS8.pack $ "SUB " <> Text.unpack subject <> " " <> show sid <> "\r\n"
  sendAll conn.connSocket subMsg
  pure sid

-- | Unsubscribe from a subject
unsubscribe :: NatsChannel -> Word64 -> IO ()
unsubscribe (NatsChannel conn) sid = do
  atomicModifyIORef' conn.connSubscriptions $ \subs ->
    (Map.delete sid subs, ())
  let unsubMsg = BS8.pack $ "UNSUB " <> show sid <> "\r\n"
  sendAll conn.connSocket unsubMsg

-- | Acknowledge a message (for AMQP compatibility)
-- NATS doesn't have built-in ACKs, but JetStream does
-- For basic NATS, this is a no-op
ack :: NatsChannel -> Word64 -> IO ()
ack _ _ = pure ()

-- | Negative acknowledgment (for AMQP compatibility)
nack :: NatsChannel -> Word64 -> Bool -> IO ()
nack _ _ _ = pure ()
