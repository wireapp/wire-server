{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTTP2.Client.Manager.Internal where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString
import qualified Data.ByteString as BS
import Data.ByteString.UTF8 as UTF8
import Data.IORef
import Data.Map
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Streaming.Network
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Unique
import GHC.IO.Exception
import qualified Network.HTTP2.Client as HTTP2
import qualified Network.Socket as NS
import qualified OpenSSL.Session as SSL
import System.IO.Error
import System.Timeout
import Prelude

data HTTP2Conn = HTTP2Conn
  { backgroundThread :: Async (),
    disconnect :: IO (),
    connectionActionMVar :: MVar ConnectionAction
  }

type TLSEnabled = Bool

type HostName = ByteString

type Port = Int

type Target = (TLSEnabled, HostName, Port)

data ConnectionAction
  = SendRequest Request
  | CloseConnection

data Request = Request
  { -- | The request to be sent.
    request :: HTTP2.Request,
    -- | Consumer for the response, must not exit until the response body is
    -- completely consumed.
    --
    -- The response consumer has to return 'IO ()' because we want to processes
    -- different requests on one connection and 'HTTP2.run' ties the return type
    -- of response consumer to the return type of itself. Even if the response
    -- consumer returned something else we would need another empty MVar to
    -- write the result, this is being dealt with in
    -- 'sendRequestWithConnection'.
    responseConsumer :: HTTP2.Response -> IO (),
    -- | There are exceptions which cannot be communicated in a continuation
    -- becasue they can be raised even before the continuation starts. We also
    -- need a way to communicate any exceptions raised by the continuation
    -- itself. This 'MVar' will be written to in any of those cases.
    exceptionMVar :: MVar SomeException
  }

-- | FUTUREWORK: Support HTTPS, perhaps ALPN negotiation can also be used to
-- HTTP1. I think HTTP1 vs HTTP2 can not be negotated without TLS, so perhaps
-- this manager will default to HTTP2.
data Http2Manager = Http2Manager
  { connections :: TVar (Map Target HTTP2Conn),
    cacheLimit :: Int,
    -- | In microseconds, defaults to 30s
    tcpConnectionTimeout :: Int,
    sslContext :: SSL.SSLContext,
    sslRemoveTrailingDot :: Bool
  }

defaultHttp2Manager :: IO Http2Manager
defaultHttp2Manager = do
  ctx <- SSL.context
  SSL.contextSetVerificationMode ctx $
    SSL.VerifyPeer
      { vpFailIfNoPeerCert = True,
        -- Only relevant when running as server
        vpClientOnce = False,
        vpCallback = Nothing
      }
  SSL.contextSetALPNProtos ctx ["h2"]
  http2ManagerWithSSLCtx ctx

http2ManagerWithSSLCtx :: SSL.SSLContext -> IO Http2Manager
http2ManagerWithSSLCtx sslContext = do
  connections <- newTVarIO mempty
  let cacheLimit = 20
      tcpConnectionTimeout = 30_000_000
      sslRemoveTrailingDot = False
  pure $ Http2Manager {..}

-- | Warning: This won't affect already established connections
setCacheLimit :: Int -> Http2Manager -> Http2Manager
setCacheLimit cl mgr = mgr {cacheLimit = cl}

-- | Warning: This won't affect already established connections
setSSLContext :: SSL.SSLContext -> Http2Manager -> Http2Manager
setSSLContext ctx mgr = mgr {sslContext = ctx}

-- | Remove traling dots in hostname while verifying hostname in the certificate
-- presented by the server. For instance, when connecting with
-- 'foo.example.com.' (Note the trailing dot) by default most SSL libraries fail
-- hostname verification if the server has a certificate for 'foo.example.com'
-- (Note the lack of a trailing dot). Setting this flag makes the hostname
-- verification succeed for these hosts. However, this will make the hostname
-- verification fail if the host presents a certificate which does have a
-- trailing dot.
--
-- Discussion about why this is not implemented as a flag on 'SSLContext':
-- https://github.com/openssl/openssl/issues/11560
--
-- Warning: This won't affect already established connections
setSSLRemoveTrailingDot :: Bool -> Http2Manager -> Http2Manager
setSSLRemoveTrailingDot b mgr = mgr {sslRemoveTrailingDot = b}

-- | In microseconds
setTCPConnectionTimeout :: Int -> Http2Manager -> Http2Manager
setTCPConnectionTimeout n mgr = mgr {tcpConnectionTimeout = n}

-- | Does not check whether connection is actually running. Users should use
-- 'withHTTP2Request'. This function is good for testing.
sendRequestWithConnection :: HTTP2Conn -> HTTP2.Request -> (HTTP2.Response -> IO r) -> IO r
sendRequestWithConnection conn req k = do
  result :: MVar r <- newEmptyMVar
  threadKilled :: MVar SomeException <- newEmptyMVar
  putMVar (connectionActionMVar conn) (SendRequest (Request req (putMVar result <=< k) threadKilled))
  race (takeMVar result) (takeMVar threadKilled) >>= \case
    Left r -> pure r
    Right (SomeException e) -> throw e

-- | Make an HTTP2 request, if it is the first time the 'Http2Manager' sees this
-- target, it creates the connection and keeps it around for
-- any subsequent requests. Subsequest requests try to use this connection, in
-- case the connection is already dead (e.g. the background thread has
-- finished), a new connection is created.
--
-- It is important that the continuation provided by the caller of this function
-- consumes the response body completely before it returns.
--
-- NOTE: If many concurrent requests are made to the same server using a single
-- instance of 'Http2Manager', it could cause the manager to make multiple
-- connections to the server. Eventually only one connection will be kept open.
-- This, in theory, would cause some contention over 'STM' based 'Map' that the
-- 'Http2Manager' keeps and so could decrease throughput. In cases where many
-- concurrent requests are to be made, it might be best to ensure that a
-- connection exists using 'connectIfNotAlreadyConnected' before making all the
-- requests.
withHTTP2Request :: Http2Manager -> Target -> HTTP2.Request -> (HTTP2.Response -> IO a) -> IO a
withHTTP2Request mgr target req k = do
  conn <- getOrMakeConnection mgr target
  sendRequestWithConnection conn req k

-- | Temporary workaround for https://github.com/kazu-yamamoto/http2/issues/102
withHTTP2RequestOnSingleUseConn :: Http2Manager -> Target -> HTTP2.Request -> (HTTP2.Response -> IO a) -> IO a
withHTTP2RequestOnSingleUseConn Http2Manager {..} target req k = do
  sendReqMVar <- newEmptyMVar
  thread <- liftIO . async $ startPersistentHTTP2Connection sslContext target cacheLimit sslRemoveTrailingDot tcpConnectionTimeout sendReqMVar
  let newConn = HTTP2Conn thread (putMVar sendReqMVar CloseConnection) sendReqMVar
  sendRequestWithConnection newConn req $ \resp -> do
    k resp <* disconnect newConn

-- | Connects to a server if it is not already connected, useful when making
-- many concurrent requests. This way the first few requests don't have to fight
-- for making a connection This way the first few requests don't have to fight
-- for making a connection.
connectIfNotAlreadyConnected :: Http2Manager -> Target -> IO ()
connectIfNotAlreadyConnected mgr target = void $ getOrMakeConnection mgr target

-- | Gets a connection if it exists and is alive, otherwise connects to the
-- given 'Target'.
getOrMakeConnection :: Http2Manager -> Target -> IO HTTP2Conn
getOrMakeConnection mgr@Http2Manager {..} target = do
  mConn <- atomically $ getConnection mgr target
  maybe connect pure mConn
  where
    -- Ensures that any old connection is preserved. This is required to ensure
    -- that concurrent calls to this function don't cause the connections to
    -- leak.
    insertNewConn :: HTTP2Conn -> STM (Bool, HTTP2Conn)
    insertNewConn newConn = do
      stateTVar connections $ \conns ->
        case Map.lookup target conns of
          Nothing -> ((True, newConn), Map.insert target newConn conns)
          Just alreadyEstablishedConn -> ((False, alreadyEstablishedConn), conns)

    connect :: IO HTTP2Conn
    connect = do
      sendReqMVar <- newEmptyMVar
      thread <- liftIO . async $ startPersistentHTTP2Connection sslContext target cacheLimit sslRemoveTrailingDot tcpConnectionTimeout sendReqMVar
      let newConn = HTTP2Conn thread (putMVar sendReqMVar CloseConnection) sendReqMVar
      (inserted, finalConn) <- atomically $ insertNewConn newConn
      unless inserted $ do
        -- It is possible that the connection won't leak because it is waiting
        -- on an MVar and as soon as it gets removed from the map and GC collects
        -- the 'HTTP2Conn', the connection thread _should_ in theory get
        -- 'BlockedIndefinitelyOnMVar' exception. So perhaps this is useless?
        disconnect newConn
      pure finalConn

-- | Removes connection from map if it is not alive anymore
getConnection :: Http2Manager -> Target -> STM (Maybe HTTP2Conn)
getConnection mgr target = do
  conns <- readTVar (connections mgr)
  case Map.lookup target conns of
    Nothing -> pure Nothing
    Just conn ->
      -- If there is a connection for the target, ensure that it is alive
      -- before using it.
      pollSTM (backgroundThread conn) >>= \case
        Nothing -> pure (Just conn)
        Just _ -> do
          -- Maybe there is value in logging any exceptions we
          -- receive here. But logging in STM will be tricky, and the threads
          -- running requests on the connection which got an exception would've
          -- anyway received the exception, so maybe it is not as valuable.
          writeTVar (connections mgr) $ Map.delete target conns
          pure Nothing

-- | Disconnects HTTP2 connection if there exists one. Will hang around until
-- all the ongoing requests complete. This would throw an error if the
-- background thread maintaining the connection throws an error, e.g. there was
-- a TLS error or the connection was already disconnected with error.
disconnectTarget :: Http2Manager -> Target -> IO ()
disconnectTarget mgr target = do
  mConn <- atomically $ getConnection mgr target
  case mConn of
    Nothing -> pure ()
    Just conn -> do
      disconnect conn
      wait (backgroundThread conn)
        `finally` (atomically . modifyTVar' (connections mgr) $ Map.delete target)

-- | Disconnects HTTP2 connection if there exists one. If the background thread
-- running the connection does not finish within 1 second, it is canceled.
-- Errors from the background thread running the connection are not propagated.
--
-- NOTE: Any requests in progress might not finish correctly.
disconnectTargetWithTimeout :: Http2Manager -> Target -> Int -> IO ()
disconnectTargetWithTimeout mgr target microSeconds = do
  mConn <- atomically $ getConnection mgr target
  case mConn of
    Nothing -> pure ()
    Just conn -> do
      disconnect conn

      -- Wait with timeout using two threads:
      -- 1. background thread which _should_ be exiting soon
      -- 2. sleep for given number of microseconds.
      --
      -- whenever one of them finishes, the other is canceled. Errors are
      -- ignored.
      --
      -- All of this to say wait max 1 second for the background thread to
      -- finish.
      let waitWithTimeout = do
            waitOneSec <- async $ threadDelay microSeconds
            void $ waitAnyCatchCancel [waitOneSec, backgroundThread conn]

      waitWithTimeout
        `finally` (atomically . modifyTVar' (connections mgr) $ Map.delete target)

startPersistentHTTP2Connection ::
  SSL.SSLContext ->
  Target ->
  -- cacheLimit
  Int ->
  -- sslRemoveTrailingDot
  Bool ->
  -- | TCP connect timeout in microseconds
  Int ->
  -- MVar used to communicate requests or the need to close the connection.  (We could use a
  -- queue here to queue several requests, but since the requestor has to wait for the
  -- response, it might as well block before sending off the request.)
  MVar ConnectionAction ->
  IO ()
startPersistentHTTP2Connection ctx (tlsEnabled, hostname, port) cl removeTrailingDot tcpConnectTimeout sendReqMVar = do
  liveReqs <- newIORef mempty
  let clientConfig =
        HTTP2.defaultClientConfig
          { HTTP2.scheme = if tlsEnabled then "https" else "http",
            HTTP2.authority = UTF8.toString hostname,
            HTTP2.cacheLimit = cl
          }
      -- Sends error to requests which show up too late, i.e. after the
      -- connection is already closed
      tooLateNotifier e = forever $ do
        takeMVar sendReqMVar >>= \case
          SendRequest Request {..} -> do
            -- No need to get stuck here
            void $ tryPutMVar exceptionMVar (SomeException e)
          CloseConnection -> pure ()

      -- Sends errors to the request threads when an error occurs
      cleanupThreadsWith (SomeException e) = do
        -- Is it really OK to cancel the remaining threads even when throwing
        -- 'ConnectionAlreadyClosed', there is a chance that they could finish,
        -- but how would we know here?
        mapM_ (\(thread, _) -> cancelWith thread e) =<< readIORef liveReqs
        -- Spawns a thread that will hang around for 1 second to deal with
        -- the race betwen main thread sending a request and this thread
        -- already having stoped waiting for new requests. Sending requests
        -- after 'handleRequests' has finsihed just causes the main thread
        -- to hang until recieving 'BlockedIndefinitelyOnMVar'.
        --
        -- 1 second is hopefully enough to ensure that this thread is seen
        -- as finished.
        void $ async $ race_ (tooLateNotifier e) (threadDelay 1_000_000)

      hostnameForTLS =
        if removeTrailingDot
          then fromMaybe hostname (stripSuffix "." hostname)
          else hostname
      transportConfig
        | tlsEnabled = Just $ TLSParams ctx hostnameForTLS
        | otherwise = Nothing

  handle cleanupThreadsWith $
    bracket connectTCPWithTimeout NS.close $ \sock -> do
      bracket (mkTransport sock transportConfig) cleanupTransport $ \transport ->
        bracket (allocHTTP2Config transport) HTTP2.freeSimpleConfig $ \http2Cfg -> do
          let runAction = HTTP2.run clientConfig http2Cfg $ \sendReq _aux -> do
                handleRequests liveReqs sendReq
          -- Any request threads still hanging about after 'runAction' finishes
          -- are canceled with 'ConnectionAlreadyClosed'.
          flip finally (cleanupThreadsWith (SomeException ConnectionAlreadyClosed)) $
            -- Any exceptions thrown will get re-thrown to any running requests,
            -- handle at the top level is not good as 'finally' wrapping this
            -- function would kill all threads with some other exception.
            handle cleanupThreadsWith $
              runAction
  where
    handleRequests :: IORef LiveReqs -> SendReqFn -> IO ()
    handleRequests liveReqs sendReq = do
      let waitAndFork = do
            reqOrStop <- takeMVar sendReqMVar
            case reqOrStop of
              SendRequest r@(Request {..}) -> do
                processRequest liveReqs sendReq r
                  `catches` exceptionHandlers exceptionMVar
                waitAndFork
              CloseConnection -> do
                mapM_ (wait . fst) =<< readIORef liveReqs
      waitAndFork

    processRequest :: IORef LiveReqs -> SendReqFn -> Request -> IO ()
    processRequest liveReqs sendReq Request {..} = do
      unique <- newUnique
      thread <- async $ do
        let actionWithHandlers =
              sendReq request responseConsumer
                `catches` exceptionHandlers exceptionMVar
            cleanup = do
              atomicModifyIORef' liveReqs (\m -> (Map.delete unique m, ()))
        actionWithHandlers `finally` cleanup
      atomicModifyIORef' liveReqs (\m -> (Map.insert unique (thread, exceptionMVar) m, ()))

    -- Specially handle 'ConnectionAlreadyClosed' otherwise it shows up as 'SomeAsyncException'
    tooLateHandler threadKilled e@ConnectionAlreadyClosed =
      putMVar threadKilled (SomeException e)
    generalHandler threadKilled e = putMVar threadKilled e
    exceptionHandlers threadKilled = [Handler $ tooLateHandler threadKilled, Handler $ generalHandler threadKilled]

    connectTCPWithTimeout :: IO NS.Socket
    connectTCPWithTimeout = do
      mSock <- timeout tcpConnectTimeout $ fst <$> getSocketTCP hostname port
      case mSock of
        Just sock -> pure sock
        Nothing -> do
          let errStr =
                "TCP connection with "
                  <> Text.unpack (Text.decodeUtf8 hostname)
                  <> ":"
                  <> show port
                  <> " took longer than "
                  <> show tcpConnectTimeout
                  <> " microseconds"
          throwIO $ mkIOError TimeExpired errStr Nothing Nothing

type LiveReqs = Map Unique (Async (), MVar SomeException)

type SendReqFn = HTTP2.Request -> (HTTP2.Response -> IO ()) -> IO ()

data Transport
  = InsecureTransport NS.Socket
  | SecureTransport SSL.SSL NS.Socket

data TLSParams = TLSParams
  { context :: SSL.SSLContext,
    hostname :: HostName
  }

mkTransport :: NS.Socket -> Maybe TLSParams -> IO Transport
mkTransport sock Nothing = pure $ InsecureTransport sock
mkTransport sock (Just TLSParams {..}) = do
  ssl <- SSL.connection context sock
  let hostnameStr = Text.unpack $ Text.decodeUtf8 hostname
  -- Perhaps a hook at enable/disable or customize this would be nice.
  -- OpenSSL also supports a callback.
  SSL.setTlsextHostName ssl hostnameStr
  SSL.enableHostnameValidation ssl hostnameStr
  SSL.connect ssl
  pure $ SecureTransport ssl sock

cleanupTransport :: Transport -> IO ()
cleanupTransport (InsecureTransport _) = pure ()
cleanupTransport (SecureTransport ssl _) = SSL.shutdown ssl SSL.Unidirectional

data ConnectionAlreadyClosed = ConnectionAlreadyClosed
  deriving (Show)

instance Exception ConnectionAlreadyClosed

bufsize :: Int
bufsize = 4096

allocHTTP2Config :: Transport -> IO HTTP2.Config
allocHTTP2Config (InsecureTransport sock) = HTTP2.allocSimpleConfig sock bufsize
allocHTTP2Config (SecureTransport ssl sock) = do
  config <- HTTP2.allocSimpleConfig sock bufsize
  pure $
    config
      { HTTP2.confSendAll = SSL.write ssl,
        HTTP2.confReadN = readData
      }
  where
    -- Sometimes the frame header says that the payload length is 0. Reading 0
    -- bytes multiple times seems to be causing errors in openssl. I cannot figure
    -- out why. The previous implementation didn't try to read from the socket
    -- when trying to read 0 bytes, so special handling for 0 maintains that
    -- behaviour.
    readData :: Int -> IO ByteString
    readData 0 = pure mempty
    readData n = loop mempty n

    loop :: ByteString -> Int -> IO ByteString
    loop acc 0 = pure acc
    loop acc n = do
      -- Handling SSL.ConnectionAbruptlyTerminated as a stream end
      -- (some sites terminate SSL connection right after returning the data).
      chunk <- SSL.read ssl n `catch` \(_ :: SSL.ConnectionAbruptlyTerminated) -> pure mempty
      let chunkLen = BS.length chunk
      if
          | chunkLen == 0 || chunkLen == n ->
              pure (acc <> chunk)
          | chunkLen > n ->
              error "openssl: SSL.read returned more bytes than asked for, this is probably a bug"
          | otherwise ->
              loop (acc <> chunk) (n - chunkLen)
