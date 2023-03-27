{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module HTTP2.Client.Manager where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString
import Data.IORef
import Data.Map
import qualified Data.Map as Map
import Data.Streaming.Network
import qualified Network.HTTP2.Client as HTTP2
import qualified Network.Socket as NS
import Prelude

data HTTP2Conn = HTTP2Conn
  { backgroundThread :: Async (),
    -- Maybe the 'disconect' action can also take care of cleaning up the thread
    disconnect :: IO (),
    -- See comment in 'startPersistentHTTP2Connection' about why this returns
    -- '()'
    sendRequest :: HTTP2.Request -> (HTTP2.Response -> IO ()) -> IO ()
  }

type Target = (ByteString, Int)

-- FUTUREWORK: Support HTTPS, perhaps ALPN negatiation can also be used to
-- HTTP1. I think HTTP1 vs HTTP2 can not be negotated without TLS, so perhaps
-- this manager will default to HTTP2.
data HTTP2Manager = HTTP2Manager
  { connections :: TVar (Map Target HTTP2Conn),
    cacheLimit :: Int
  }

defaultHTTP2Manager :: IO HTTP2Manager
defaultHTTP2Manager =
  HTTP2Manager <$> newTVarIO mempty <*> pure 20

-- | Make an HTTP2 request, if it is the first time the 'HTTP2Manager' sees this
-- (server,port) comibination, it creates the connection and keeps it around for
-- any subsequent requests. Subsequest requests try to use this connection, in
-- case the connection is already dead (e.g. the background thread has
-- finished), a new connection is created.
--
-- It is important to consume the response body completely before the
-- continuation can finish.
withHTTP2Request :: HTTP2Manager -> ByteString -> Int -> HTTP2.Request -> (HTTP2.Response -> IO a) -> IO a
withHTTP2Request mgr host port req f = do
  mConn <- atomically $ getConnection mgr host port
  conn <- maybe connect pure mConn
  -- A null pointer! Its is better (faster?) to do this than create 'IORef
  -- (Maybe a)' as we can assume that 'sendRequest' either calls the
  -- continuation or fails, either way the 'readIORef' should never be
  -- evaluating the error.
  result <- newIORef (error "impossible::sendRequest suceeded without calling the continuation")
  sendRequest conn req (writeIORef result <=< f)
  readIORef result
  where
    -- Ensures that any old connection is preserved. This is required to ensure
    -- that concurrent calls to this function don't cause the connections to
    -- leak. It is possible that the connection won't leak because it is waiting
    -- on an MVar and as soon as it gets removed from the map and GC collects
    -- the 'HTTP2Conn', the connection thread _should_ in theory get
    -- 'BlockedIndefinitelyOnMVar' exception. So perhaps this is useless?
    insertNewConn :: HTTP2Conn -> STM (Bool, HTTP2Conn)
    insertNewConn newConn = do
      stateTVar (connections mgr) $ \conns ->
        case Map.lookup (host, port) conns of
          Nothing -> ((True, newConn), Map.insert (host, port) newConn conns)
          Just alreadyEstablishedConn -> ((False, alreadyEstablishedConn), conns)

    connect :: IO HTTP2Conn
    connect = do
      sendReqMVar <- newEmptyMVar
      stopMVar <- newEmptyMVar
      thread <- liftIO . async $ startPersistentHTTP2Connection host port (cacheLimit mgr) sendReqMVar stopMVar
      conn <- HTTP2Conn thread (putMVar stopMVar ()) <$> takeMVar sendReqMVar
      (inserted, finalConn) <- atomically $ insertNewConn conn
      unless inserted $ putMVar stopMVar ()
      pure finalConn

-- | Removes connection from map if it is not alive anymore
getConnection :: HTTP2Manager -> ByteString -> Int -> STM (Maybe HTTP2Conn)
getConnection mgr host port = do
  conns <- readTVar (connections mgr)
  case Map.lookup (host, port) conns of
    Nothing -> pure Nothing
    Just conn ->
      -- If there is a connection for the (host,port), ensure that it is alive
      -- before using it.
      pollSTM (backgroundThread conn) >>= \case
        Nothing -> pure (Just conn)
        Just (Left (SomeException _err)) -> do
          -- TODO: Log the error, maybe by adding a generic logger function to Http2Manager
          writeTVar (connections mgr) $ Map.delete (host, port) conns
          pure Nothing
        Just (Right ()) -> do
          writeTVar (connections mgr) $ Map.delete (host, port) conns
          pure Nothing

-- | Disconnects HTTP2 connection if there exists one. If the background thread
-- running the connection does not finish within 1 second, it is canceled.
--
-- NOTE: Any requests in progress might not finish correctly.
-- FUTUREWORK: Write a safe version of this function.
unsafeDisconnectServer :: HTTP2Manager -> ByteString -> Int -> IO ()
unsafeDisconnectServer mgr host port = do
  mConn <- atomically $ getConnection mgr host port
  case mConn of
    Nothing -> pure ()
    Just conn -> do
      disconnect conn

      -- Wait on two threads:
      -- 1. background thread which _should_ be exiting soon
      -- 2. sleep for 1 second.
      --
      -- whenever one of them finishes, the other is canceled. Errors are
      -- ignored.
      --
      -- All of this to say wait max 1 second for the background thread to
      -- finish.
      waitOneSec <- async $ threadDelay 1_000_000
      _ <- waitAnyCatchCancel [waitOneSec, backgroundThread conn]
      atomically . modifyTVar' (connections mgr) $ Map.delete (host, port)

startPersistentHTTP2Connection ::
  -- hostname
  ByteString ->
  -- port
  Int ->
  -- cacheLimit
  Int ->
  -- empty mvar, written when connection is established and request can be sent,
  -- the MVar will contain a continuation to be used for each request
  --
  -- The response consumer has to return 'IO ()' because we want to processes
  -- different requests on this and 'HTTP2.run' ties the return type of response
  -- consumer to the return type of itself. Even if the response consumer
  -- returned something else we would need another empty MVar to write the
  -- result, this is being done in 'withHTTP2Request'.
  MVar (HTTP2.Request -> (HTTP2.Response -> IO ()) -> IO ()) ->
  -- empty mvar, the connection will wait for this MVar to be written. The
  -- connection will close immediately, any pending requests might get
  -- terminated abruptly.
  --
  -- Another way to terminate the connection would be to cancel the thread which
  -- runs this function
  MVar () ->
  IO ()
startPersistentHTTP2Connection hostname port cl sendReqMVar gracefulStop = do
  let clientConfig =
        HTTP2.ClientConfig
          { HTTP2.scheme = "http",
            HTTP2.authority = hostname,
            HTTP2.cacheLimit = cl
          }
  bracket (fst <$> getSocketTCP hostname port) NS.close $ \sock ->
    bracket (HTTP2.allocSimpleConfig sock 4096) HTTP2.freeSimpleConfig $ \http2Cfg ->
      HTTP2.run clientConfig http2Cfg $ \sendReq -> do
        putMVar sendReqMVar sendReq
        takeMVar gracefulStop
