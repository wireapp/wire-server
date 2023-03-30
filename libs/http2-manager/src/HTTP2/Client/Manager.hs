{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.Unique
import qualified Network.HTTP2.Client as HTTP2
import qualified Network.Socket as NS
import Prelude

data HTTP2Conn = HTTP2Conn
  { backgroundThread :: Async (),
    disconnect :: IO (),
    -- See comment in 'startPersistentHTTP2Connection' about why this returns
    -- '()'
    sendRequestMVar :: MVar (Either Request ())
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

-- | Does not check whether connection is actually running. Users should use
-- 'withHTTP2Request'. This function is good for testing.
sendRequestWithConnection :: HTTP2Conn -> HTTP2.Request -> (HTTP2.Response -> IO a) -> IO a
sendRequestWithConnection conn req f = do
  result <- newEmptyMVar
  threadKilled <- newEmptyMVar
  putMVar (sendRequestMVar conn) (Left (Request req (putMVar result <=< f) threadKilled))
  race (takeMVar result) (takeMVar threadKilled) >>= \case
    Left x -> pure x
    Right (SomeException e) -> throw e

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
  sendRequestWithConnection conn req f
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
      thread <- liftIO . async $ startPersistentHTTP2Connection host port (cacheLimit mgr) sendReqMVar
      let conn = HTTP2Conn thread (putMVar sendReqMVar (Right ())) sendReqMVar
      (inserted, finalConn) <- atomically $ insertNewConn conn
      unless inserted $ disconnect conn
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
    -- | MVar to communicate lack of response due to an exception.
    exceptionMVar :: MVar SomeException
  }

-- | Used to close a persistent connection gracefully
type CloseConnection = ()

startPersistentHTTP2Connection ::
  -- hostname
  ByteString ->
  -- port
  Int ->
  -- cacheLimit
  Int ->
  -- MVar used to communicate requests or the need to close the connection.
  MVar (Either Request CloseConnection) ->
  IO ()
startPersistentHTTP2Connection hostname port cl sendReqMVar = do
  let clientConfig =
        HTTP2.ClientConfig
          { HTTP2.scheme = "http",
            HTTP2.authority = hostname,
            HTTP2.cacheLimit = cl
          }
  -- What if the first bracket fails? The other thread will probably still write
  -- to the MVar and nobody will listen to it. TODO: Write a test and handle
  -- this.
  bracket (fst <$> getSocketTCP hostname port) NS.close $ \sock ->
    bracket (HTTP2.allocSimpleConfig sock 4096) HTTP2.freeSimpleConfig $ \http2Cfg -> do
      liveReqs <- newIORef mempty
      let tooLateNotifier = forever $ do
            takeMVar sendReqMVar >>= \case
              Left Request {..} -> do
                -- No need to get stuck here
                void $ tryPutMVar exceptionMVar (SomeException ConnectionAlreadyClosed)
              _ -> pure ()
      let cleanup = do
            -- Is it really OK to cancel the remaining threads because if there
            -- are any threads here?
            mapM_ (\(thread, _) -> cancelWith thread ConnectionAlreadyClosed) =<< readIORef liveReqs
            -- Spawns a thread that will hang around for 1 second to deal with
            -- the race betwen main thread sending a request and this thread
            -- already having stoped waiting for new requests. Sending requests
            -- after 'handleRequests' has finsihed just causes the main thread
            -- to hang until recieving 'BlockedIndefinitelyOnMVar'.
            --
            -- 1 second is hopefully enough to ensure that this thread is seen
            -- as finished.
            void $ async $ race_ tooLateNotifier (threadDelay 1_000_000)
      flip finally cleanup $ HTTP2.run clientConfig http2Cfg $ \sendReq -> do
        handleRequests liveReqs sendReq
  where
    handleRequests :: IORef (Map Unique (Async (), MVar SomeException)) -> (HTTP2.Request -> (HTTP2.Response -> IO ()) -> IO ()) -> IO ()
    handleRequests liveReqs sendReq = do
      let waitAndFork = do
            reqOrStop <- takeMVar sendReqMVar
            case reqOrStop of
              Left r@(Request {..}) -> do
                processRequest liveReqs sendReq r
                  `catches` exceptionHandlers exceptionMVar
                waitAndFork
              Right () -> do
                -- TODO: Maybe add a timeout?
                mapM_ (wait . fst) =<< readIORef liveReqs
      waitAndFork

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

data ConnectionAlreadyClosed = ConnectionAlreadyClosed
  deriving (Show)

instance Exception ConnectionAlreadyClosed
