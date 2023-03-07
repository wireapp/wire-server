-- TODO: Move this to some other package, perhaps even upstream?
module Wire.API.Federation.HTTP2 where

import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Exception
import qualified Data.Map as Map
import Data.Streaming.Network
import Imports
import qualified Network.HTTP2.Client as HTTP2
import qualified Network.Socket as NS

data HTTP2Conn = HTTP2Conn
  { backgroundThread :: Async (),
    -- Maybe the 'disconect' action can also take care of cleaning up the thread
    disconnect :: IO (),
    -- See comment in 'startPersistentHTTP2Connection' about what this 'Chan'
    -- contains
    sendRequest :: HTTP2.Request -> (HTTP2.Response -> IO ()) -> IO ()
  }

data HTTP2Manager = HTTP2Manager
  { connections :: TVar (Map (ByteString, Int) HTTP2Conn),
    cacheLimit :: Int
  }

withHTTP2Request :: HTTP2Manager -> ByteString -> Int -> HTTP2.Request -> (HTTP2.Response -> IO a) -> IO a
withHTTP2Request mgr host port req f = do
  mConn <- atomically getConnection
  conn <- maybe connect pure mConn
  -- Better than a null pointer
  result <- newIORef (error "impossible")
  sendRequest conn req (writeIORef result <=< f)
  readIORef result
  where
    -- Removes connection from map if it is not alive anymore
    getConnection :: STM (Maybe HTTP2Conn)
    getConnection = do
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
startPersistentHTTP2Connection hostname port cacheLimit sendReqMVar gracefulStop = do
  let clientConfig =
        HTTP2.ClientConfig
          { HTTP2.scheme = "https",
            HTTP2.authority = hostname,
            HTTP2.cacheLimit = cacheLimit
          }
  bracket (fst <$> getSocketTCP hostname port) NS.close $ \sock ->
    bracket (HTTP2.allocSimpleConfig sock 4096) HTTP2.freeSimpleConfig $ \http2Cfg ->
      HTTP2.run clientConfig http2Cfg $ \sendRequest -> do
        putMVar sendReqMVar sendRequest
        takeMVar gracefulStop
