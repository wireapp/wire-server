-- TODO: Move this to some other package, perhaps even upstream?
-- TODO: Chan is perhaps not the best choice, either TChan, TQueue or Chan from unagi-chan would be better
module Wire.API.Federation.HTTP2 where

import Control.Concurrent.Async
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
  { -- TODO: Look into some STM based map to avoid contention
    connections :: MVar (Map (ByteString, Int) HTTP2Conn),
    cacheLimit :: Int
  }

withHTTP2Request :: HTTP2Manager -> ByteString -> Int -> HTTP2.Request -> (HTTP2.Response -> IO a) -> IO a
withHTTP2Request mgr host port req f = do
  sendReq <- modifyMVar (connections mgr) $ \conns -> do
    case Map.lookup (host, port) conns of
      Nothing -> newConn conns
      Just conn ->
        -- If there is a connection for the (host,port), ensure that it is alive
        -- before using it.
        poll (backgroundThread conn) >>= \case
          Nothing -> pure (conns, sendRequest conn)
          Just (Left (SomeException _err)) ->
            -- TODO: Log the error, maybe by adding a generic logger function to Http2Manager
            newConn (Map.delete (host, port) conns)
          Just (Right ()) ->
            -- NOTE: or maybe just create a new connection?
            error "impossible: http2 client ended successfully"
  result <- newEmptyMVar
  sendReq req (putMVar result <=< f)
  -- The reqeust can only exit with an exception or after writing
  -- the 'result' MVar, so the `Nothing` case _should_ never happen.
  maybe (throw FailedToGetResponse) pure =<< tryTakeMVar result
  where
    newConn conns = do
      sendReqMVar <- newEmptyMVar
      stopMVar <- newEmptyMVar
      thread <- async $ startPersistentHTTP2Connection host port (cacheLimit mgr) sendReqMVar stopMVar
      sr <- takeMVar sendReqMVar
      pure (Map.insert (host, port) (HTTP2Conn thread (putMVar stopMVar ()) sr) conns, sr)

data Http2ManagerError = FailedToGetResponse
  deriving (Show)

instance Exception Http2ManagerError

startPersistentHTTP2Connection ::
  -- hostname
  ByteString ->
  -- port
  Int ->
  -- cacheLimit
  Int ->
  -- empty mvar, written when connection is established and request can be sent,
  -- the MVar will contain a continuation to be used for each request
  MVar (HTTP2.Request -> (HTTP2.Response -> IO ()) -> IO ()) ->
  -- empty mvar, the connection will wait for this MVar to be written. The
  -- connection will close immediately, any pending requests might get
  -- terminated abruptly.
  --
  -- Another way to terminate the connection would be to cancel the thread which
  -- runs this function
  MVar () ->
  -- Channel for recieveing requests to be executed, contains a request, a
  -- response consumer and an empty mvar for the handle to thread actually
  -- running the request.
  --
  -- The response consumer has to return 'IO ()' because we want to processes
  -- different requests on this and 'HTTP2.run' ties the return type of response
  -- consumer to the return type of itself. Even if the response consumer
  -- returned something else we would need another empty MVar to write the
  -- result, this is being done in 'withHTTP2Request'.
  -- Chan (HTTP2.Request, HTTP2.Response -> IO (), MVar (Async ())) ->
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
