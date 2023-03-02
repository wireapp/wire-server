-- TODO: Move this to some other package, perhaps even upstream?
-- TODO: Chan is perhaps not the best choice, either TChan, TQueue or Chan from unagi-chan would be better
module Wire.API.Federation.HTTP2 where

import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Exception
import qualified Data.Map as Map
import Data.Streaming.Network
import Imports
import qualified Network.HTTP2.Client as HTTP2
import qualified Network.Socket as NS

data HTTP2Conn = HTTP2Conn
  { backgroundThread :: Async (),
    -- See comment in 'startPersistentHTTP2Connection' about what this 'Chan'
    -- contains
    requestChan :: Chan (HTTP2.Request, HTTP2.Response -> IO (), MVar (Async ()))
  }

data HTTP2Manager = HTTP2Manager
  { connections :: MVar (Map (ByteString, Int) HTTP2Conn),
    cacheLimit :: Int
  }

withHTTP2Request :: HTTP2Manager -> ByteString -> Int -> HTTP2.Request -> (HTTP2.Response -> IO a) -> IO a
withHTTP2Request mgr host port req f = do
  reqChan <- modifyMVar (connections mgr) $ \conns -> do
    case Map.lookup (host, port) conns of
      Nothing -> newConn conns
      Just conn ->
        -- If there is a connection for the (host,port), ensure that it is alive
        -- before using it.
        poll (backgroundThread conn) >>= \case
          Nothing -> pure (conns, requestChan conn)
          Just (Left (SomeException _err)) ->
            -- TODO: Log the error, maybe by adding a generic logger function to Http2Manager
            newConn (Map.delete (host, port) conns)
          Just (Right ()) ->
            -- NOTE: or maybe just create a new connection?
            error "impossible: http2 client ended successfully"
  result <- newEmptyMVar
  reqThreadMVar <- newEmptyMVar
  writeChan reqChan (req, putMVar result <=< f, reqThreadMVar)
  reqThread <- takeMVar reqThreadMVar
  let waitForReqThread =
        waitCatch reqThread >>= \case
          Left (SomeException e) -> throw e
          -- The reqeust thread can only exit with an exception or after writing
          -- the 'result' MVar, so this _should_ never happen.
          Right () -> throw FailedToGetResponse
  either id id <$> race (takeMVar result) waitForReqThread
  where
    newConn conns = do
      reqChan <- newChan
      started <- newEmptyMVar
      thread <- async $ startPersistentHTTP2Connection host port (cacheLimit mgr) started reqChan
      takeMVar started
      pure (Map.insert (host, port) (HTTP2Conn thread reqChan) conns, reqChan)

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
  -- empty mvar, written when the client is ready to send requests
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
  Chan (HTTP2.Request, HTTP2.Response -> IO (), MVar (Async ())) ->
  IO ()
startPersistentHTTP2Connection hostname port cacheLimit started reqChan = do
  let clientConfig =
        HTTP2.ClientConfig
          { HTTP2.scheme = "https",
            HTTP2.authority = hostname,
            HTTP2.cacheLimit = cacheLimit
          }
  bracket (fst <$> getSocketTCP hostname port) NS.close $ \sock ->
    bracket (HTTP2.allocSimpleConfig sock 4096) HTTP2.freeSimpleConfig $ \http2Cfg ->
      HTTP2.run clientConfig http2Cfg $ \sendRequest -> do
        putMVar started ()
        forever $ do
          (req, respConsumer, reqThreadMVar) <- readChan reqChan
          putMVar reqThreadMVar =<< async (sendRequest req respConsumer)
