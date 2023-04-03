{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.HTTP2.Client.ManagerSpec where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Streaming.Network (bindPortTCP, bindRandomPortTCP)
import Data.Unique
import GHC.IO.Exception
import HTTP2.Client.Manager
import Network.HTTP.Types
import qualified Network.HTTP2.Client as Client
import qualified Network.HTTP2.Server as Server
import Network.Socket
import Test.Hspec

echoTest :: HTTP2Manager -> Int -> Expectation
echoTest mgr serverPort =
  withHTTP2Request mgr "localhost" serverPort (Client.requestBuilder "GET" "/echo" [] "some body") $ \res -> do
    Client.responseStatus res `shouldBe` Just status200
    readResponseBody res `shouldReturn` "some body"

spec :: Spec
spec = describe "HTTP2.Client.Manager" $ do
  it "should be able to make an HTTP2 request" $ do
    withTestServer $ \TestServer {..} -> do
      mgr <- defaultHTTP2Manager

      echoTest mgr serverPort

      readIORef acceptedConns `shouldReturn` 1

  it "should fail appropriately when a server is not available" $ do
    mgr <- defaultHTTP2Manager

    -- Assumes that nothing is running on this port
    echoTest mgr 42420 `shouldThrow` (\IOError {..} -> ioe_type == NoSuchThing)

  it "should be able to re-use an HTTP2 connection for multiple requests" $ do
    withTestServer $ \TestServer {..} -> do
      mgr <- defaultHTTP2Manager

      echoTest mgr serverPort
      echoTest mgr serverPort

      readIORef acceptedConns `shouldReturn` 1

  it "shouldn't try to re-use a disconnected connection" $ do
    withTestServer $ \TestServer {..} -> do
      mgr <- defaultHTTP2Manager

      echoTest mgr serverPort
      unsafeDisconnectServer mgr "localhost" serverPort
      echoTest mgr serverPort

      readIORef acceptedConns `shouldReturn` 2

  it "should re-use connections even for concurrent requests" $ do
    withTestServer $ \TestServer {..} -> do
      mgr <- defaultHTTP2Manager

      -- Do 1 request before the concurrent ones, otherwise most of them would
      -- of course end up making multiple connections.
      echoTest mgr serverPort
      mapConcurrently_ id $ replicate 10 (echoTest mgr serverPort)

      readIORef acceptedConns `shouldReturn` 1

  it "should re-use the connection even if one of the requests is stuck forever" $ do
    withTestServer $ \TestServer {..} -> do
      mgr <- defaultHTTP2Manager

      infiniteRespRecieved <- newEmptyMVar
      chunkOfInfiniteTest <- newEmptyMVar

      infiniteRespThread <- async $ withHTTP2Request mgr "localhost" serverPort (Client.requestNoBody "GET" "/inifite" []) $ \res -> do
        putMVar infiniteRespRecieved ()
        Client.responseStatus res `shouldBe` Just status200

        -- The test server writes "foo\n" in each chunk. We do this to ensure
        -- that this request is still alive even after an echo test which
        -- started after this and finished before this.
        takeMVar chunkOfInfiniteTest
        Client.getResponseBodyChunk res `shouldReturn` BS.concat (replicate 1000 "foo\n")

      takeMVar infiniteRespRecieved
      echoTest mgr serverPort

      putMVar chunkOfInfiniteTest ()
      -- We wait on the thread here to ensure that any expectation failures in
      -- the thread are caught.
      wait infiniteRespThread

      readIORef acceptedConns `shouldReturn` 1

  it "should re-use the connection even an exception is thrown while handling a response" $ do
    withTestServer $ \TestServer {..} -> do
      mgr <- defaultHTTP2Manager

      let exceptionThrower =
            withHTTP2Request mgr "localhost" serverPort (Client.requestBuilder "GET" "/echo" [] "some body") $ \_ ->
              throw TestException

      -- Also test if the exception is propagated correctly and doesn't cause
      -- other errors
      exceptionThrower `shouldThrow` (== TestException)
      echoTest mgr serverPort

      readIORef acceptedConns `shouldReturn` 1

  it "should fail with appropriate error when a dead connection is used" $ do
    mgr <- defaultHTTP2Manager
    Just deadConn <- withTestServer $ \TestServer {..} -> do
      echoTest mgr serverPort
      readIORef acceptedConns `shouldReturn` 1
      Map.lookup ("localhost", serverPort) <$> readTVarIO (connections mgr)

    let brokenRequest = sendRequestWithConnection deadConn (Client.requestBuilder "GET" "/echo" [] "some body") $ \_ -> do
          expectationFailure "Expected no response when request is made to a dead server"
    brokenRequest `shouldThrow` (\ConnectionAlreadyClosed -> True)

  it "should create a new connection when the server restarts" $ do
    mgr <- defaultHTTP2Manager
    port <- withTestServer $ \TestServer {..} -> do
      echoTest mgr serverPort
      readIORef acceptedConns `shouldReturn` 1
      pure serverPort

    -- See "should fail with appropriate error when a dead connection is used"
    -- to know what happens when we don't wait for the background thread to go
    -- away.
    Just deadConn <- Map.lookup ("localhost", port) <$> readTVarIO (connections mgr)
    wait $ backgroundThread deadConn

    withTestServerOnPort port $ \TestServer {..} -> do
      echoTest mgr port
      -- this is still 1 because we have a new 'TestServer'
      readIORef acceptedConns `shouldReturn` 1

data TestException = TestException
  deriving (Show, Eq)

instance Exception TestException

data TestServer = TestServer
  { serverThread :: Async (),
    acceptedConns :: IORef Int,
    liveConns :: IORef (Map Unique (Async ())),
    serverPort :: Int
  }

withTestServer :: (TestServer -> IO a) -> IO a
withTestServer action = do
  bracket (bindRandomPortTCP "*") (close . snd) (withTestServerOnSocket action)

withTestServerOnPort :: Int -> (TestServer -> IO a) -> IO a
withTestServerOnPort serverPort action = do
  bracket (bindPortTCP serverPort "*") close $ \listenSock ->
    withTestServerOnSocket action (serverPort, listenSock)

withTestServerOnSocket :: (TestServer -> IO a) -> (Int, Socket) -> IO a
withTestServerOnSocket action (serverPort, listenSock) = do
  acceptedConns <- newIORef 0
  liveConns <- newIORef mempty
  let cleanupServer serverThread = do
        cancel serverThread
        mapM_ cancel =<< readIORef liveConns
  bracket (async $ testServerTCP listenSock acceptedConns liveConns) cleanupServer $ \serverThread ->
    action TestServer {..}

testServerTCP :: Socket -> IORef Int -> IORef (Map Unique (Async ())) -> IO ()
testServerTCP listenSock connsCounter conns = do
  listen listenSock 1024
  forever $ do
    (sock, _) <- accept listenSock
    connKey <- newUnique
    modifyIORef connsCounter (+ 1)
    let cleanup cfg = do
          Client.freeSimpleConfig cfg
          close sock
    thread <- async $ bracket (Server.allocSimpleConfig sock 4096) cleanup $ \cfg -> do
      Server.run cfg testServer `finally` modifyIORef conns (Map.delete connKey)
    modifyIORef conns $ Map.insert connKey thread

testServer :: Server.Request -> Server.Aux -> (Server.Response -> [Server.PushPromise] -> IO ()) -> IO ()
testServer req _ respWriter = do
  case Server.requestPath req of
    Just "/echo" -> do
      reqBody <- readRequestBody req
      respWriter (Server.responseBuilder status200 [] (Builder.lazyByteString reqBody)) []
    Just "/inifite" -> do
      let infiniteBSWriter :: (Builder.Builder -> IO ()) -> IO () -> IO ()
          infiniteBSWriter bsWriter flush = do
            bsWriter $ Builder.lazyByteString $ LBS.concat $ replicate 1000 "foo\n"
            flush
            infiniteBSWriter bsWriter flush
          infiniteResponse = Server.responseStreaming status200 [] infiniteBSWriter
      respWriter infiniteResponse []
    _ -> do
      respWriter (Server.responseNoBody status404 []) []

readResponseBody :: Client.Response -> IO LBS.ByteString
readResponseBody res = readChunks (Client.getResponseBodyChunk res)

readRequestBody :: Server.Request -> IO LBS.ByteString
readRequestBody req = readChunks (Server.getRequestBodyChunk req)

readChunks :: IO ByteString -> IO LBS.ByteString
readChunks action = LBS.fromChunks <$> go []
  where
    go chunks = do
      action >>= \case
        "" -> pure chunks
        c -> go (c : chunks)
