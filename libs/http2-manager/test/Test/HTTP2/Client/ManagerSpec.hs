{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.HTTP2.Client.ManagerSpec where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Streaming.Network (bindRandomPortGen)
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
        Client.getResponseBodyChunk res `shouldReturn` "foo\n"

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

data TestException = TestException
  deriving (Show, Eq)

instance Exception TestException

data TestServer = TestServer
  { serverThread :: Async (),
    acceptedConns :: IORef Int,
    serverPort :: Int
  }

withTestServer :: (TestServer -> IO a) -> IO a
withTestServer action = do
  bracket (bindRandomPortGen Stream "*") (close . snd) $ \(serverPort, listenSock) -> do
    acceptedConns <- newIORef 0
    serverThread <- async $ testServerTCP listenSock acceptedConns
    action TestServer {..}

testServerTCP :: Socket -> IORef Int -> IO ()
testServerTCP listenSock connsCounter = do
  listen listenSock 1024
  forever $ do
    (sock, _) <- accept listenSock
    modifyIORef connsCounter (+ 1)
    -- This can leak!
    async $ bracket (Client.allocSimpleConfig sock 4096) Client.freeSimpleConfig $ \cfg -> do
      Server.run cfg testServer

testServer :: Server.Request -> Server.Aux -> (Server.Response -> [Server.PushPromise] -> IO ()) -> IO ()
testServer req _ respWriter = do
  case Server.requestPath req of
    Just "/echo" -> do
      reqBody <- readRequestBody req
      respWriter (Server.responseBuilder status200 [] (Builder.lazyByteString reqBody)) []
    Just "/inifite" -> do
      let infiniteBSWriter :: (Builder.Builder -> IO ()) -> IO () -> IO ()
          infiniteBSWriter bsWriter flush = do
            bsWriter "foo\n"
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
