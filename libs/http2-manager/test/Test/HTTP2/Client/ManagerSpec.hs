{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Maybe (isJust)
import Data.Streaming.Network (bindPortTCP, bindRandomPortTCP)
import Data.Unique
import Foreign.Marshal.Alloc (mallocBytes)
import GHC.IO.Exception
import HTTP2.Client.Manager
import Network.HTTP.Types
import qualified Network.HTTP2.Client as Client
import qualified Network.HTTP2.Client as HTTP2
import qualified Network.HTTP2.Server as Server
import Network.Socket
import qualified OpenSSL.Session as SSL
import qualified System.TimeManager
import Test.Hspec

echoTest :: HTTP2Manager -> TLSEnabled -> Int -> Expectation
echoTest mgr tlsEnabled serverPort =
  withHTTP2Request mgr tlsEnabled "localhost" serverPort (Client.requestBuilder "GET" "/echo" [] "some body") $ \res -> do
    Client.responseStatus res `shouldBe` Just status200
    readResponseBody res `shouldReturn` "some body"

spec :: Spec
spec = do
  describe "HTTP2.Client.Manager [without TLS]" $ do
    specTemplate Nothing
  -- TODO: Perhaps there are some TLS specific test cases we might want to
  -- write. Like ensure that the hostname validation works (it currently
  -- does't!).
  describe "HTTP2.Client.Manager [with TLS]" $ do
    localhostCtx <- runIO $ loadServerSSLContext
    specTemplate (Just localhostCtx)

    it "should error appropriately when the server presents a wrong certificate" $ do
      ctx <- loadWrongServerSSLContext
      mgr <- mkTestManager
      withTestServer (Just ctx) $ \TestServer {..} ->
        -- TODO: Specify the specific exception
        echoTest mgr True serverPort `shouldThrow` (\(SomeException _) -> True)

specTemplate :: Maybe SSL.SSLContext -> Spec
specTemplate mCtx = do
  it "should be able to make an HTTP2 request" $ do
    withTestServer mCtx $ \TestServer {..} -> do
      mgr <- mkTestManager

      echoTest mgr (isJust mCtx) serverPort

      readIORef acceptedConns `shouldReturn` 1

  it "should fail appropriately when a server is not available" $ do
    mgr <- mkTestManager

    -- Assumes that nothing is running on this port
    echoTest mgr (isJust mCtx) 42420 `shouldThrow` (\IOError {..} -> ioe_type == NoSuchThing)

  it "should be able to re-use an HTTP2 connection for multiple requests" $ do
    withTestServer mCtx $ \TestServer {..} -> do
      mgr <- mkTestManager

      echoTest mgr (isJust mCtx) serverPort
      echoTest mgr (isJust mCtx) serverPort

      readIORef acceptedConns `shouldReturn` 1

  it "shouldn't try to re-use a disconnected connection" $ do
    withTestServer mCtx $ \TestServer {..} -> do
      mgr <- mkTestManager

      echoTest mgr (isJust mCtx) serverPort
      unsafeDisconnectServer mgr (isJust mCtx) "localhost" serverPort
      echoTest mgr (isJust mCtx) serverPort

      readIORef acceptedConns `shouldReturn` 2

  it "should re-use connections even for concurrent requests" $ do
    withTestServer mCtx $ \TestServer {..} -> do
      mgr <- mkTestManager

      -- Do 1 request before the concurrent ones, otherwise most of them would
      -- of course end up making multiple connections.
      echoTest mgr (isJust mCtx) serverPort
      mapConcurrently_ id $ replicate 10 (echoTest mgr (isJust mCtx) serverPort)

      readIORef acceptedConns `shouldReturn` 1

  it "should re-use the connection even if one of the requests is stuck forever" $ do
    withTestServer mCtx $ \TestServer {..} -> do
      mgr <- mkTestManager
      infiniteRespRecieved <- newEmptyMVar
      chunkOfInfiniteTest <- newEmptyMVar

      infiniteRespThread <- async $ withHTTP2Request mgr (isJust mCtx) "localhost" serverPort (Client.requestNoBody "GET" "/inifite" []) $ \res -> do
        putMVar infiniteRespRecieved ()
        Client.responseStatus res `shouldBe` Just status200

        -- The test server writes "foo\n" 1000 times in each chunk. We do this
        -- to ensure that this request is still alive even after an echo test
        -- which started after this and finished before this.
        takeMVar chunkOfInfiniteTest
        Client.getResponseBodyChunk res `shouldReturn` BS.concat (replicate 1000 "foo\n")

      takeMVar infiniteRespRecieved
      echoTest mgr (isJust mCtx) serverPort

      putMVar chunkOfInfiniteTest ()
      -- We wait on the thread here to ensure that any expectation failures in
      -- the thread are caught.
      wait infiniteRespThread

      readIORef acceptedConns `shouldReturn` 1

  it "should re-use the connection even an exception is thrown while handling a response" $ do
    withTestServer mCtx $ \TestServer {..} -> do
      mgr <- mkTestManager

      let exceptionThrower =
            withHTTP2Request mgr (isJust mCtx) "localhost" serverPort (Client.requestBuilder "GET" "/echo" [] "some body") $ \_ ->
              throw TestException

      -- Also test if the exception is propagated correctly and doesn't cause
      -- other errors
      exceptionThrower `shouldThrow` (== TestException)
      echoTest mgr (isJust mCtx) serverPort

      readIORef acceptedConns `shouldReturn` 1

  it "should fail with appropriate error when a dead connection is used" $ do
    mgr <- mkTestManager
    Just deadConn <- withTestServer mCtx $ \TestServer {..} -> do
      echoTest mgr (isJust mCtx) serverPort
      readIORef acceptedConns `shouldReturn` 1
      Map.lookup (isJust mCtx, "localhost", serverPort) <$> readTVarIO (connections mgr)

    let brokenRequest = sendRequestWithConnection deadConn (Client.requestBuilder "GET" "/echo" [] "some body") $ \_ -> do
          expectationFailure "Expected no response when request is made to a dead server"
    brokenRequest `shouldThrow` (\ConnectionAlreadyClosed -> True)

  it "should create a new connection when the server restarts" $ do
    mgr <- mkTestManager
    port <- withTestServer mCtx $ \TestServer {..} -> do
      echoTest mgr (isJust mCtx) serverPort
      readIORef acceptedConns `shouldReturn` 1
      pure serverPort

    -- See "should fail with appropriate error when a dead connection is used"
    -- to know what happens when we don't wait for the background thread to go
    -- away.
    Just deadConn <- Map.lookup (isJust mCtx, "localhost", port) <$> readTVarIO (connections mgr)
    wait $ backgroundThread deadConn

    withTestServerOnPort mCtx port $ \TestServer {..} -> do
      echoTest mgr (isJust mCtx) port
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

loadServerSSLContext :: IO SSL.SSLContext
loadServerSSLContext = do
  ctx <- SSL.context
  SSL.contextSetCertificateFile ctx "test/resources/localhost.pem"
  SSL.contextSetPrivateKeyFile ctx "test/resources/localhost-key.pem"
  SSL.contextSetALPNProtos ctx ["h2"]
  SSL.contextSetCiphers ctx "HIGH"
  sslCheck <- SSL.contextCheckPrivateKey ctx
  sslCheck `shouldBe` True
  pure ctx

loadWrongServerSSLContext :: IO SSL.SSLContext
loadWrongServerSSLContext = do
  ctx <- SSL.context
  SSL.contextSetCertificateFile ctx "test/resources/localhost.example.com.pem"
  SSL.contextSetPrivateKeyFile ctx "test/resources/localhost.example.com-key.pem"
  SSL.contextSetALPNProtos ctx ["h2"]
  SSL.contextSetCiphers ctx "HIGH"
  sslCheck <- SSL.contextCheckPrivateKey ctx
  sslCheck `shouldBe` True
  pure ctx

mkTestManager :: IO HTTP2Manager
mkTestManager = do
  mgr <- defaultHTTP2Manager
  SSL.contextSetCAFile (sslContext mgr) "test/resources/unit-ca.pem"
  pure mgr

withTestServer :: Maybe SSL.SSLContext -> (TestServer -> IO a) -> IO a
withTestServer mCtx action = do
  bracket (bindRandomPortTCP "*") (close . snd) $ \(port, sock) ->
    withTestServerOnSocket mCtx action (port, sock)

withTestServerOnPort :: Maybe SSL.SSLContext -> Int -> (TestServer -> IO a) -> IO a
withTestServerOnPort mCtx serverPort action = do
  bracket (bindPortTCP serverPort "*") close $ \listenSock ->
    withTestServerOnSocket mCtx action (serverPort, listenSock)

withTestServerOnSocket :: Maybe SSL.SSLContext -> (TestServer -> IO a) -> (Int, Socket) -> IO a
withTestServerOnSocket mCtx action (serverPort, listenSock) = do
  acceptedConns <- newIORef 0
  liveConns <- newIORef mempty
  let cleanupServer serverThread = do
        cancel serverThread
        mapM_ cancel =<< readIORef liveConns
  bracket (async $ testServerOnSocket mCtx listenSock acceptedConns liveConns) cleanupServer $ \serverThread ->
    action TestServer {..}

allocServerConfig :: Either Socket SSL.SSL -> IO Server.Config
allocServerConfig (Left sock) = HTTP2.allocSimpleConfig sock 4096
allocServerConfig (Right ssl) = do
  let bufsize = 4096
  buf <- mallocBytes bufsize
  timmgr <- System.TimeManager.initialize $ 30 * 1000000
  -- Sometimes the frame header says that the payload length is 0. Reading 0
  -- bytes multiple times seems to be causing errors in openssl. I cannot figure
  -- out why. The previous implementation didn't try to read from the socket
  -- when trying to read 0 bytes, so special handling for 0 maintains that
  -- behaviour.
  let readData prevChunk 0 = pure prevChunk
      readData prevChunk n = do
        -- Handling SSL.ConnectionAbruptlyTerminated as a stream end
        -- (some sites terminate SSL connection right after returning the data).
        chunk <- SSL.read ssl n `catch` \(_ :: SSL.ConnectionAbruptlyTerminated) -> pure mempty
        let chunkLen = BS.length chunk
        if
            | chunkLen == 0 || chunkLen == n ->
                pure (prevChunk <> chunk)
            | chunkLen > n ->
                error "openssl: SSL.read returned more bytes than asked for, this is probably a bug"
            | otherwise ->
                readData (prevChunk <> chunk) (n - chunkLen)
  pure
    Server.Config
      { Server.confWriteBuffer = buf,
        Server.confBufferSize = bufsize,
        Server.confSendAll = SSL.write ssl,
        Server.confReadN = readData mempty,
        Server.confPositionReadMaker = Server.defaultPositionReadMaker,
        Server.confTimeoutManager = timmgr
      }

testServerOnSocket :: Maybe SSL.SSLContext -> Socket -> IORef Int -> IORef (Map Unique (Async ())) -> IO ()
testServerOnSocket mCtx listenSock connsCounter conns = do
  listen listenSock 1024
  forever $ do
    (sock, _) <- accept listenSock
    serverCfgParam <- case mCtx of
      Nothing -> pure $ Left sock
      Just ctx -> do
        ssl <- SSL.connection ctx sock
        SSL.accept ssl
        pure (Right ssl)
    connKey <- newUnique
    modifyIORef connsCounter (+ 1)
    let shutdownSSL = case serverCfgParam of
          Left _ -> pure ()
          Right ssl -> SSL.shutdown ssl SSL.Bidirectional
        cleanup cfg = do
          Server.freeSimpleConfig cfg `finally` (shutdownSSL `finally` close sock)
    thread <- async $ bracket (allocServerConfig serverCfgParam) cleanup $ \cfg -> do
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
