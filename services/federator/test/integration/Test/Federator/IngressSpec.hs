-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Federator.IngressSpec where

import Control.Concurrent.Async (async, cancel)
import Control.Exception (ErrorCall (ErrorCall), bracket, finally)
import Control.Exception qualified as E
import Control.Lens (view)
import Control.Monad.Catch (throwM)
import Control.Monad.Codensity
import Data.Aeson qualified as Aeson
import Data.Binary.Builder
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.Domain
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (UserLegalHoldNoConsent))
import Data.Streaming.Network (bindRandomPortTCP)
import Data.String.Conversions
import Data.Text.Encoding qualified as Text
import Federator.Discovery
import Federator.Monitor (FederationSetupError)
import Federator.Monitor.Internal (mkSSLContextWithoutCert)
import Federator.Remote
import Foreign.Marshal.Alloc (mallocBytes)
import HTTP2.Client.Manager (http2ManagerWithSSLCtx)
import Imports
import Network.HTTP.Types qualified as HTTP
import Network.HTTP2.Server (defaultServerConfig)
import Network.HTTP2.Server qualified as HTTP2
import Network.HTTP2.Server.Internal qualified as HTTP2
import Network.Socket (Socket, close)
import Network.Socket qualified as NS
import OpenSSL.Session (SSLContext)
import OpenSSL.Session qualified as SSL
import Polysemy
import Polysemy.Embed
import Polysemy.Error
import Polysemy.Input
import Servant.Client.Core
import System.TimeManager qualified
import Test.Federator.Util
import Test.Hspec
import Util.Options (Endpoint (Endpoint))
import Wire.API.Federation.Client
import Wire.API.Federation.Component
import Wire.API.Federation.Domain
import Wire.API.User
import Wire.Network.DNS.SRV

-- | This module contains tests for the interface between federator and ingress.  Ingress is
-- mocked with nginz.
spec :: TestEnv -> Spec
spec env = do
  describe "Ingress" $ do
    it "should be accessible using http2 and forward to the local brig" $
      runTestFederator env $ do
        brig <- view teBrig <$> ask
        user <- randomUser brig

        let expectedProfile = mkUserProfile EmailVisibleToSelf user Nothing UserLegalHoldNoConsent
        runTestSem $ do
          resp <-
            liftToCodensity
              . assertNoError @RemoteError
              $ inwardBrigCallViaIngress
                "get-users-by-ids"
                (Aeson.fromEncoding (Aeson.toEncoding [userId user]))
          embed . lift @Codensity $ do
            bdy <- streamingResponseStrictBody resp
            let actualProfile = Aeson.decode (toLazyByteString bdy)
            responseStatusCode resp `shouldBe` HTTP.status200
            actualProfile `shouldBe` Just [expectedProfile]
    it "should strip the trailing dot from authority when calling a discovered target" $
      let authorityCheckingHandler :: HTTP2.Request -> IO HTTP2.Response
          authorityCheckingHandler req =
            pure $
              case HTTP2.requestAuthority req of
                Just "localhost" ->
                  HTTP2.responseNoBody HTTP.status200 []
                Just authority ->
                  HTTP2.responseBuilder (HTTP.mkStatus 421 "Misdirected Request") [] (Builder.stringUtf8 authority)
                Nothing ->
                  HTTP2.responseNoBody HTTP.status400 []
       in withMockHttp2TlsServer authorityCheckingHandler $ \port ->
            runTestFederator env $
              runTestSem $ do
                resp <-
                  liftToCodensity
                    . assertNoError @RemoteError
                    $ callRemoteTarget
                      (SrvTarget "localhost." (fromIntegral port))
                      "test"
                      mempty
                embed . lift @Codensity $
                  responseStatusCode resp `shouldBe` HTTP.status200

  it "testRejectRequestsWithoutClientCertIngress" (testRejectRequestsWithoutClientCertIngress env)

-- @SF.Federation @TSFI.RESTfulAPI @S2 @S3 @S7
--
-- This test was primarily intended to test that federator is using the API right (header
-- name etc.), but it is also effectively testing that federator rejects clients without
-- certificates that have been validated by ingress.
--
-- We can't test end-to-end here: the TLS termination happens in k8s, and would have to be
-- tested there (and with a good emulation of the concrete configuration of the prod
-- system).
testRejectRequestsWithoutClientCertIngress :: TestEnv -> IO ()
testRejectRequestsWithoutClientCertIngress env = runTestFederator env $ do
  brig <- view teBrig <$> ask
  user <- randomUser brig
  hdl <- randomHandle
  _ <- putHandle brig (userId user) hdl

  settings <- view teSettings
  sslCtxWithoutCert <-
    either (throwM @_ @FederationSetupError) pure
      <=< runM
      . runEmbedded (liftIO @(TestFederator IO))
      . runError
      $ mkSSLContextWithoutCert settings
  runTestSem $ do
    r <-
      runError @RemoteError $
        inwardBrigCallViaIngressWithSettings
          sslCtxWithoutCert
          "get-user-by-handle"
          (Aeson.fromEncoding (Aeson.toEncoding hdl))
    liftToCodensity . embed $ case r of
      Right _ -> expectationFailure "Expected client certificate error, got response"
      Left (RemoteError {}) ->
        expectationFailure "Expected client certificate error, got remote error"
      Left (RemoteErrorResponse _ _ status _) -> status `shouldBe` HTTP.status400

-- @END

liftToCodensity :: (Member (Embed (Codensity IO)) r) => Sem (Embed IO ': r) a -> Sem r a
liftToCodensity = runEmbedded @IO @(Codensity IO) lift

runTestSem :: Sem '[Input TestEnv, Embed (Codensity IO)] a -> TestFederator IO a
runTestSem action = do
  e <- ask
  liftIO . lowerCodensity . runM . runInputConst e $ action

discoverConst :: SrvTarget -> Sem (DiscoverFederator ': r) a -> Sem r a
discoverConst target = interpret $ \case
  DiscoverFederator _ -> pure (Right target)
  DiscoverAllFederators _ -> pure (Right (pure target))

inwardBrigCallViaIngress ::
  (Members [Input TestEnv, Embed (Codensity IO), Error RemoteError] r) =>
  Text ->
  Builder ->
  Sem r StreamingResponse
inwardBrigCallViaIngress path payload = do
  sslCtx <- inputs (view teSSLContext)
  inwardBrigCallViaIngressWithSettings sslCtx path payload

inwardBrigCallViaIngressWithSettings ::
  (Members [Input TestEnv, Embed (Codensity IO), Error RemoteError] r) =>
  SSLContext ->
  Text ->
  Builder ->
  Sem r StreamingResponse
inwardBrigCallViaIngressWithSettings sslCtx requestPath payload =
  do
    Endpoint ingressHost ingressPort <- nginxIngress . view teTstOpts <$> input
    originDomain <- originDomain . view teTstOpts <$> input
    let target = SrvTarget (cs ingressHost) ingressPort
        headers = [(originDomainHeaderName, Text.encodeUtf8 originDomain)]
    callRemoteTargetWithSettings sslCtx target requestPath headers payload

callRemoteTarget ::
  (Members [Input TestEnv, Embed (Codensity IO), Error RemoteError] r) =>
  SrvTarget ->
  Text ->
  Builder ->
  Sem r StreamingResponse
callRemoteTarget target requestPath payload = do
  sslCtx <- inputs (view teSSLContext)
  originDomain <- originDomain . view teTstOpts <$> input
  let headers = [(originDomainHeaderName, Text.encodeUtf8 originDomain)]
  callRemoteTargetWithSettings sslCtx target requestPath headers payload

callRemoteTargetWithSettings ::
  (Members [Input TestEnv, Embed (Codensity IO), Error RemoteError] r) =>
  SSLContext ->
  SrvTarget ->
  Text ->
  [HTTP.Header] ->
  Builder ->
  Sem r StreamingResponse
callRemoteTargetWithSettings sslCtx target requestPath headers payload = do
  mgr <- liftToCodensity . liftIO $ http2ManagerWithSSLCtx sslCtx
  liftToCodensity
    . runInputConst mgr
    . runInputConst (RequestId defRequestId)
    . assertNoError @DiscoveryFailure
    . discoverConst target
    . interpretRemote
    $ discoverAndCall (Domain "example.com") Brig requestPath headers payload

withMockHttp2TlsServer ::
  (HTTP2.Request -> IO HTTP2.Response) ->
  (Int -> IO a) ->
  IO a
withMockHttp2TlsServer handler action = do
  sslCtx <- loadMockServerSSLContext
  bracket (bindRandomPortTCP "*") (close . snd) $ \(port, sock) ->
    bracket (async $ mockHttp2TlsServerOnSocket sslCtx sock) cancel (const (action port))
  where
    loadMockServerSSLContext :: IO SSLContext
    loadMockServerSSLContext = do
      ctx <- SSL.context
      SSL.contextSetCertificateFile ctx "test/resources/integration-leaf.pem"
      SSL.contextSetPrivateKeyFile ctx "test/resources/integration-leaf-key.pem"
      SSL.contextSetALPNProtos ctx ["h2"]
      SSL.contextSetCiphers ctx "HIGH"
      sslCheck <- SSL.contextCheckPrivateKey ctx
      unless sslCheck $ throwM $ ErrorCall "Mock test server SSL private key check failed"
      pure ctx

    mockHttp2TlsServerOnSocket ::
      SSLContext ->
      Socket ->
      IO ()
    mockHttp2TlsServerOnSocket ctx listenSock = do
      NS.listen listenSock 1024
      forever $ do
        (sock, _) <- NS.accept listenSock
        ssl <- SSL.connection ctx sock
        SSL.accept ssl
        let cleanup cfg =
              HTTP2.freeSimpleConfig cfg
                `finally` (SSL.shutdown ssl SSL.Bidirectional `finally` close sock)
        bracket (allocMockServerConfig ssl) cleanup $ \cfg ->
          HTTP2.run defaultServerConfig cfg mockHttp2Handler

    mockHttp2Handler ::
      HTTP2.Request ->
      HTTP2.Aux ->
      (HTTP2.Response -> [HTTP2.PushPromise] -> IO ()) ->
      IO ()
    mockHttp2Handler req _ respond =
      handler req >>= \resp -> respond resp []

    allocMockServerConfig :: SSL.SSL -> IO HTTP2.Config
    allocMockServerConfig ssl = do
      buf <- mallocBytes 4096
      timmgr <- System.TimeManager.initialize $ 30 * 1000000
      let readData :: ByteString -> Int -> IO ByteString
          readData acc 0 = pure acc
          readData acc n = do
            chunk <- SSL.read ssl n `E.catch` \(_ :: SSL.ConnectionAbruptlyTerminated) -> pure mempty
            let chunkLen = BS.length chunk
            if
              | chunkLen == 0 || chunkLen == n ->
                  pure (acc <> chunk)
              | chunkLen > n ->
                  error "openssl: SSL.read returned more bytes than asked for, this is probably a bug"
              | otherwise ->
                  readData (acc <> chunk) (n - chunkLen)

      let sock = fromMaybe (error "Mock test server: SSL without socket") (SSL.sslSocket ssl)
      mysa <- NS.getSocketName sock
      peersa <- NS.getPeerName sock
      pure
        HTTP2.Config
          { HTTP2.confWriteBuffer = buf,
            HTTP2.confBufferSize = 4096,
            HTTP2.confSendAll = SSL.write ssl,
            HTTP2.confReadN = readData mempty,
            HTTP2.confPositionReadMaker = HTTP2.defaultPositionReadMaker,
            HTTP2.confTimeoutManager = timmgr,
            HTTP2.confMySockAddr = mysa,
            HTTP2.confPeerSockAddr = peersa,
            HTTP2.confReadNTimeout = False
          }
