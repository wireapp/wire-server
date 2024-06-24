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

module Test.Federator.Remote where

import Control.Exception (bracket)
import Control.Monad.Codensity
import Data.Domain
import Data.Id
import Federator.Discovery
import Federator.Env (mkHttp2Manager)
import Federator.Options
import Federator.Remote
import Federator.Run (mkTLSSettingsOrThrow)
import Imports
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as Warp
import Network.Wai.Utilities.MockServer (startMockServer)
import OpenSSL.Session (SSLContext)
import Polysemy
import Polysemy.Embed
import Polysemy.Error
import Polysemy.Input
import Test.Federator.Options (defRunSettings)
import Test.Federator.Util
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Federation.Component
import Wire.API.Federation.Error
import Wire.Network.DNS.SRV (SrvTarget (SrvTarget))

tests :: TestTree
tests =
  testGroup
    "Federator.Remote"
    [ testValidatesCertificateSuccess,
      testValidatesCertificateWrongHostname,
      testConnectionError
    ]

settings :: RunSettings
settings =
  ( defRunSettings
      "test/resources/unit/localhost.pem"
      "test/resources/unit/localhost-key.pem"
  )
    { useSystemCAStore = False,
      remoteCAStore = Just "test/resources/unit/unit-ca.pem"
    }

discoverLocalhost :: ByteString -> Int -> Sem (DiscoverFederator ': r) a -> Sem r a
discoverLocalhost hostname port = interpret $ \case
  DiscoverAllFederators (Domain "localhost") ->
    pure (Right (pure (SrvTarget hostname (fromIntegral port))))
  DiscoverAllFederators _ -> pure (Left (DiscoveryFailureSrvNotAvailable "only localhost is supported"))
  DiscoverFederator (Domain "localhost") ->
    pure (Right (SrvTarget hostname (fromIntegral port)))
  DiscoverFederator _ -> pure (Left (DiscoveryFailureSrvNotAvailable "only localhost is supported"))

assertNoRemoteError :: Either RemoteError x -> IO x
assertNoRemoteError = \case
  Left err -> assertFailure $ "Unexpected remote error: " <> show err
  Right x -> pure x

mkTestCall :: SSLContext -> ByteString -> Int -> Codensity IO (Either RemoteError ())
mkTestCall sslCtx hostname port = do
  mgr <- liftIO $ mkHttp2Manager 1_000_000 sslCtx
  runM
    . runEmbedded @IO @(Codensity IO) liftIO
    . runError @RemoteError
    . void
    . runInputConst mgr
    . runInputConst (RequestId "test")
    . discoverLocalhost hostname port
    . assertNoError @DiscoveryFailure
    . interpretRemote
    $ discoverAndCall (Domain "localhost") Brig "test" [] mempty

withMockServer :: Warp.TLSSettings -> (Warp.Port -> IO a) -> IO a
withMockServer tls k =
  bracket
    (startMockServer (Just tls) app)
    fst
    (k . snd)
  where
    app _req respond = respond $ responseLBS status200 [] "mock body"

testValidatesCertificateSuccess :: TestTree
testValidatesCertificateSuccess =
  testGroup
    "can get response with valid certificate"
    [ testCase "when hostname=localhost and certificate-for=localhost" $
        withMockServer certForLocalhost $ \port -> do
          tlsSettings <- mkTLSSettingsOrThrow settings
          runCodensity (mkTestCall tlsSettings "localhost" port) assertNoRemoteError,
      testCase "when hostname=localhost. and certificate-for=localhost" $
        withMockServer certForLocalhost $ \port -> do
          tlsSettings <- mkTLSSettingsOrThrow settings
          res <- readFile "/etc/resolv.conf"
          putStrLn res
          runCodensity (mkTestCall tlsSettings "localhost." port) assertNoRemoteError,
      -- It is not very clear how to handle this, this test just exists to
      -- document what we do.
      -- Some discussion from author of curl:
      -- https://lists.w3.org/Archives/Public/ietf-http-wg/2016JanMar/0430.html
      --
      -- Perhaps it is also not possible to get a publically verifiable
      -- certificate like this from any of the CAs:
      -- https://github.com/certbot/certbot/issues/3718
      testCase "when hostname=localhost. and certificate-for=localhost." $
        withMockServer certForLocalhostDot $ \port -> do
          tlsSettings <- mkTLSSettingsOrThrow settings
          runCodensity (mkTestCall tlsSettings "localhost." port) $ \case
            Left _ -> pure ()
            Right _ -> assertFailure "Congratulations, you fixed a known issue!"
    ]

--
-- This is a group of test cases where refusing to connect with the server is
-- checked. The second test case refuses to connect with a server when the
-- certificate's X509v3 Extended Key Usage extension is present and it does not
-- list "TLS Web Server Authentication" among the purposes.
testValidatesCertificateWrongHostname :: TestTree
testValidatesCertificateWrongHostname =
  testGroup
    "testValidatesCertificateWrongHostname - refuses to connect with server"
    [ testCase "when the server's certificate doesn't match the hostname" $
        withMockServer certForWrongDomain $ \port -> do
          tlsSettings <- mkTLSSettingsOrThrow settings
          runCodensity (mkTestCall tlsSettings "localhost" port) $ \case
            Left (RemoteError _ _ (FederatorClientTLSException _)) -> pure ()
            Left x -> assertFailure $ "Expected TLS failure, got: " <> show x
            Right _ -> assertFailure "Expected connection with the server to fail",
      testCase "when the server's certificate does not have the server key usage flag" $
        withMockServer certWithoutServerKeyUsage $ \port -> do
          tlsSettings <- mkTLSSettingsOrThrow settings
          runCodensity (mkTestCall tlsSettings "localhost" port) $ \case
            Left (RemoteError _ _ (FederatorClientTLSException _)) -> pure ()
            Left x -> assertFailure $ "Expected TLS failure, got: " <> show x
            Right _ -> assertFailure "Expected connection with the server to fail"
    ]

testConnectionError :: TestTree
testConnectionError = testCase "connection failures are reported correctly" $ do
  tlsSettings <- mkTLSSettingsOrThrow settings
  runCodensity (mkTestCall tlsSettings "localhost" 1) $ \case
    Left (RemoteError _ _ (FederatorClientConnectionError _)) -> pure ()
    Left x -> assertFailure $ "Expected connection error, got: " <> show x
    Right _ -> assertFailure "Expected connection with the server to fail"

certForLocalhost :: Warp.TLSSettings
certForLocalhost = Warp.tlsSettings "test/resources/unit/localhost.pem" "test/resources/unit/localhost-key.pem"

certForLocalhostDot :: Warp.TLSSettings
certForLocalhostDot = Warp.tlsSettings "test/resources/unit/localhost-dot.pem" "test/resources/unit/localhost-dot-key.pem"

certForWrongDomain :: Warp.TLSSettings
certForWrongDomain = Warp.tlsSettings "test/resources/unit/localhost.example.com.pem" "test/resources/unit/localhost.example.com-key.pem"

certWithoutServerKeyUsage :: Warp.TLSSettings
certWithoutServerKeyUsage =
  Warp.tlsSettings
    "test/resources/unit/localhost.client-only.pem"
    "test/resources/unit/localhost.client-only-key.pem"
