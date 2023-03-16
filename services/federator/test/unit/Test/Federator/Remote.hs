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
import Federator.Discovery
import Federator.Env (TLSSettings)
import Federator.Options
import Federator.Remote
import Federator.Run (mkTLSSettingsOrThrow)
import Imports
import Network.HTTP.Types (status200)
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import Network.Wai.Utilities.MockServer (startMockServer)
import Polysemy
import Polysemy.Embed
import Polysemy.Error
import Polysemy.Input
import Test.Federator.Options (defRunSettings)
import Test.Federator.Util
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Pending (flakyTestCase)
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

discoverLocalhost :: Int -> Sem (DiscoverFederator ': r) a -> Sem r a
discoverLocalhost port = interpret $ \case
  DiscoverAllFederators (Domain "localhost") ->
    pure (Right (pure (SrvTarget "localhost" (fromIntegral port))))
  DiscoverAllFederators _ -> pure (Left (DiscoveryFailureSrvNotAvailable "only localhost is supported"))
  DiscoverFederator (Domain "localhost") ->
    pure (Right (SrvTarget "localhost" (fromIntegral port)))
  DiscoverFederator _ -> pure (Left (DiscoveryFailureSrvNotAvailable "only localhost is supported"))

assertNoRemoteError :: IO (Either RemoteError x) -> IO x
assertNoRemoteError action =
  action >>= \case
    Left err -> assertFailure $ "Unexpected remote error: " <> show err
    Right x -> pure x

mkTestCall :: TLSSettings -> Int -> IO (Either RemoteError ())
mkTestCall tlsSettings port =
  runM
    . runError @RemoteError
    . void
    . runInputConst tlsSettings
    . discoverLocalhost port
    . assertNoError @DiscoveryFailure
    . runEmbedded @(Codensity IO) @IO lowerCodensity
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
    [ flakyTestCase "when hostname=localhost and certificate-for=localhost" $
        withMockServer certForLocalhost $ \port -> do
          tlsSettings <- mkTLSSettingsOrThrow settings
          assertNoRemoteError (mkTestCall tlsSettings port),
      flakyTestCase "when hostname=localhost. and certificate-for=localhost" $
        withMockServer certForLocalhost $ \port -> do
          tlsSettings <- mkTLSSettingsOrThrow settings
          assertNoRemoteError (mkTestCall tlsSettings port),
      -- This is a limitation of the TLS library, this test just exists to document that.
      testCase "when hostname=localhost. and certificate-for=localhost." $
        withMockServer certForLocalhostDot $ \port -> do
          tlsSettings <- mkTLSSettingsOrThrow settings
          eitherClient <- mkTestCall tlsSettings port
          case eitherClient of
            Left _ -> pure ()
            Right _ -> assertFailure "Congratulations, you fixed a known issue!"
    ]

-- @SF.Federation @TSFI.Federate @TSFI.DNS @S2
--
-- This is a group of test cases where refusing to connect with the server is
-- checked. The second test case refuses to connect with a server when the
-- certificate's X509v3 Extended Key Usage extension is present and it does not
-- list "TLS Web Server Authentication" among the purposes.
testValidatesCertificateWrongHostname :: TestTree
testValidatesCertificateWrongHostname =
  testGroup
    "refuses to connect with server"
    [ testCase "when the server's certificate doesn't match the hostname" $
        withMockServer certForWrongDomain $ \port -> do
          tlsSettings <- mkTLSSettingsOrThrow settings
          eitherClient <- mkTestCall tlsSettings port
          case eitherClient of
            Left (RemoteError _ (FederatorClientTLSException _)) -> pure ()
            Left x -> assertFailure $ "Expected TLS failure, got: " <> show x
            Right _ -> assertFailure "Expected connection with the server to fail",
      testCase "when the server's certificate does not have the server key usage flag" $
        withMockServer certWithoutServerKeyUsage $ \port -> do
          tlsSettings <- mkTLSSettingsOrThrow settings
          eitherClient <- mkTestCall tlsSettings port
          case eitherClient of
            Left (RemoteError _ (FederatorClientTLSException _)) -> pure ()
            Left x -> assertFailure $ "Expected TLS failure, got: " <> show x
            Right _ -> assertFailure "Expected connection with the server to fail"
    ]

-- @END

testConnectionError :: TestTree
testConnectionError = testCase "connection failures are reported correctly" $ do
  tlsSettings <- mkTLSSettingsOrThrow settings
  result <- mkTestCall tlsSettings 1
  case result of
    Left (RemoteError _ (FederatorClientConnectionError _)) -> pure ()
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
