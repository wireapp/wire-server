{-# LANGUAGE NumericUnderscores #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

import Data.Streaming.Network (bindRandomPortTCP)
import Federator.Options
import Federator.Remote
import Federator.Run (mkTLSSettingsOrThrow)
import Imports
import Network.HTTP.Types (status200)
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import Polysemy
import qualified Polysemy.Error as Polysemy
import qualified Polysemy.Input as Polysemy
import Test.Federator.Options (defRunSettings)
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO (bracket, timeout)
import qualified UnliftIO.Async as Async
import Wire.Network.DNS.SRV (SrvTarget (SrvTarget))

tests :: TestTree
tests =
  testGroup
    "Federator.Remote"
    [ testGroup
        "mkGrpcClient"
        [ testValidatesCertificateSuccess,
          testValidatesCertificateWrongHostname
        ]
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

assertNoError ::
  forall e r x.
  (Show e, Member (Embed IO) r) =>
  Sem (Polysemy.Error e ': r) x ->
  Sem r x
assertNoError action =
  Polysemy.runError action >>= \case
    Left err -> embed . assertFailure $ "Unexpected error: " <> show err
    Right x -> pure x

testValidatesCertificateSuccess :: TestTree
testValidatesCertificateSuccess =
  testGroup
    "can get response with valid certificate"
    [ testCase "when hostname=localhost and certificate-for=localhost" $ do
        bracket (startMockServer certForLocalhost) (\(serverThread, _) -> Async.cancel serverThread) $ \(_, port) -> do
          tlsSettings <- mkTLSSettingsOrThrow settings
          void . Polysemy.runM . assertNoError @RemoteError . Polysemy.runInputConst tlsSettings $ mkGrpcClient (SrvTarget "localhost" (fromIntegral port)),
      testCase "when hostname=localhost. and certificate-for=localhost" $ do
        bracket (startMockServer certForLocalhost) (\(serverThread, _) -> Async.cancel serverThread) $ \(_, port) -> do
          tlsSettings <- mkTLSSettingsOrThrow settings
          void . Polysemy.runM . assertNoError @RemoteError . Polysemy.runInputConst tlsSettings $ mkGrpcClient (SrvTarget "localhost." (fromIntegral port)),
      -- This is a limitation of the TLS library, this test just exists to document that.
      testCase "when hostname=localhost. and certificate-for=localhost." $ do
        bracket (startMockServer certForLocalhostDot) (\(serverThread, _) -> Async.cancel serverThread) $ \(_, port) -> do
          tlsSettings <- mkTLSSettingsOrThrow settings
          eitherClient <-
            Polysemy.runM
              . Polysemy.runError @RemoteError
              . Polysemy.runInputConst tlsSettings
              $ mkGrpcClient (SrvTarget "localhost." (fromIntegral port))
          case eitherClient of
            Left _ -> pure ()
            Right _ -> assertFailure "Congratulations, you fixed a known issue!"
    ]

testValidatesCertificateWrongHostname :: TestTree
testValidatesCertificateWrongHostname =
  testGroup
    "refuses to connect with server"
    [ testCase "when the server's certificate doesn't match the hostname" $
        bracket (startMockServer certForWrongDomain) (Async.cancel . fst) $ \(_, port) -> do
          tlsSettings <- mkTLSSettingsOrThrow settings
          eitherClient <-
            Polysemy.runM
              . Polysemy.runError
              . Polysemy.runInputConst tlsSettings
              $ mkGrpcClient (SrvTarget "localhost." (fromIntegral port))
          case eitherClient of
            Left (RemoteErrorTLSException _ _) -> pure ()
            Left x -> assertFailure $ "Expected TLS failure, got: " <> show x
            Right _ -> assertFailure "Expected connection with the server to fail",
      testCase "when the server's certificate does not have the server key usage flag" $
        bracket (startMockServer certWithoutServerKeyUsage) (Async.cancel . fst) $ \(_, port) -> do
          tlsSettings <- mkTLSSettingsOrThrow settings
          eitherClient <-
            Polysemy.runM
              . Polysemy.runError
              . Polysemy.runInputConst tlsSettings
              $ mkGrpcClient (SrvTarget "localhost." (fromIntegral port))
          case eitherClient of
            Left (RemoteErrorTLSException _ _) -> pure ()
            Left x -> assertFailure $ "Expected TLS failure, got: " <> show x
            Right _ -> assertFailure "Expected connection with the server to fail"
    ]

certForLocalhost :: WarpTLS.TLSSettings
certForLocalhost = WarpTLS.tlsSettings "test/resources/unit/localhost.pem" "test/resources/unit/localhost-key.pem"

certForLocalhostDot :: WarpTLS.TLSSettings
certForLocalhostDot = WarpTLS.tlsSettings "test/resources/unit/localhost-dot.pem" "test/resources/unit/localhost-dot-key.pem"

certForWrongDomain :: WarpTLS.TLSSettings
certForWrongDomain = WarpTLS.tlsSettings "test/resources/unit/localhost.example.com.pem" "test/resources/unit/localhost.example.com-key.pem"

certWithoutServerKeyUsage :: WarpTLS.TLSSettings
certWithoutServerKeyUsage =
  WarpTLS.tlsSettings
    "test/resources/unit/localhost.client-only.pem"
    "test/resources/unit/localhost.client-only-key.pem"

startMockServer :: MonadIO m => WarpTLS.TLSSettings -> m (Async.Async (), Warp.Port)
startMockServer tlsSettings = liftIO $ do
  (port, sock) <- bindRandomPortTCP "*6"
  serverStarted <- newEmptyMVar
  let wsettings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setGracefulCloseTimeout2 0 -- Defaults to 2 seconds, causes server stop to take very long
          & Warp.setBeforeMainLoop (putMVar serverStarted ())
      app _req respond = respond $ responseLBS status200 [] "dragons be here"

  serverThread <- Async.async $ WarpTLS.runTLSSocket tlsSettings wsettings sock app
  serverStartedSignal <- timeout 10_000_000 (readMVar serverStarted)
  case serverStartedSignal of
    Nothing -> do
      maybeException <- Async.poll serverThread
      case maybeException of
        Just (Left err) -> assertFailure $ "mock server errored while starting: \n" <> show err
        _ -> pure ()
      liftIO $ Async.cancel serverThread
      assertFailure $ "Failed to start the mock server within 10 seconds on port: " <> show port
    _ -> do
      pure (serverThread, port)
