{-# LANGUAGE NumericUnderscores #-}

module Test.Federator.Remote where

-- type Effects = '[Embed IO, TinyLog, Polysemy.Reader RunSettings, Polysemy.Reader CertificateStore]
-- mkGrpcClient ::
--   Members '[Embed IO, TinyLog, Polysemy.Reader RunSettings, Polysemy.Reader CertificateStore] r =>
--   SrvTarget ->
-- Sem r (Either RemoteError GrpcClient)

import Data.Streaming.Network (bindRandomPortTCP)
import Federator.Options
import Federator.Remote
import Federator.Run (mkCAStore)
import Imports
import Network.HTTP.Types (status200)
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import qualified Polysemy
import qualified Polysemy.Reader as Polysemy
import qualified Polysemy.TinyLog as TinyLog
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

testValidatesCertificateSuccess :: TestTree
testValidatesCertificateSuccess =
  testGroup
    "can get response with valid certificate"
    [ testCase "when hostname=localhost and certificate-for=localhost" $ do
        bracket (startMockServer certForLocalhost) (\(serverThread, _) -> Async.cancel serverThread) $ \(_, port) -> do
          caStore <- mkCAStore (RunSettings AllowAll (Just False) (Just "test/resources/unit/unit-ca.pem"))
          eitherClient <- Polysemy.runM . TinyLog.discardLogs . Polysemy.runReader caStore $ mkGrpcClient (SrvTarget "localhost" (fromIntegral port))
          case eitherClient of
            Left err -> assertFailure $ "Unexpected error: " <> show err
            Right _ -> pure (),
      testCase "when hostname=localhost. and certificate-for=localhost" $ do
        bracket (startMockServer certForLocalhost) (\(serverThread, _) -> Async.cancel serverThread) $ \(_, port) -> do
          caStore <- mkCAStore (RunSettings AllowAll (Just False) (Just "test/resources/unit/unit-ca.pem"))
          eitherClient <- Polysemy.runM . TinyLog.discardLogs . Polysemy.runReader caStore $ mkGrpcClient (SrvTarget "localhost." (fromIntegral port))
          case eitherClient of
            Left err -> assertFailure $ "Unexpected error: " <> show err
            Right _ -> pure (),
      -- This is a limitation of the TLS library, this test just exists to document that.
      testCase "when hostname=localhost. and certificate-for=localhost." $ do
        bracket (startMockServer certForLocalhostDot) (\(serverThread, _) -> Async.cancel serverThread) $ \(_, port) -> do
          caStore <- mkCAStore (RunSettings AllowAll (Just False) (Just "test/resources/unit/unit-ca.pem"))
          eitherClient <-
            Polysemy.runM . TinyLog.discardLogs . Polysemy.runReader caStore $
              mkGrpcClient (SrvTarget "localhost." (fromIntegral port))
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
          caStore <- mkCAStore (RunSettings AllowAll (Just False) (Just "test/resources/unit/unit-ca.pem"))
          eitherClient <-
            Polysemy.runM . TinyLog.discardLogs . Polysemy.runReader caStore $
              mkGrpcClient (SrvTarget "localhost." (fromIntegral port))
          case eitherClient of
            Left (RemoteErrorTLSException _) -> pure ()
            Left x -> assertFailure $ "Expected TLS failure, got: " <> show x
            Right _ -> assertFailure "Expected connection with the server to fail"
    ]

certForLocalhost :: WarpTLS.TLSSettings
certForLocalhost = WarpTLS.tlsSettings "test/resources/unit/localhost.pem" "test/resources/unit/localhost-key.pem"

certForLocalhostDot :: WarpTLS.TLSSettings
certForLocalhostDot = WarpTLS.tlsSettings "test/resources/unit/localhost-dot.pem" "test/resources/unit/localhost-dot-key.pem"

certForWrongDomain :: WarpTLS.TLSSettings
certForWrongDomain = WarpTLS.tlsSettings "test/resources/unit/localhost.example.com.pem" "test/resources/unit/localhost.example.com-key.pem"

startMockServer :: MonadIO m => WarpTLS.TLSSettings -> m (Async.Async (), Warp.Port)
startMockServer tlsSettings = liftIO $ do
  (port, sock) <- bindRandomPortTCP "*6"
  serverStarted <- newEmptyMVar
  let settings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setGracefulCloseTimeout2 0 -- Defaults to 2 seconds, causes server stop to take very long
          & Warp.setBeforeMainLoop (putMVar serverStarted ())
      app _req respond = respond $ responseLBS status200 [] "dragons be here"

  serverThread <- Async.async $ WarpTLS.runTLSSocket tlsSettings settings sock app
  serverStartedSignal <- timeout 10_000_000 (takeMVar serverStarted)
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
