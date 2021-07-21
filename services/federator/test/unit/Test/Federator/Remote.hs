{-# LANGUAGE NumericUnderscores #-}

module Test.Federator.Remote where

-- type Effects = '[Embed IO, TinyLog, Polysemy.Reader RunSettings, Polysemy.Reader CertificateStore]
-- mkGrpcClient ::
--   Members '[Embed IO, TinyLog, Polysemy.Reader RunSettings, Polysemy.Reader CertificateStore] r =>
--   SrvTarget ->
-- Sem r (Either RemoteError GrpcClient)

import Data.Streaming.Network (bindRandomPortTCP)
import Federator.Options
import Federator.Remote (mkGrpcClient)
import Federator.Run (mkCAStore)
import Imports
import Network.HTTP.Types (status200)
import qualified Network.TLS.Extra.Cipher as TLS
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
    [ testGroup "mkGrpcClient" [testValidatesCertificateSuccess]
    ]

testValidatesCertificateSuccess :: TestTree
testValidatesCertificateSuccess = testCase "can get response from a server with a valid certificate" $ do
  bracket startMockServer (\(serverThread, _) -> Async.cancel serverThread) $ \(_, port) -> do
    -- threadDelay 1000_000_000
    caStore <- mkCAStore (RunSettings AllowAll (Just False) (Just "test/resources/unit/unit-ca.pem"))
    eitherClient <- Polysemy.runM . TinyLog.discardLogs . Polysemy.runReader caStore $ mkGrpcClient (SrvTarget "localhost" (fromIntegral port))
    case eitherClient of
      Left err -> assertFailure $ "Unexpected error: " <> show err
      Right _ -> pure ()

startMockServer :: MonadIO m => m (Async.Async (), Warp.Port)
startMockServer = liftIO $ do
  (port, sock) <- bindRandomPortTCP "*6"
  serverStarted <- newEmptyMVar
  let settings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setGracefulCloseTimeout2 0 -- Defaults to 2 seconds, causes server stop to take very long
          & Warp.setBeforeMainLoop (putMVar serverStarted ())
      tlsSettings =
        (WarpTLS.tlsSettings "test/resources/unit/localhost.pem" "test/resources/unit/localhost-key.pem")
          { WarpTLS.tlsCiphers =
              [ TLS.cipher_ECDHE_RSA_AES256GCM_SHA384,
                TLS.cipher_ECDHE_RSA_AES256CBC_SHA384,
                TLS.cipher_ECDHE_RSA_AES128GCM_SHA256,
                TLS.cipher_ECDHE_RSA_AES128CBC_SHA256
              ]
          }
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
