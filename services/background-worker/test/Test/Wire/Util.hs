{-# LANGUAGE RecordWildCards #-}

module Test.Wire.Util where

import Control.Concurrent.Chan
import Imports
import Network.HTTP.Client
import System.Logger.Class qualified as Logger
import Util.Options
import Wire.API.Routes.FederationDomainConfig
import Wire.BackgroundWorker.Env hiding (federatorInternal, galley)
import Wire.BackgroundWorker.Env qualified as E
import Wire.BackgroundWorker.Util

testEnv :: IO Env
testEnv = do
  http2Manager <- initHttp2Manager
  logger <- Logger.new Logger.defSettings
  statuses <- newIORef mempty
  backendNotificationMetrics <- mkBackendNotificationMetrics
  httpManager <- newManager defaultManagerSettings
  remoteDomains <- newIORef defFederationDomainConfigs
  remoteDomainsChan <- newChan
  let federatorInternal = Endpoint "localhost" 0
      rabbitmqAdminClient = undefined
      rabbitmqVHost = undefined
      metrics = undefined
      galley = Endpoint "localhost" 8085
      brig = Endpoint "localhost" 8082
      defederationTimeout = responseTimeoutNone
  pure Env {..}

runTestAppT :: AppT IO a -> Int -> IO a
runTestAppT app port = do
  baseEnv <- testEnv
  runTestAppTWithEnv baseEnv app port

runTestAppTWithEnv :: Env -> AppT IO a -> Int -> IO a
runTestAppTWithEnv Env {..} app port = do
  let env = Env {federatorInternal = Endpoint "localhost" (fromIntegral port), ..}
  runAppT env app

data FakeEnvelope = FakeEnvelope
  { rejections :: IORef [Bool],
    acks :: IORef Int
  }

newFakeEnvelope :: IO FakeEnvelope
newFakeEnvelope =
  FakeEnvelope
    <$> newIORef []
    <*> newIORef 0

instance RabbitMQEnvelope FakeEnvelope where
  ack e = atomicModifyIORef' e.acks $ \a -> (a + 1, ())
  reject e requeueFlag = atomicModifyIORef' e.rejections $ \r -> (r <> [requeueFlag], ())
