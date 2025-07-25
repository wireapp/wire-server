{-# LANGUAGE RecordWildCards #-}

module Test.Wire.Util where

import Imports
import Network.HTTP.Client
import System.Logger.Class qualified as Logger
import Util.Options (Endpoint (..))
import Wire.BackgroundWorker.Env hiding (federatorInternal)
import Wire.BackgroundWorker.Env qualified as E
import Wire.BackgroundWorker.Options

testEnv :: IO Env
testEnv = do
  http2Manager <- initHttp2Manager
  logger <- Logger.new Logger.defSettings
  let cassandra = undefined
  statuses <- newIORef mempty
  backendNotificationMetrics <- mkBackendNotificationMetrics
  workerRunningGauge <- mkWorkerRunningGauge
  httpManager <- newManager defaultManagerSettings
  let federatorInternal = Endpoint "localhost" 0
      rabbitmqAdminClient = undefined
      rabbitmqVHost = undefined
      defederationTimeout = responseTimeoutNone
      backendNotificationsConfig = BackendNotificationsConfig 1000 500000 1000
  pure Env {..}

runTestAppT :: AppT IO a -> Int -> IO a
runTestAppT app port = do
  baseEnv <- testEnv
  runTestAppTWithEnv baseEnv app port

runTestAppTWithEnv :: Env -> AppT IO a -> Int -> IO a
runTestAppTWithEnv Env {..} app port = do
  let env = Env {federatorInternal = Endpoint "localhost" (fromIntegral port), ..}
  runAppT env app
