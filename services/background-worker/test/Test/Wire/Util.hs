{-# LANGUAGE RecordWildCards #-}

module Test.Wire.Util where

import Data.Domain (Domain (Domain))
import Data.Proxy
import Data.Range
import Imports
import Network.HTTP.Client hiding (Proxy)
import System.Logger.Class qualified as Logger
import Util.Options (Endpoint (..))
import Wire.BackgroundWorker.Env hiding (federatorInternal)
import Wire.BackgroundWorker.Env qualified as E
import Wire.BackgroundWorker.Options
import Wire.ConversationStore

testEnv :: IO Env
testEnv = do
  http2Manager <- initHttp2Manager
  logger <- Logger.new Logger.defSettings
  let cassandra = undefined
      cassandraGalley = undefined
      cassandraBrig = undefined
      postgresMigration = PostgresMigrationOpts CassandraStorage
  statuses <- newIORef mempty
  backendNotificationMetrics <- mkBackendNotificationMetrics
  workerRunningGauge <- mkWorkerRunningGauge
  httpManager <- newManager defaultManagerSettings
  let federatorInternal = Endpoint "localhost" 0
      rabbitmqAdminClient = undefined
      rabbitmqVHost = undefined
      defederationTimeout = responseTimeoutNone
      backendNotificationsConfig = BackendNotificationsConfig 1000 500000 1000
      backgroundJobsConfig =
        BackgroundJobsConfig
          { concurrency = toRange (Proxy @1),
            jobTimeout = toRange (Proxy @100),
            maxAttempts = toRange (Proxy @3)
          }
      hasqlPool = undefined
      amqpJobsPublisherChannel = undefined
      amqpBackendNotificationsChannel = undefined
      domain = Domain "local"
      gundeckEndpoint = undefined
      brigEndpoint = undefined
  pure Env {..}

runTestAppT :: AppT IO a -> Int -> IO a
runTestAppT app port = do
  baseEnv <- testEnv
  runTestAppTWithEnv baseEnv app port

runTestAppTWithEnv :: Env -> AppT IO a -> Int -> IO a
runTestAppTWithEnv Env {..} app port = do
  let env = Env {federatorInternal = Endpoint "localhost" (fromIntegral port), ..}
  runAppT env app
