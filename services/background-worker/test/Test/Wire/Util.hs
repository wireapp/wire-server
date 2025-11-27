{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.Util where

import Data.Domain (Domain (Domain))
import Data.Misc
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
            jobTimeout = Duration 100,
            maxAttempts = toRange (Proxy @3)
          }
      hasqlPool = undefined
      amqpJobsPublisherChannel = undefined
      amqpBackendNotificationsChannel = undefined
      federationDomain = Domain "local"
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
