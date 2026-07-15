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

module Wire.BackgroundWorker.Options where

import Data.Aeson
import Data.Aeson.Types (JSONPathElement (Key), parserThrowError)
import Data.Misc
import Data.Range (Range, unsafeRange)
import GHC.Generics
import Hasql.Pool.Extended
import Imports
import Network.AMQP.Extended
import System.Cron (CronSchedule, parseCronSchedule)
import System.Logger.Extended
import Util.Options
import Wire.Migration
import Wire.PostgresMigrationOpts

data Opts = Opts
  { logLevel :: !Level,
    logFormat :: !(Maybe (Last LogFormat)),
    backgroundWorker :: !Endpoint,
    galley :: Endpoint,
    federatorInternal :: !Endpoint,
    brig :: Endpoint,
    gundeck :: Endpoint,
    spar :: Endpoint,
    rabbitmq :: !RabbitMqOpts,
    -- | Seconds, Nothing for no timeout
    defederationTimeout :: Maybe Int,
    backendNotificationPusher :: BackendNotificationsConfig,
    cassandra :: CassandraOpts,
    cassandraBrig :: CassandraOpts,
    postgresqlPool :: !PoolConfig,
    postgresMigration :: !PostgresMigrationOpts,
    migrateConversations :: !Bool,
    migrationOptions :: !MigrationOptions,
    migrateConversationCodes :: !Bool,
    migrateTeamFeatures :: !Bool,
    migrateDomainRegistration :: !Bool,
    scheduledJobs :: ScheduledJobsConfig,
    meetingsCleanup :: MeetingsCleanupConfig,
    backgroundJobs :: BackgroundJobsConfig
  }
  deriving (Show, Generic)
  deriving (FromJSON) via Generically Opts

data BackendNotificationsConfig = BackendNotificationsConfig
  { -- | Minimum amount of time (in microseconds) to wait before doing the first
    -- retry in pushing a notification. Futher retries are done in a jittered
    -- exponential way.
    -- https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/
    pushBackoffMinWait :: Int,
    -- | Upper limit on amount of time (in microseconds) to wait before retrying
    -- any notification. This exists to ensure that exponential back-off doesn't
    -- cause wait times to be very big.
    pushBackoffMaxWait :: Int,
    -- | The list of remotes is refreshed at an interval. This value in
    -- microseconds decides the interval for polling.
    remotesRefreshInterval :: Int
  }
  deriving (Show, Generic)
  deriving (FromJSON) via Generically BackendNotificationsConfig

newtype RabbitMqOpts = RabbitMqOpts {unRabbitMqOpts :: Either AmqpEndpoint RabbitMqAdminOpts}
  deriving (Show)

instance FromJSON RabbitMqOpts where
  parseJSON v =
    RabbitMqOpts
      <$> ( (Right <$> parseJSON v)
              <|> (Left <$> parseJSON v)
          )

data BackgroundJobsConfig = BackgroundJobsConfig
  { -- | Maximum parallel jobs processed by this process
    concurrency :: Range 1 1000 Int,
    -- | Per-attempt timeout (seconds)
    jobTimeout :: Duration,
    -- | Total attempts including first run
    maxAttempts :: Range 1 1000 Int
  }
  deriving (Show, Generic)
  deriving (FromJSON) via Generically BackgroundJobsConfig

data ScheduledJobsConfig = ScheduledJobsConfig
  { -- | Arbiter dispatcher poll interval for scheduled jobs.
    -- Lower values reduce discovery latency for due jobs.
    pollInterval :: Duration,
    -- | Number of worker threads in each scheduled-job queue.
    workerThreads :: Range 1 1000 Int,
    -- | How long a claimed job remains invisible while it is processed.
    visibilityTimeout :: Duration,
    -- | How often a running job refreshes its visibility timeout.
    jobHeartbeatInterval :: Duration,
    -- | How often a worker refreshes its own heartbeat.
    workerHeartbeatInterval :: Duration,
    -- | Base used by Arbiter's exponential retry backoff.
    backoffBase :: Double,
    -- | Upper bound for Arbiter's exponential retry backoff.
    backoffCap :: Duration,
    -- | Jitter mode used for retry delays.
    jitter :: ScheduledJobsJitter,
    -- | Maximum time to wait for in-flight jobs during shutdown.
    -- 'Nothing' waits indefinitely.
    gracefulShutdownTimeout :: Maybe Duration,
    -- | How often the Arbiter reaper runs.
    reaperInterval :: Duration,
    -- | Maximum time allowed for one reaper pass.
    reaperTimeout :: Duration,
    -- | How old a worker heartbeat may be before it is considered stale.
    workerStaleThreshold :: Duration
  }
  deriving (Show, Generic)

data ScheduledJobsJitter
  = ScheduledJobsNoJitter
  | ScheduledJobsFullJitter
  | ScheduledJobsEqualJitter
  deriving (Eq, Show, Generic)

instance FromJSON ScheduledJobsJitter where
  parseJSON = withText "ScheduledJobsJitter" $ \case
    "none" -> pure ScheduledJobsNoJitter
    "full" -> pure ScheduledJobsFullJitter
    "equal" -> pure ScheduledJobsEqualJitter
    _ -> fail "expected one of: none, full, equal"

instance FromJSON ScheduledJobsConfig where
  parseJSON =
    withObject "ScheduledJobsConfig" $ \o -> do
      pollInterval <- o .:? "pollInterval" .!= unsafeParseDuration "5s"
      workerThreads <- o .:? "workerThreads" .!= unsafeRange 1
      visibilityTimeout <- o .:? "visibilityTimeout" .!= unsafeParseDuration "60s"
      jobHeartbeatInterval <- o .:? "jobHeartbeatInterval" .!= unsafeParseDuration "30s"
      workerHeartbeatInterval <- o .:? "workerHeartbeatInterval" .!= unsafeParseDuration "10s"
      backoffBase <- o .:? "backoffBase" .!= 2.0
      backoffCap <- o .:? "backoffCap" .!= unsafeParseDuration "1048576s"
      jitter <- o .:? "jitter" .!= ScheduledJobsEqualJitter
      gracefulShutdownTimeout <-
        o .:? "gracefulShutdownTimeout" .!= Just (unsafeParseDuration "30s")
      reaperInterval <- o .:? "reaperInterval" .!= unsafeParseDuration "300s"
      reaperTimeout <- o .:? "reaperTimeout" .!= unsafeParseDuration "300s"
      workerStaleThreshold <- o .:? "workerStaleThreshold" .!= unsafeParseDuration "300s"
      let validatePositive key value =
            when (duration value <= 0) $
              parserThrowError [Key key] $
                show key <> " must be greater than 0, got: " <> show value
      validatePositive "pollInterval" pollInterval
      validatePositive "visibilityTimeout" visibilityTimeout
      validatePositive "jobHeartbeatInterval" jobHeartbeatInterval
      validatePositive "workerHeartbeatInterval" workerHeartbeatInterval
      validatePositive "backoffCap" backoffCap
      validatePositive "reaperInterval" reaperInterval
      validatePositive "reaperTimeout" reaperTimeout
      validatePositive "workerStaleThreshold" workerStaleThreshold
      for_ gracefulShutdownTimeout $ validatePositive "gracefulShutdownTimeout"
      when (backoffBase <= 0) $
        parserThrowError [Key "backoffBase"] $
          "backoffBase must be greater than 0, got: " <> show backoffBase
      pure ScheduledJobsConfig {..}

data MeetingsCleanupConfig = MeetingsCleanupConfig
  { -- | Delete meetings older than this many hours
    cleanOlderThanHours :: Double,
    -- | Maximum number of meetings to delete per batch
    batchSize :: Int,
    -- | Cron schedule for the cleanup job
    schedule :: CronSchedule
  }
  deriving (Show, Generic)

instance FromJSON MeetingsCleanupConfig where
  parseJSON =
    withObject "MeetingsCleanupConfig" $ \o -> do
      cleanOlderThanHours <- o .: "cleanOlderThanHours"
      when (cleanOlderThanHours < 0) $
        parserThrowError [Key "cleanOlderThanHours"] $
          "cleanOlderThanHours must be non-negative, got: " <> show cleanOlderThanHours
      batchSize <- o .: "batchSize"
      when (batchSize <= 0) $
        parserThrowError [Key "batchSize"] $
          "batchSize must be greater than 0, got: " <> show batchSize
      scheduleRaw <- o .: "schedule"
      schedule <-
        case parseCronSchedule scheduleRaw of
          Left e -> parserThrowError [Key "schedule"] $ "Cannot parse cronjob syntax: " <> e
          Right x -> pure x
      pure $ MeetingsCleanupConfig {..}
