{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.JobSubsystem.Workers
  ( RecurringJobRunnerConfig (..),
    OneOffJobRunnerConfig (..),
    runScheduledJobsMigrations,
    runRecurringJobRunner,
    runOneOffJobRunner,
  )
where

import Arbiter.Core qualified as ArbiterCore
import Arbiter.Core.Job.Types (JobRead)
import Arbiter.Core.QueueRegistry (RegistryTables, TableForPayload)
import Arbiter.Migrations qualified as ArbiterMigrations
import Arbiter.Worker qualified as ArbiterWorker
import Arbiter.Worker.Config qualified as ArbiterWorkerConfig
import Arbiter.Worker.Cron qualified as ArbiterWorkerCron
import Control.Exception (throwIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Secret (SecretText, revealSecretText)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Time.Clock (NominalDiffTime)
import GHC.TypeLits (KnownSymbol)
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports
import System.Cron (CronSchedule, serializeCronSchedule)
import System.IO.Error (userError)
import System.Logger qualified as Log
import UnliftIO.Async qualified as Async
import Wire.API.Jobs (MeetingsCleanupJob (..), ScheduledJobsRegistry)
import Wire.JobSubsystem.ArbiterAdapter (WireArbiter, WireArbiterEnv (..), runWireArbiter)

data RecurringJobRunnerConfig registry = RecurringJobRunnerConfig
  { recurringJobRunnerLogger :: Log.Logger,
    recurringJobRunnerSchedule :: CronSchedule,
    -- May contain the PostgreSQL password. Keep it wrapped until the Arbiter boundary.
    recurringJobRunnerArbiterConnStr :: SecretText,
    recurringJobRunnerSchemaName :: Text,
    recurringJobRunnerPollInterval :: NominalDiffTime,
    recurringJobRunnerWorkerThreads :: Int,
    recurringJobRunnerJobName :: Text,
    recurringJobRunnerQueueName :: Text
  }

data OneOffJobRunnerConfig registry (payload :: Type) = OneOffJobRunnerConfig
  { oneOffJobRunnerLogger :: Log.Logger,
    -- May contain the PostgreSQL password. Keep it wrapped until the Arbiter boundary.
    oneOffJobRunnerArbiterConnStr :: SecretText,
    oneOffJobRunnerSchemaName :: Text,
    oneOffJobRunnerPollInterval :: NominalDiffTime,
    oneOffJobRunnerWorkerThreads :: Int,
    oneOffJobRunnerJobName :: Text,
    oneOffJobRunnerQueueName :: Text
  }

-- | Apply all migrations for the scheduled-jobs registry before constructing
-- any worker pools or accepting scheduled jobs.
runScheduledJobsMigrations :: SecretText -> Text -> IO ()
runScheduledJobsMigrations connStr schemaName = do
  result <-
    ArbiterMigrations.runMigrationsForRegistry
      (Proxy @ScheduledJobsRegistry)
      (Text.encodeUtf8 $ revealSecretText connStr)
      schemaName
      ArbiterMigrations.defaultMigrationConfig
  case result of
    ArbiterMigrations.MigrationSuccess -> pure ()
    ArbiterMigrations.MigrationError err ->
      throwIO . userError $
        "Arbiter migrations failed for schema " <> T.unpack schemaName <> ": " <> err

-- This runner is specialized to the meetings cleanup payload for now.
-- If we add another cron job later, we can either reuse this helper with a
-- shared payload type or factor out the common Arbiter setup first.
runRecurringJobRunner ::
  forall registry.
  ( RegistryTables registry,
    KnownSymbol (TableForPayload MeetingsCleanupJob registry),
    FromJSON MeetingsCleanupJob,
    ToJSON MeetingsCleanupJob
  ) =>
  HasqlPoolExt.Pool ->
  RecurringJobRunnerConfig registry ->
  (MeetingsCleanupJob -> IO ()) ->
  IO (IO ())
runRecurringJobRunner postgresPool RecurringJobRunnerConfig {..} runJob = do
  let arbiterConnStr = Text.encodeUtf8 (revealSecretText recurringJobRunnerArbiterConnStr)
  Log.info recurringJobRunnerLogger $
    Log.msg (Log.val "Starting scheduled jobs worker")
      . Log.field "job_name" recurringJobRunnerJobName
      . Log.field "queue_name" recurringJobRunnerQueueName
      . Log.field "schedule" (show recurringJobRunnerSchedule)

  let arbiterEnv =
        WireArbiterEnv
          { schemaName = recurringJobRunnerSchemaName,
            connectionPool = postgresPool,
            activeConn = Nothing,
            transactionDepth = 0,
            preparedStatements = False
          }

  let workerHandler _conn job =
        liftIO $ do
          Log.info recurringJobRunnerLogger $
            Log.msg (Log.val "Running scheduled job")
              . Log.field "job_name" recurringJobRunnerJobName
              . Log.field "queue_name" recurringJobRunnerQueueName
          runJob (ArbiterCore.payload job)

      cronJob =
        case ArbiterWorkerCron.cronJob
          recurringJobRunnerJobName
          (serializeCronSchedule recurringJobRunnerSchedule)
          ArbiterWorkerCron.SkipOverlap
          ( \_ scheduledFor ->
              (ArbiterCore.defaultGroupedJob recurringJobRunnerQueueName MeetingsCleanupJob)
                { ArbiterCore.notVisibleUntil = Just scheduledFor,
                  ArbiterCore.maxAttempts = Just 3
                }
          ) of
          Left err -> error $ "Invalid cron schedule for " <> T.unpack recurringJobRunnerJobName <> ": " <> err
          Right job -> job

  workerConfig <-
    ( ArbiterWorker.defaultWorkerConfig
        arbiterConnStr
        recurringJobRunnerWorkerThreads
        workerHandler ::
        IO
          ( ArbiterWorker.WorkerConfig
              (WireArbiter registry)
              MeetingsCleanupJob
              ()
          )
    )
  let workerConfig' =
        applyExplicitDefaults
          recurringJobRunnerPollInterval
          workerConfig
            { ArbiterWorkerConfig.cronJobs = [cronJob]
            }

  workerAsync <-
    Async.async $
      runWireArbiter arbiterEnv $
        ArbiterWorker.runWorkerPool workerConfig'

  pure $ do
    ArbiterWorker.shutdownWorker workerConfig'
    Async.cancel workerAsync

runOneOffJobRunner ::
  forall registry (payload :: Type).
  ( RegistryTables registry,
    KnownSymbol (TableForPayload payload registry),
    FromJSON payload,
    ToJSON payload
  ) =>
  HasqlPoolExt.Pool ->
  OneOffJobRunnerConfig registry payload ->
  (JobRead payload -> IO ()) ->
  IO (IO ())
runOneOffJobRunner postgresPool OneOffJobRunnerConfig {..} runJob = do
  let arbiterConnStr = Text.encodeUtf8 (revealSecretText oneOffJobRunnerArbiterConnStr)
  Log.info oneOffJobRunnerLogger $
    Log.msg (Log.val "Starting one-off jobs worker")
      . Log.field "job_name" oneOffJobRunnerJobName
      . Log.field "queue_name" oneOffJobRunnerQueueName

  let arbiterEnv =
        WireArbiterEnv
          { schemaName = oneOffJobRunnerSchemaName,
            connectionPool = postgresPool,
            activeConn = Nothing,
            transactionDepth = 0,
            preparedStatements = False
          }
  let workerHandler _conn job =
        liftIO $ do
          Log.info oneOffJobRunnerLogger $
            Log.msg (Log.val "Running one-off job")
              . Log.field "job_name" oneOffJobRunnerJobName
              . Log.field "queue_name" oneOffJobRunnerQueueName
          runJob job

  workerConfig <-
    ( ArbiterWorker.defaultWorkerConfig
        arbiterConnStr
        oneOffJobRunnerWorkerThreads
        workerHandler ::
        IO
          ( ArbiterWorker.WorkerConfig
              (WireArbiter registry)
              payload
              ()
          )
    )
  let workerConfig' = applyExplicitDefaults oneOffJobRunnerPollInterval workerConfig

  workerAsync <-
    Async.async $
      runWireArbiter arbiterEnv $
        ArbiterWorker.runWorkerPool workerConfig'

  pure $ do
    ArbiterWorker.shutdownWorker workerConfig'
    Async.cancel workerAsync

-- | Make the effective Arbiter defaults explicit in our code, even when we
-- keep the same values as 'defaultWorkerConfig'.
--
-- This is intentionally redundant with Arbiter's own defaults. The point is to
-- show the runtime behavior in our repository and keep the knobs in one place
-- if we need to tune them later.
applyExplicitDefaults ::
  NominalDiffTime ->
  ArbiterWorker.WorkerConfig m payload result ->
  ArbiterWorker.WorkerConfig m payload result
applyExplicitDefaults pollInterval cfg =
  cfg
    { -- How often the dispatcher wakes up to look for newly visible jobs.
      -- Lower values reduce discovery latency at the cost of more DB traffic.
      ArbiterWorkerConfig.pollInterval = pollInterval,
      -- How long a claimed job stays invisible while a worker processes it.
      -- Must exceed the job heartbeat interval so active jobs are not reclaimed.
      ArbiterWorkerConfig.visibilityTimeout = 60,
      -- How often a running job refreshes its visibility timeout.
      -- Keeps long-running jobs from being reclaimed mid-flight.
      ArbiterWorkerConfig.jobHeartbeatInterval = 30,
      -- How often the worker process updates its own heartbeat and pause state.
      -- This drives liveness, re-registration, and paused-state reconciliation.
      ArbiterWorkerConfig.workerHeartbeatInterval = 10,
      -- Retry strategy for transient worker failures.
      -- Arbiter uses exponential backoff with jitter by default.
      ArbiterWorkerConfig.backoffStrategy = ArbiterWorker.exponentialBackoff 2.0 1_048_576,
      -- Jitter mode for retry delays.
      -- Equal jitter smooths retry spikes without making them too aggressive.
      ArbiterWorkerConfig.jitter = ArbiterWorker.EqualJitter,
      -- How long the worker waits for in-flight jobs during shutdown.
      -- If set, the pool exits after this grace period instead of waiting forever.
      ArbiterWorkerConfig.gracefulShutdownTimeout = Just 30,
      -- How often the reaper runs. It refreshes groups, sweeps stale workers,
      -- and moves exhausted jobs to the DLQ.
      ArbiterWorkerConfig.reaperInterval = 300,
      -- How old a worker heartbeat may be before it is considered stale.
      -- Stale workers are swept from the registry by the reaper.
      ArbiterWorkerConfig.workerStaleThreshold = 300
    }
