{-# LANGUAGE ConstraintKinds #-}
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
  ( ScheduledJobWorkerSettings (..),
    ScheduledJobsRunnerConfig (..),
    runScheduledJobsMigrations,
    runScheduledJobsRunner,
  )
where

import Arbiter.Core qualified as ArbiterCore
import Arbiter.Core.Job.Types (JobRead, RegistryAdmissionPolicies)
import Arbiter.Core.QueueRegistry (RegistryTables, TableForPayload)
import Arbiter.Migrations qualified as ArbiterMigrations
import Arbiter.Worker qualified as ArbiterWorker
import Arbiter.Worker.Config qualified as ArbiterWorkerConfig
import Arbiter.Worker.Cron qualified as ArbiterWorkerCron
import Control.Exception (bracket, bracket_, throwIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable qualified as Hashable
import Data.Proxy (Proxy (..))
import Data.Secret (SecretText, revealSecretText)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Time.Clock (NominalDiffTime)
import GHC.TypeLits (KnownSymbol)
import Hasql.Connection qualified as HasqlConnection
import Hasql.Connection.Settings qualified as HasqlConnectionSettings
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Hasql.Session qualified as HasqlSession
import Hasql.Statement qualified as HasqlStatement
import Hasql.TH
import Imports
import System.Cron (CronSchedule, serializeCronSchedule)
import System.IO.Error (userError)
import System.Logger qualified as Log
import System.Timeout (timeout)
import UnliftIO.Async qualified as Async
import Wire.API.Jobs
import Wire.JobSubsystem.ArbiterAdapter

-- | Runtime settings shared by every scheduled-job runner in a process.
--
-- These values deliberately mirror the Arbiter worker defaults that we use.
-- Keeping them in one record ensures all scheduled job types use the same
-- execution policy as settings are added or tuned.
data ScheduledJobWorkerSettings = ScheduledJobWorkerSettings
  { scheduledJobWorkerThreads :: Int,
    scheduledJobPollInterval :: NominalDiffTime,
    scheduledJobVisibilityTimeout :: NominalDiffTime,
    scheduledJobHeartbeatInterval :: NominalDiffTime,
    scheduledJobWorkerHeartbeatInterval :: NominalDiffTime,
    scheduledJobBackoffBase :: Double,
    scheduledJobBackoffCap :: NominalDiffTime,
    scheduledJobJitter :: ArbiterWorker.Jitter,
    scheduledJobGracefulShutdownTimeout :: Maybe NominalDiffTime,
    scheduledJobReaperInterval :: NominalDiffTime,
    scheduledJobReaperTimeout :: NominalDiffTime,
    scheduledJobWorkerStaleThreshold :: NominalDiffTime
  }

data ScheduledJobsRunnerConfig registry = ScheduledJobsRunnerConfig
  { scheduledJobsRunnerLogger :: Log.Logger,
    scheduledJobsRunnerSchedule :: CronSchedule,
    -- May contain the PostgreSQL password. Keep it wrapped until the Arbiter boundary.
    scheduledJobsRunnerArbiterConnStr :: SecretText,
    scheduledJobsRunnerSchemaName :: Text,
    scheduledJobsRunnerSettings :: ScheduledJobWorkerSettings
  }

-- | Apply all migrations for the scheduled-jobs registry before constructing
-- any worker pools or accepting scheduled jobs.
runScheduledJobsMigrations :: SecretText -> Text -> IO ()
runScheduledJobsMigrations connStr schemaName =
  withArbiterMigrationLock connStr schemaName $ do
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

-- | Serialize Arbiter schema migrations across all background-worker instances.
--
-- Arbiter's migration API opens its own PostgreSQL connection, so the lock is
-- held on a separate dedicated connection for the entire migration call. This
-- is sufficient because every Wire Server migration caller enters through this
-- wrapper. The connection is kept open until the lock is released; otherwise a
-- session-level advisory lock would be released before the migrations finish.
withArbiterMigrationLock :: SecretText -> Text -> IO a -> IO a
withArbiterMigrationLock connStr schemaName action = do
  bracket acquireConnection HasqlConnection.release $ \lockConnection -> do
    bracket_
      (acquireArbiterMigrationLockWithTimeout lockConnection)
      (runAdvisoryLockStatement lockConnection releaseArbiterMigrationLock)
      action
  where
    lockId :: Int64
    lockId = fromIntegral . Hashable.hash $ ("wire-server:arbiter-migrations:" <> schemaName :: Text)

    acquireArbiterMigrationLockWithTimeout :: HasqlConnection.Connection -> IO ()
    acquireArbiterMigrationLockWithTimeout connection = do
      acquired <- timeout arbiterMigrationLockWaitTimeoutMicros retryUntilAcquired
      case acquired of
        Just () -> pure ()
        Nothing ->
          throwIO . userError $
            "Timed out waiting for the Arbiter migration lock for schema " <> T.unpack schemaName
      where
        retryUntilAcquired :: IO ()
        retryUntilAcquired = do
          acquired <- runAdvisoryLockStatement connection tryArbiterMigrationLock
          if acquired
            then pure ()
            else do
              threadDelay arbiterMigrationLockRetryIntervalMicros
              retryUntilAcquired

        -- Do not let a stuck migration block service startup indefinitely. The
        -- migration itself is not subject to this timeout once the lock is held.
        arbiterMigrationLockRetryIntervalMicros :: Int
        arbiterMigrationLockRetryIntervalMicros = 1_000_000

        arbiterMigrationLockWaitTimeoutMicros :: Int
        arbiterMigrationLockWaitTimeoutMicros = 1 * 60 * 1_000_000

    acquireConnection :: IO HasqlConnection.Connection
    acquireConnection = do
      connectionResult <- HasqlConnection.acquire . HasqlConnectionSettings.connectionString $ revealSecretText connStr
      either
        ( \err ->
            throwIO . userError $
              "Failed to acquire PostgreSQL connection for Arbiter migration lock: " <> show err
        )
        pure
        connectionResult

    runAdvisoryLockStatement ::
      HasqlConnection.Connection ->
      HasqlStatement.Statement Int64 a ->
      IO a
    runAdvisoryLockStatement connection statement = do
      result <- HasqlConnection.use connection (HasqlSession.statement lockId statement)
      either
        ( \err ->
            throwIO . userError $
              "Arbiter migration advisory lock query failed: " <> show err
        )
        pure
        result

    tryArbiterMigrationLock :: HasqlStatement.Statement Int64 Bool
    tryArbiterMigrationLock =
      [singletonStatement|SELECT (pg_try_advisory_lock($1 :: bigint) :: bool)|]

    releaseArbiterMigrationLock :: HasqlStatement.Statement Int64 ()
    releaseArbiterMigrationLock =
      [resultlessStatement|SELECT (1 :: integer) FROM (SELECT pg_advisory_unlock($1 :: bigint))|]

-- | Start the single worker pool for all scheduled jobs.
--
-- Cron jobs and directly inserted one-off jobs use the same Arbiter queue. The
-- payload sum type is dispatched here, while the callbacks retain typed job
-- values at the service boundary.
runScheduledJobsRunner ::
  forall registry.
  ( RegistryTables registry,
    RegistryAdmissionPolicies registry,
    KnownSymbol (TableForPayload ScheduledJobPayload registry),
    FromJSON ScheduledJobPayload,
    ToJSON ScheduledJobPayload
  ) =>
  HasqlPoolExt.Pool ->
  ScheduledJobsRunnerConfig registry ->
  JobWorkerHandlers ->
  IO (IO ())
runScheduledJobsRunner postgresPool runnerConfig JobWorkerHandlers {..} = do
  let arbiterConnStr = Text.encodeUtf8 (revealSecretText runnerConfig.scheduledJobsRunnerArbiterConnStr)
  Log.info runnerConfig.scheduledJobsRunnerLogger $
    Log.msg (Log.val "Starting scheduled jobs worker")
      . Log.field "queue_name" scheduledJobsQueueName
      . Log.field "schedule" (show runnerConfig.scheduledJobsRunnerSchedule)

  let arbiterEnv = mkNewWireArbiterEnv runnerConfig.scheduledJobsRunnerSchemaName postgresPool
      workerHandler _conn job = liftIO $ do
        Log.info runnerConfig.scheduledJobsRunnerLogger $
          Log.msg (Log.val "Running scheduled job")
            . Log.field "queue_name" scheduledJobsQueueName
            . Log.field "payload_type" (scheduledJobPayloadTypeName job.payload)
        case job.payload of
          MeetingsCleanup payload -> scheduledJobsRunMeetingsCleanup payload
          AdminlessDeletion payload -> scheduledJobsRunAdminlessDeletion (mapJobPayload (const payload) job)
          AdminlessReminder payload -> scheduledJobsRunAdminlessReminder (mapJobPayload (const payload) job)

  cronJob <-
    case ArbiterWorkerCron.cronJob
      "meetings-cleanup"
      (serializeCronSchedule runnerConfig.scheduledJobsRunnerSchedule)
      ArbiterWorkerCron.SkipOverlap
      ( \_ scheduledFor ->
          (ArbiterCore.defaultGroupedJob "meetings-cleanup" (MeetingsCleanup MeetingsCleanupJob))
            { ArbiterCore.notVisibleUntil = Just scheduledFor,
              ArbiterCore.maxAttempts = Just 3
            }
      ) of
      Left err -> throwIO . userError $ "Invalid cron schedule for meetings-cleanup: " <> err
      Right job -> pure job

  workerConfig <-
    ( ArbiterWorker.defaultWorkerConfig
        arbiterConnStr
        runnerConfig.scheduledJobsRunnerSettings.scheduledJobWorkerThreads
        workerHandler ::
        IO
          ( ArbiterWorker.WorkerConfig
              (WireArbiter registry)
              ScheduledJobPayload
              ()
          )
    )
  let workerConfig' =
        applyExplicitDefaults
          runnerConfig.scheduledJobsRunnerSettings
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

scheduledJobPayloadTypeName :: ScheduledJobPayload -> Text
scheduledJobPayloadTypeName = \case
  MeetingsCleanup _ -> "meetings_cleanup"
  AdminlessDeletion _ -> "adminless_deletion"
  AdminlessReminder _ -> "adminless_reminder"

mapJobPayload :: (a -> b) -> JobRead a -> JobRead b
mapJobPayload f job =
  ArbiterCore.Job
    { ArbiterCore.primaryKey = job.primaryKey,
      ArbiterCore.payload = f job.payload,
      ArbiterCore.queueName = job.queueName,
      ArbiterCore.groupKey = job.groupKey,
      ArbiterCore.insertedAt = job.insertedAt,
      ArbiterCore.updatedAt = job.updatedAt,
      ArbiterCore.attempts = job.attempts,
      ArbiterCore.lastError = job.lastError,
      ArbiterCore.priority = job.priority,
      ArbiterCore.lastAttemptedAt = job.lastAttemptedAt,
      ArbiterCore.notVisibleUntil = job.notVisibleUntil,
      ArbiterCore.dedupKey = job.dedupKey,
      ArbiterCore.maxAttempts = job.maxAttempts,
      ArbiterCore.parentId = job.parentId,
      ArbiterCore.parentState = job.parentState,
      ArbiterCore.suspended = job.suspended,
      ArbiterCore.claimedBy = job.claimedBy,
      ArbiterCore.admission = job.admission
    }

applyExplicitDefaults ::
  ScheduledJobWorkerSettings ->
  ArbiterWorker.WorkerConfig m payload result ->
  ArbiterWorker.WorkerConfig m payload result
applyExplicitDefaults settings cfg =
  cfg
    { -- How often the dispatcher wakes up to look for newly visible jobs.
      -- Lower values reduce discovery latency at the cost of more DB traffic.
      ArbiterWorkerConfig.pollInterval = settings.scheduledJobPollInterval,
      -- How long a claimed job stays invisible while a worker processes it.
      -- Must exceed the job heartbeat interval so active jobs are not reclaimed.
      ArbiterWorkerConfig.visibilityTimeout = settings.scheduledJobVisibilityTimeout,
      -- How often a running job refreshes its visibility timeout.
      -- Keeps long-running jobs from being reclaimed mid-flight.
      ArbiterWorkerConfig.jobHeartbeatInterval = settings.scheduledJobHeartbeatInterval,
      -- How often the worker process updates its own heartbeat and pause state.
      -- This drives liveness, re-registration, and paused-state reconciliation.
      ArbiterWorkerConfig.workerHeartbeatInterval = settings.scheduledJobWorkerHeartbeatInterval,
      -- Retry strategy for transient worker failures.
      -- Arbiter uses exponential backoff with jitter by default.
      ArbiterWorkerConfig.backoffStrategy =
        ArbiterWorker.exponentialBackoff
          settings.scheduledJobBackoffBase
          settings.scheduledJobBackoffCap,
      -- Jitter mode for retry delays.
      -- Equal jitter smooths retry spikes without making them too aggressive.
      ArbiterWorkerConfig.jitter = settings.scheduledJobJitter,
      -- How long the worker waits for in-flight jobs during shutdown.
      -- If set, the pool exits after this grace period instead of waiting forever.
      ArbiterWorkerConfig.gracefulShutdownTimeout = settings.scheduledJobGracefulShutdownTimeout,
      -- How often the reaper runs. It refreshes groups, sweeps stale workers,
      -- and moves exhausted jobs to the DLQ.
      ArbiterWorkerConfig.reaperInterval = settings.scheduledJobReaperInterval,
      -- Maximum time allowed for one reaper pass.
      ArbiterWorkerConfig.reaperTimeout = settings.scheduledJobReaperTimeout,
      -- How old a worker heartbeat may be before it is considered stale.
      -- Stale workers are swept from the registry by the reaper.
      ArbiterWorkerConfig.workerStaleThreshold = settings.scheduledJobWorkerStaleThreshold
    }
