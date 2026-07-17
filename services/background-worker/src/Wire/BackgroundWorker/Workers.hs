{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

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

module Wire.BackgroundWorker.Workers (startWorker) where

import Arbiter.Core qualified as ArbiterCore
import Arbiter.Core.Job.Types (JobRead, RegistryAdmissionPolicies)
import Arbiter.Core.QueueRegistry (RegistryTables, TableForPayload)
import Arbiter.Worker qualified as ArbiterWorker
import Arbiter.Worker.Config qualified as ArbiterWorkerConfig
import Arbiter.Worker.Cron qualified as ArbiterWorkerCron
import Control.Exception (throwIO)
import Data.Misc (Duration, duration)
import Data.Proxy (Proxy (..))
import Data.Range (fromRange)
import Data.Secret (SecretText, revealSecretText)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Time.Clock (NominalDiffTime)
import GHC.TypeLits (KnownSymbol)
import Imports
import System.Cron (CronSchedule, serializeCronSchedule)
import System.IO.Error (userError)
import System.Logger qualified as Log
import UnliftIO.Async qualified as Async
import Wire.API.Jobs
import Wire.AdminlessJobsWorker (runAdminlessDeletionJob, runAdminlessReminderJob)
import Wire.BackgroundWorker.Env (AppT, Env (..), runAppT)
import Wire.BackgroundWorker.Options (MeetingsCleanupConfig (..), ScheduledJobsConfig (..), ScheduledJobsJitter (..))
import Wire.BackgroundWorker.Util
import Wire.ExternalAccess.External
import Wire.JobSubsystem.ArbiterAdapter
import Wire.JobSubsystem.Migrations (runScheduledJobsMigrations)
import Wire.MeetingsCleanupWorker

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

startWorker :: ScheduledJobsConfig -> MeetingsCleanupConfig -> AppT IO CleanupAction
startWorker scheduledConfig meetingsCleanupConfig = do
  env <- ask
  extEnv <- liftIO $ initExtEnv True
  let cleanupConfig =
        CleanupConfig
          { retentionHours = meetingsCleanupConfig.cleanOlderThanHours,
            batchSize = meetingsCleanupConfig.batchSize
          }
      workerSettings =
        ScheduledJobWorkerSettings
          { scheduledJobWorkerThreads = fromRange scheduledConfig.workerThreads,
            scheduledJobPollInterval = scheduledJobsDuration scheduledConfig.pollInterval,
            scheduledJobVisibilityTimeout = scheduledJobsDuration scheduledConfig.visibilityTimeout,
            scheduledJobHeartbeatInterval = scheduledJobsDuration scheduledConfig.jobHeartbeatInterval,
            scheduledJobWorkerHeartbeatInterval = scheduledJobsDuration scheduledConfig.workerHeartbeatInterval,
            scheduledJobBackoffBase = scheduledConfig.backoffBase,
            scheduledJobBackoffCap = scheduledJobsDuration scheduledConfig.backoffCap,
            scheduledJobJitter = scheduledJobsJitter scheduledConfig.jitter,
            scheduledJobGracefulShutdownTimeout = fmap scheduledJobsDuration scheduledConfig.gracefulShutdownTimeout,
            scheduledJobReaperInterval = scheduledJobsDuration scheduledConfig.reaperInterval,
            scheduledJobReaperTimeout = scheduledJobsDuration scheduledConfig.reaperTimeout,
            scheduledJobWorkerStaleThreshold = scheduledJobsDuration scheduledConfig.workerStaleThreshold
          }
      workersConfig =
        ScheduledJobsRunnerConfig
          { scheduledJobsRunnerLogger = env.logger,
            scheduledJobsRunnerSchedule = meetingsCleanupConfig.schedule,
            -- Arbiter still uses the connection string for LISTEN/NOTIFY.
            -- The actual job DB access goes through the shared Hasql pool
            -- passed from the background-worker environment.
            scheduledJobsRunnerArbiterConnStr = env.arbiterConnStr,
            scheduledJobsRunnerSchemaName = ArbiterCore.defaultSchemaName,
            scheduledJobsRunnerSettings = workerSettings
          } ::
          ScheduledJobsRunnerConfig ScheduledJobsRegistry
  liftIO $ runScheduledJobsMigrations env.arbiterConnStr ArbiterCore.defaultSchemaName
  liftIO $ runScheduledJobsRunner env extEnv workersConfig cleanupConfig

scheduledJobsDuration :: Duration -> NominalDiffTime
scheduledJobsDuration = realToFrac . duration

scheduledJobsJitter :: ScheduledJobsJitter -> ArbiterWorker.Jitter
scheduledJobsJitter = \case
  ScheduledJobsNoJitter -> ArbiterWorker.NoJitter
  ScheduledJobsFullJitter -> ArbiterWorker.FullJitter
  ScheduledJobsEqualJitter -> ArbiterWorker.EqualJitter

-- | Start the worker pools for the scheduled-job queues.
--
-- Each domain queue has its own Arbiter table and worker pool. The meetings
-- pool owns the recurring cleanup cron job, while the conversations pool owns
-- the adminless one-off jobs. Both pools are supervised by Arbiter's
-- multi-pool runner, so they share the process lifecycle without sharing a
-- payload type or queue.
runScheduledJobsRunner ::
  forall registry.
  ( RegistryTables registry,
    RegistryAdmissionPolicies registry,
    KnownSymbol (TableForPayload MeetingsJobPayload registry),
    KnownSymbol (TableForPayload ConversationsJobPayload registry)
  ) =>
  Env ->
  ExtEnv ->
  ScheduledJobsRunnerConfig registry ->
  CleanupConfig ->
  IO (IO ())
runScheduledJobsRunner env extEnv runnerConfig cleanupConfig = do
  let arbiterConnStr = Text.encodeUtf8 (revealSecretText runnerConfig.scheduledJobsRunnerArbiterConnStr)
  Log.info runnerConfig.scheduledJobsRunnerLogger $
    Log.msg (Log.val "Starting scheduled jobs worker")
      . Log.field "queue_names" (T.intercalate "," [meetingsQueueName, conversationsQueueName])
      . Log.field "schedule" (show runnerConfig.scheduledJobsRunnerSchedule)

  let arbiterEnv = mkNewWireArbiterEnv runnerConfig.scheduledJobsRunnerSchemaName env.hasqlPool
      meetingsWorkerHandler _conn job = liftIO $ do
        Log.info runnerConfig.scheduledJobsRunnerLogger $
          Log.msg (Log.val "Running scheduled job")
            . Log.field "queue_name" meetingsQueueName
            . Log.field "payload_type" (meetingsJobPayloadTypeName job.payload)
        case job.payload of
          MeetingsCleanup _ -> runAppT env $ runCleanupOldMeetings cleanupConfig

      conversationsWorkerHandler _conn job = liftIO $ do
        Log.info runnerConfig.scheduledJobsRunnerLogger $
          Log.msg (Log.val "Running scheduled job")
            . Log.field "queue_name" conversationsQueueName
            . Log.field "payload_type" (conversationsJobPayloadTypeName job.payload)
        case job.payload of
          AdminlessDeletion payload -> runAppT env $ runAdminlessDeletionJob extEnv (mapJobPayload (const payload) job)
          AdminlessReminder payload -> runAppT env $ runAdminlessReminderJob extEnv (mapJobPayload (const payload) job)

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

  meetingsWorkerConfig <-
    ( ArbiterWorker.defaultWorkerConfig
        arbiterConnStr
        runnerConfig.scheduledJobsRunnerSettings.scheduledJobWorkerThreads
        meetingsWorkerHandler ::
        IO
          ( ArbiterWorker.WorkerConfig
              (WireArbiter registry)
              MeetingsJobPayload
              ()
          )
    )

  conversationsWorkerConfig <-
    ( ArbiterWorker.defaultWorkerConfig
        arbiterConnStr
        runnerConfig.scheduledJobsRunnerSettings.scheduledJobWorkerThreads
        conversationsWorkerHandler ::
        IO
          ( ArbiterWorker.WorkerConfig
              (WireArbiter registry)
              ConversationsJobPayload
              ()
          )
    )

  let meetingsWorkerConfig' =
        applyExplicitDefaults
          runnerConfig.scheduledJobsRunnerSettings
          meetingsWorkerConfig
            { ArbiterWorkerConfig.cronJobs = [cronJob]
            }
      conversationsWorkerConfig' =
        applyExplicitDefaults
          runnerConfig.scheduledJobsRunnerSettings
          conversationsWorkerConfig
      workerPools =
        [ ArbiterWorker.namedWorkerPool meetingsWorkerConfig',
          ArbiterWorker.namedWorkerPool conversationsWorkerConfig'
        ]
      shutdownWorkerPools _ = do
        ArbiterWorker.shutdownWorker meetingsWorkerConfig'
        ArbiterWorker.shutdownWorker conversationsWorkerConfig'

  workerAsync <-
    Async.async $
      runWireArbiter arbiterEnv $
        ArbiterWorker.runWorkerPools
          (Proxy @registry)
          workerPools
          shutdownWorkerPools

  pure $ do
    ArbiterWorker.shutdownWorker meetingsWorkerConfig'
    ArbiterWorker.shutdownWorker conversationsWorkerConfig'
    Async.cancel workerAsync

meetingsJobPayloadTypeName :: MeetingsJobPayload -> Text
meetingsJobPayloadTypeName = \case
  MeetingsCleanup _ -> "meetings_cleanup"

conversationsJobPayloadTypeName :: ConversationsJobPayload -> Text
conversationsJobPayloadTypeName = \case
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
