{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Arbiter.Core.Exceptions (throwRetryable)
import Arbiter.Worker qualified as ArbiterWorker
import Arbiter.Worker.Config qualified as ArbiterWorkerConfig
import Arbiter.Worker.Cron qualified as ArbiterWorkerCron
import Control.Exception (throwIO)
import Data.Id (RequestId)
import Data.Misc (Duration, duration)
import Data.Range (fromRange)
import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime)
import Imports
import System.Cron (CronSchedule, serializeCronSchedule)
import System.IO.Error (userError)
import System.Logger qualified as Log
import UnliftIO.Async qualified as Async
import Wire.API.BackgroundJobs (BackgroundJobPayload (..))
import Wire.API.Jobs
import Wire.AdminlessJobsWorker (runAdminlessDeletionJob, runAdminlessReminderJob, runAdminlessSetupJob)
import Wire.BackgroundJobsPublisher.Arbiter (interpretBackgroundJobPublisherArbiter)
import Wire.BackgroundJobsRunner (runJob)
import Wire.BackgroundJobsRunner.Interpreter (interpretBackgroundJobRunner)
import Wire.BackgroundWorker.Env (AppT, Env (..), runAppT)
import Wire.BackgroundWorker.Options (JobConfig (..), JobJitter (..), MeetingsCleanupConfig (..))
import Wire.BackgroundWorker.Util
import Wire.Effects (runBackgroundWorkerEffects)
import Wire.ExternalAccess.External
import Wire.JobSubsystem (JobSubsystemConfig (..))
import Wire.JobSubsystem.ArbiterAdapter
import Wire.JobSubsystem.Migrations (runJobMigrations)
import Wire.MeetingsCleanupWorker

-- | Runtime settings shared by every job runner in a process.
--
-- These values deliberately mirror the Arbiter worker defaults that we use.
-- Keeping them in one record ensures all job types use the same
-- execution policy as settings are added or tuned.
data JobWorkerSettings = JobWorkerSettings
  { jobWorkerThreads :: Int,
    jobPollInterval :: NominalDiffTime,
    jobVisibilityTimeout :: NominalDiffTime,
    jobHeartbeatInterval :: NominalDiffTime,
    jobWorkerHeartbeatInterval :: NominalDiffTime,
    jobBackoffBase :: Double,
    jobBackoffCap :: NominalDiffTime,
    jobJitter :: ArbiterWorker.Jitter,
    jobGracefulShutdownTimeout :: Maybe NominalDiffTime,
    jobReaperInterval :: NominalDiffTime,
    jobReaperTimeout :: NominalDiffTime,
    jobWorkerStaleThreshold :: NominalDiffTime
  }

data JobRunnerConfig registry = JobRunnerConfig
  { jobRunnerLogger :: Log.Logger,
    jobRunnerSchedule :: CronSchedule,
    jobRunnerSchemaName :: Text,
    jobRunnerSettings :: JobWorkerSettings
  }

startWorker :: JobConfig -> MeetingsCleanupConfig -> AppT IO CleanupAction
startWorker scheduledConfig meetingsCleanupConfig = do
  env <- ask
  extEnv <- liftIO $ initExtEnv True
  let cleanupConfig =
        CleanupConfig
          { retentionHours = meetingsCleanupConfig.cleanOlderThanHours,
            batchSize = meetingsCleanupConfig.batchSize
          }
      workerSettings =
        JobWorkerSettings
          { jobWorkerThreads = fromRange scheduledConfig.workerThreads,
            jobPollInterval = jobDuration scheduledConfig.pollInterval,
            jobVisibilityTimeout = jobDuration scheduledConfig.visibilityTimeout,
            jobHeartbeatInterval = jobDuration scheduledConfig.jobHeartbeatInterval,
            jobWorkerHeartbeatInterval = jobDuration scheduledConfig.workerHeartbeatInterval,
            jobBackoffBase = scheduledConfig.backoffBase,
            jobBackoffCap = jobDuration scheduledConfig.backoffCap,
            jobJitter = toJobJitter scheduledConfig.jitter,
            jobGracefulShutdownTimeout = fmap jobDuration scheduledConfig.gracefulShutdownTimeout,
            jobReaperInterval = jobDuration scheduledConfig.reaperInterval,
            jobReaperTimeout = jobDuration scheduledConfig.reaperTimeout,
            jobWorkerStaleThreshold = jobDuration scheduledConfig.workerStaleThreshold
          }
      workersConfig =
        JobRunnerConfig
          { jobRunnerLogger = env.logger,
            jobRunnerSchedule = meetingsCleanupConfig.schedule,
            jobRunnerSchemaName = ArbiterCore.defaultSchemaName,
            jobRunnerSettings = workerSettings
          } ::
          JobRunnerConfig JobRegistry
  liftIO $ runJobMigrations env.arbiterConnStr ArbiterCore.defaultSchemaName
  liftIO $ runJobRunner env extEnv workersConfig cleanupConfig

jobDuration :: Duration -> NominalDiffTime
jobDuration = realToFrac . duration

toJobJitter :: JobJitter -> ArbiterWorker.Jitter
toJobJitter = \case
  JobNoJitter -> ArbiterWorker.NoJitter
  JobFullJitter -> ArbiterWorker.FullJitter
  JobEqualJitter -> ArbiterWorker.EqualJitter

-- | Start the worker pools for the job queues.
--
-- Each domain queue has its own Arbiter table and worker pool. The meetings
-- pool owns the recurring cleanup cron job, while the conversations pool owns
-- the adminless one-off jobs. Both pools are supervised by Arbiter's
-- multi-pool runner, so they share the process lifecycle without sharing a
-- payload type or queue.
runJobRunner ::
  Env ->
  ExtEnv ->
  JobRunnerConfig JobRegistry ->
  CleanupConfig ->
  IO (IO ())
runJobRunner env extEnv runnerConfig cleanupConfig = do
  Log.info runnerConfig.jobRunnerLogger $
    Log.msg (Log.val "Starting job worker")
      . Log.field "queue_names" (T.intercalate "," [meetingsQueueName, conversationsQueueName, userGroupsQueueName])
      . Log.field "schedule" (show runnerConfig.jobRunnerSchedule)

  let arbiterEnv = mkNewWireArbiterEnv runnerConfig.jobRunnerSchemaName env.hasqlPool
      meetingsWorkerHandler _conn job = liftIO $ do
        Log.info runnerConfig.jobRunnerLogger $
          Log.msg (Log.val "Running job")
            . Log.field "queue_name" meetingsQueueName
            . Log.field "payload_type" (meetingsJobPayloadTypeName job.payload)
        case job.payload of
          MeetingsCleanup _ -> runAppT env $ runCleanupOldMeetings cleanupConfig

      conversationsWorkerHandler _conn job = liftIO $ do
        Log.info runnerConfig.jobRunnerLogger $
          Log.msg (Log.val "Running job")
            . Log.field "queue_name" conversationsQueueName
            . Log.field "payload_type" (conversationsJobPayloadTypeName job.payload)
        case job.payload of
          AdminlessSetup payload -> runAppT env $ runAdminlessSetupJob extEnv (mapJobPayload (const payload) job)
          AdminlessDeletion payload -> runAppT env $ runAdminlessDeletionJob extEnv (mapJobPayload (const payload) job)
          AdminlessReminder payload -> runAppT env $ runAdminlessReminderJob extEnv (mapJobPayload (const payload) job)

      userGroupsWorkerHandler _conn job = liftIO $ do
        Log.info runnerConfig.jobRunnerLogger $
          Log.msg (Log.val "Running job")
            . Log.field "queue_name" userGroupsQueueName
            . Log.field "payload_type" (userGroupsJobPayloadTypeName job.payload)
        case job.payload of
          UserGroupsSyncUserGroup payload ->
            runUserGroupsJob env extEnv runnerConfig.jobRunnerSchemaName payload.userGroupsSyncUserGroupJobRequestId $
              BackgroundJobSyncUserGroup payload.userGroupsSyncUserGroupJobData
          UserGroupsSyncUserGroupAndChannel payload ->
            runUserGroupsJob env extEnv runnerConfig.jobRunnerSchemaName payload.userGroupsSyncUserGroupAndChannelJobRequestId $
              BackgroundJobSyncUserGroupAndChannel payload.userGroupsSyncUserGroupAndChannelJobData

  cronJob <- case ArbiterWorkerCron.cronJob
    "meetings-cleanup"
    (serializeCronSchedule runnerConfig.jobRunnerSchedule)
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
    ( ArbiterWorker.transactionalWorkerConfig
        runnerConfig.jobRunnerSettings.jobWorkerThreads
        meetingsWorkerHandler ::
        IO
          ( ArbiterWorker.WorkerConfig
              (WireArbiter JobRegistry)
              MeetingsJobPayload
          )
    )

  conversationsWorkerConfig <-
    ( ArbiterWorker.transactionalWorkerConfig
        runnerConfig.jobRunnerSettings.jobWorkerThreads
        conversationsWorkerHandler ::
        IO
          ( ArbiterWorker.WorkerConfig
              (WireArbiter JobRegistry)
              ConversationsJobPayload
          )
    )

  userGroupsWorkerConfig <-
    ( ArbiterWorker.transactionalWorkerConfig
        runnerConfig.jobRunnerSettings.jobWorkerThreads
        userGroupsWorkerHandler ::
        IO
          ( ArbiterWorker.WorkerConfig
              (WireArbiter JobRegistry)
              UserGroupsJobPayload
          )
    )

  let meetingsWorkerConfig' =
        applyExplicitDefaults
          runnerConfig.jobRunnerSettings
          meetingsWorkerConfig
            { ArbiterWorkerConfig.cronJobs = [cronJob]
            }
      conversationsWorkerConfig' =
        applyExplicitDefaults
          runnerConfig.jobRunnerSettings
          conversationsWorkerConfig
      workerPools =
        [ ArbiterWorker.namedWorkerPool meetingsWorkerConfig',
          ArbiterWorker.namedWorkerPool conversationsWorkerConfig',
          ArbiterWorker.namedWorkerPool $
            applyExplicitDefaults runnerConfig.jobRunnerSettings userGroupsWorkerConfig
        ]

  workerAsync <-
    Async.async $
      runWireArbiter arbiterEnv $
        ArbiterWorker.runWorkerPools workerPools

  pure $ do
    runWireArbiter arbiterEnv $ ArbiterWorker.shutdownPools workerPools
    Async.cancel workerAsync

meetingsJobPayloadTypeName :: MeetingsJobPayload -> Text
meetingsJobPayloadTypeName = \case
  MeetingsCleanup _ -> "meetings_cleanup"

conversationsJobPayloadTypeName :: ConversationsJobPayload -> Text
conversationsJobPayloadTypeName = \case
  AdminlessSetup _ -> "adminless_setup"
  AdminlessDeletion _ -> "adminless_deletion"
  AdminlessReminder _ -> "adminless_reminder"

userGroupsJobPayloadTypeName :: UserGroupsJobPayload -> Text
userGroupsJobPayloadTypeName = \case
  UserGroupsSyncUserGroup _ -> "sync_user_group"
  UserGroupsSyncUserGroupAndChannel _ -> "sync_user_group_and_channel"

mapJobPayload :: (a -> b) -> ArbiterCore.JobRead a -> ArbiterCore.JobRead b
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
      ArbiterCore.archiveFor = job.archiveFor,
      ArbiterCore.admission = job.admission
    }

-- | Run a user-groups sync job through the standard dispatch pipeline,
-- publishing any chained syncs back onto the Arbiter user-groups queue.
-- Failures are raised as retryable so Arbiter retries up to 'maxAttempts'.
runUserGroupsJob :: Env -> ExtEnv -> Text -> RequestId -> BackgroundJobPayload -> IO ()
runUserGroupsJob env extEnv schemaName requestId payload = do
  result <-
    runBackgroundWorkerEffects env extEnv requestId Nothing
      . interpretBackgroundJobPublisherArbiter requestId (JobSubsystemConfig schemaName)
      . interpretBackgroundJobRunner
      $ runJob payload
  either throwRetryable pure result

applyExplicitDefaults ::
  JobWorkerSettings ->
  ArbiterWorker.WorkerConfig m payload ->
  ArbiterWorker.WorkerConfig m payload
applyExplicitDefaults settings cfg =
  cfg
    { -- How often the dispatcher wakes up to look for newly visible jobs.
      -- Lower values reduce discovery latency at the cost of more DB traffic.
      ArbiterWorkerConfig.pollInterval = settings.jobPollInterval,
      -- How long a claimed job stays invisible while a worker processes it.
      -- Must exceed the job heartbeat interval so active jobs are not reclaimed.
      ArbiterWorkerConfig.visibilityTimeout = settings.jobVisibilityTimeout,
      -- How often a running job refreshes its visibility timeout.
      -- Keeps long-running jobs from being reclaimed mid-flight.
      ArbiterWorkerConfig.jobHeartbeatInterval = settings.jobHeartbeatInterval,
      -- How often the worker process updates its own heartbeat and pause state.
      -- This drives liveness, re-registration, and paused-state reconciliation.
      ArbiterWorkerConfig.workerHeartbeatInterval = settings.jobWorkerHeartbeatInterval,
      -- Retry strategy for transient worker failures.
      -- Arbiter uses exponential backoff with jitter by default.
      ArbiterWorkerConfig.backoffStrategy =
        ArbiterWorker.exponentialBackoff
          settings.jobBackoffBase
          settings.jobBackoffCap,
      -- Jitter mode for retry delays.
      -- Equal jitter smooths retry spikes without making them too aggressive.
      ArbiterWorkerConfig.jitter = settings.jobJitter,
      -- How long the worker waits for in-flight jobs during shutdown.
      -- If set, the pool exits after this grace period instead of waiting forever.
      ArbiterWorkerConfig.gracefulShutdownTimeout = settings.jobGracefulShutdownTimeout,
      -- How often the reaper runs. It refreshes groups, sweeps stale workers,
      -- and moves exhausted jobs to the DLQ.
      ArbiterWorkerConfig.reaperInterval = settings.jobReaperInterval,
      -- Maximum time allowed for one reaper pass.
      ArbiterWorkerConfig.reaperTimeout = settings.jobReaperTimeout,
      -- How old a worker heartbeat may be before it is considered stale.
      -- Stale workers are swept from the registry by the reaper.
      ArbiterWorkerConfig.workerStaleThreshold = settings.jobWorkerStaleThreshold
    }
