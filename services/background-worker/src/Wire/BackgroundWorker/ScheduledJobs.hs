{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

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
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.BackgroundWorker.ScheduledJobs (startWorker) where

import Arbiter.Core qualified as ArbiterCore
import Arbiter.Worker qualified as ArbiterWorker
import Data.Id (RequestId (..))
import Data.Misc (Duration, duration)
import Data.Range (fromRange)
import Data.Time.Clock (NominalDiffTime)
import Imports
import Wire.AdminlessJobsWorker (runAdminlessDeletionJob, runAdminlessReminderJob)
import Wire.BackgroundWorker.Env (AppT, Env (..), runAppT)
import Wire.BackgroundWorker.Options (MeetingsCleanupConfig (..), ScheduledJobsConfig (..), ScheduledJobsJitter (..))
import Wire.Effects (runBackgroundWorkerEffects)
import Wire.ExternalAccess.External (initExtEnv)
import Wire.JobSubsystem
import Wire.JobSubsystem.Workers
import Wire.MeetingsCleanupWorker

startWorker :: ScheduledJobsConfig -> MeetingsCleanupConfig -> AppT IO CleanupAction
startWorker scheduledConfig config = do
  env <- ask
  extEnv <- liftIO $ initExtEnv True
  let cleanupConfig =
        CleanupConfig
          { retentionHours = config.cleanOlderThanHours,
            batchSize = config.batchSize
          }
      jobHandlers =
        JobWorkerHandlers
          { scheduledJobsRunMeetingsCleanup = \_ ->
              runAppT env $
                runCleanupOldMeetings cleanupConfig,
            scheduledJobsRunAdminlessDeletion = \job ->
              runAppT env $
                runAdminlessDeletionJob extEnv job,
            scheduledJobsRunAdminlessReminder = \job ->
              runAppT env $
                runAdminlessReminderJob extEnv job
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
        JobWorkersConfig
          { scheduledJobsRunnerConfig =
              ScheduledJobsRunnerConfig
                { scheduledJobsRunnerLogger = env.logger,
                  scheduledJobsRunnerSchedule = config.schedule,
                  -- Arbiter still uses the connection string for LISTEN/NOTIFY.
                  -- The actual job DB access goes through the shared Hasql pool
                  -- pulled from the JobSubsystem interpreter.
                  scheduledJobsRunnerArbiterConnStr = env.arbiterConnStr,
                  scheduledJobsRunnerSchemaName = ArbiterCore.defaultSchemaName,
                  scheduledJobsRunnerSettings = workerSettings
                }
          }
  result <-
    liftIO $
      runBackgroundWorkerEffects env extEnv (RequestId "scheduled-jobs") Nothing $
        startJobWorkers workersConfig jobHandlers
  either (liftIO . fail . show) pure result

scheduledJobsDuration :: Duration -> NominalDiffTime
scheduledJobsDuration = realToFrac . duration

scheduledJobsJitter :: ScheduledJobsJitter -> ArbiterWorker.Jitter
scheduledJobsJitter = \case
  ScheduledJobsNoJitter -> ArbiterWorker.NoJitter
  ScheduledJobsFullJitter -> ArbiterWorker.FullJitter
  ScheduledJobsEqualJitter -> ArbiterWorker.EqualJitter
