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
import Data.Id (RequestId (..))
import Data.Misc (Duration, durationToMicros)
import Data.Time.Clock (NominalDiffTime)
import Imports
import Wire.API.Jobs
import Wire.AdminlessJobsWorker (runAdminlessDeletionJob, runAdminlessReminderJob)
import Wire.BackgroundWorker.Env (AppT, Env (..), runAppT)
import Wire.BackgroundWorker.Options (MeetingsCleanupConfig (..), ScheduledJobsConfig (..))
import Wire.Effects (runBackgroundWorkerEffects)
import Wire.ExternalAccess.External (initExtEnv)
import Wire.JobSubsystem
import Wire.JobSubsystem.Workers
import Wire.MeetingsCleanupWorker

-- | Initial worker pool size for the scheduled-jobs queues.
--
-- Keep this explicit so the next tuning pass has a single obvious place to
-- change the parallelism for meetings cleanup and adminless jobs.
scheduledJobsWorkerThreads :: Int
scheduledJobsWorkerThreads = 1

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
          { recurringJobRunnerRunJob = \_ ->
              runAppT env $
                runCleanupOldMeetings cleanupConfig,
            adminlessDeletionJobRunnerRunJob = \job ->
              runAppT env $
                runAdminlessDeletionJob extEnv job,
            adminlessReminderJobRunnerRunJob = \job ->
              runAppT env $
                runAdminlessReminderJob extEnv job
          }
      workersConfig =
        JobWorkersConfig
          { recurringJobRunnerConfig =
              RecurringJobRunnerConfig
                { recurringJobRunnerLogger = env.logger,
                  recurringJobRunnerSchedule = config.schedule,
                  recurringJobRunnerArbiterPool = env.arbiterPool,
                  -- Arbiter still uses the connection string for LISTEN/NOTIFY.
                  -- The actual job DB access goes through 'arbiterPool'.
                  recurringJobRunnerArbiterConnStr = env.arbiterConnStr,
                  recurringJobRunnerSchemaName = ArbiterCore.defaultSchemaName,
                  recurringJobRunnerPollInterval = scheduledJobsPollIntervalSeconds scheduledConfig.pollInterval,
                  recurringJobRunnerWorkerThreads = scheduledJobsWorkerThreads,
                  recurringJobRunnerJobName = "meetings-cleanup",
                  recurringJobRunnerQueueName = meetingsCleanupQueueName
                },
            adminlessDeletionJobRunnerConfig =
              OneOffJobRunnerConfig
                { oneOffJobRunnerLogger = env.logger,
                  oneOffJobRunnerArbiterPool = env.arbiterPool,
                  -- Arbiter still uses the connection string for LISTEN/NOTIFY.
                  -- The actual job DB access goes through 'arbiterPool'.
                  oneOffJobRunnerArbiterConnStr = env.arbiterConnStr,
                  oneOffJobRunnerSchemaName = ArbiterCore.defaultSchemaName,
                  oneOffJobRunnerPollInterval = scheduledJobsPollIntervalSeconds scheduledConfig.pollInterval,
                  oneOffJobRunnerWorkerThreads = scheduledJobsWorkerThreads,
                  oneOffJobRunnerJobName = "adminless-deletion",
                  oneOffJobRunnerQueueName = adminlessDeletionQueueName
                },
            adminlessReminderJobRunnerConfig =
              OneOffJobRunnerConfig
                { oneOffJobRunnerLogger = env.logger,
                  oneOffJobRunnerArbiterPool = env.arbiterPool,
                  oneOffJobRunnerArbiterConnStr = env.arbiterConnStr,
                  oneOffJobRunnerSchemaName = ArbiterCore.defaultSchemaName,
                  oneOffJobRunnerPollInterval = scheduledJobsPollIntervalSeconds scheduledConfig.pollInterval,
                  oneOffJobRunnerWorkerThreads = scheduledJobsWorkerThreads,
                  oneOffJobRunnerJobName = "adminless-reminder",
                  oneOffJobRunnerQueueName = adminlessReminderQueueName
                }
          }
  result <-
    liftIO $
      runBackgroundWorkerEffects env extEnv (RequestId "scheduled-jobs") Nothing $
        startJobWorkers workersConfig jobHandlers
  either (liftIO . fail . show) pure result

-- | Convert the explicit background-worker config into the Arbiter poll
-- interval in seconds.
scheduledJobsPollIntervalSeconds :: Duration -> NominalDiffTime
scheduledJobsPollIntervalSeconds = (/ 1_000_000) . fromIntegral . durationToMicros
