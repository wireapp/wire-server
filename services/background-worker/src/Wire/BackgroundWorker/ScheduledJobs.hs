{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
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
import Imports
import Wire.API.Jobs
import Wire.AdminlessJobsWorker (runAdminlessDeletionJob, runAdminlessReminderJob)
import Wire.BackgroundWorker.Env (AppT, Env (..), runAppT)
import Wire.BackgroundWorker.Options (MeetingsCleanupConfig (..))
import Wire.Effects (runBackgroundWorkerEffects)
import Wire.ExternalAccess.External (initExtEnv)
import Wire.JobSubsystem
import Wire.JobSubsystem.Workers
import Wire.MeetingsCleanupWorker

startWorker :: MeetingsCleanupConfig -> AppT IO CleanupAction
startWorker config = do
  env <- ask
  extEnv <- liftIO $ initExtEnv True
  let cleanupConfig =
        CleanupConfig
          { retentionHours = config.cleanOlderThanHours,
            batchSize = config.batchSize
          }
      jobWrite scheduledFor =
        (ArbiterCore.defaultGroupedJob meetingsCleanupQueueName MeetingsCleanupJob)
          { ArbiterCore.dedupKey = Just (ArbiterCore.IgnoreDuplicate meetingsCleanupQueueName),
            ArbiterCore.notVisibleUntil = Just scheduledFor
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
                runAdminlessReminderJob job
          }
      workersConfig =
        JobWorkersConfig
          { recurringJobRunnerConfig =
              RecurringJobRunnerConfig
                { recurringJobRunnerLogger = env.logger,
                  recurringJobRunnerSchedule = config.schedule,
                  recurringJobRunnerArbiterConnStr = env.arbiterConnStr,
                  recurringJobRunnerSchemaName = ArbiterCore.defaultSchemaName,
                  recurringJobRunnerWorkerThreads = 1,
                  recurringJobRunnerEnqueueAt = \scheduledFor ->
                    void $ ArbiterCore.insertJob (jobWrite scheduledFor),
                  recurringJobRunnerJobName = "meetings-cleanup",
                  recurringJobRunnerQueueName = meetingsCleanupQueueName
                },
            adminlessDeletionJobRunnerConfig =
              OneOffJobRunnerConfig
                { oneOffJobRunnerLogger = env.logger,
                  oneOffJobRunnerArbiterConnStr = env.arbiterConnStr,
                  oneOffJobRunnerSchemaName = ArbiterCore.defaultSchemaName,
                  oneOffJobRunnerWorkerThreads = 1,
                  oneOffJobRunnerJobName = "adminless-deletion",
                  oneOffJobRunnerQueueName = adminlessDeletionQueueName
                },
            adminlessReminderJobRunnerConfig =
              OneOffJobRunnerConfig
                { oneOffJobRunnerLogger = env.logger,
                  oneOffJobRunnerArbiterConnStr = env.arbiterConnStr,
                  oneOffJobRunnerSchemaName = ArbiterCore.defaultSchemaName,
                  oneOffJobRunnerWorkerThreads = 1,
                  oneOffJobRunnerJobName = "adminless-reminder",
                  oneOffJobRunnerQueueName = adminlessReminderQueueName
                }
          }
  result <-
    liftIO $
      runBackgroundWorkerEffects env extEnv (RequestId "scheduled-jobs") Nothing $
        startJobWorkers workersConfig jobHandlers
  either (liftIO . fail . show) pure result
