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
import Data.Proxy (Proxy (..))
import Imports
import Wire.API.Jobs
  ( AdminlessDeletionJob (..),
    MeetingsCleanupJob (..),
    ScheduledJobsRegistry,
    adminlessDeletionQueueName,
    meetingsCleanupQueueName,
  )
import Wire.BackgroundWorker.Env (AppT, Env (..), runAppT)
import Wire.BackgroundWorker.Options (MeetingsCleanupConfig (..))
import Wire.BackgroundWorker.Util (CleanupAction)
import Wire.JobSubsystem.Recurring
  ( OneOffJobRunnerConfig (..),
    RecurringJobRunnerConfig (..),
    runOneOffJobRunner,
    runRecurringJobRunner,
  )
import Wire.MeetingsCleanupWorker
  ( CleanupConfig (..),
    runCleanupOldMeetings,
  )

startWorker :: MeetingsCleanupConfig -> AppT IO CleanupAction
startWorker config = do
  env <- ask
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
  cleanupRecurring <-
    liftIO $
      runRecurringJobRunner @ScheduledJobsRegistry @MeetingsCleanupJob
        (Proxy @ScheduledJobsRegistry)
        RecurringJobRunnerConfig
          { recurringJobRunnerLogger = env.logger,
            recurringJobRunnerSchedule = config.schedule,
            recurringJobRunnerArbiterConnStr = env.arbiterConnStr,
            recurringJobRunnerSchemaName = ArbiterCore.defaultSchemaName,
            recurringJobRunnerWorkerThreads = 1,
            recurringJobRunnerEnqueueAt = \scheduledFor ->
              void $ ArbiterCore.insertJob (jobWrite scheduledFor),
            recurringJobRunnerRunJob =
              runAppT env $
                runCleanupOldMeetings cleanupConfig,
            recurringJobRunnerJobName = "meetings-cleanup",
            recurringJobRunnerQueueName = meetingsCleanupQueueName
          }
  cleanupDeletion <-
    liftIO $
      runOneOffJobRunner @ScheduledJobsRegistry @AdminlessDeletionJob
        (Proxy @ScheduledJobsRegistry)
        OneOffJobRunnerConfig
          { oneOffJobRunnerLogger = env.logger,
            oneOffJobRunnerArbiterConnStr = env.arbiterConnStr,
            oneOffJobRunnerSchemaName = ArbiterCore.defaultSchemaName,
            oneOffJobRunnerWorkerThreads = 1,
            oneOffJobRunnerRunJob =
              runAppT env $
                pure (),
            oneOffJobRunnerJobName = "adminless-deletion",
            oneOffJobRunnerQueueName = adminlessDeletionQueueName
          }
  pure $ cleanupRecurring >> cleanupDeletion
