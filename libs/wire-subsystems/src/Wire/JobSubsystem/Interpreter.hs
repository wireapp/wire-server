{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
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
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
-- for more details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.JobSubsystem.Interpreter
  ( interpretJobSubsystem,
    runJobWorkers,
  )
where

import Arbiter.Core qualified as ArbiterCore
import Arbiter.Hasql.HasqlDb qualified as ArbiterHasql
import Data.Id
import Data.Proxy (Proxy (..))
import Data.Qualified
import Data.Time
import Data.UUID.V4 qualified as UUID
import Imports
import Polysemy
import Wire.API.Jobs
import Wire.JobStore qualified as JobStore
import Wire.JobSubsystem (CleanupAction, JobSubsystem (..), JobSubsystemConfig (..), JobWorkerHandlers (..), JobWorkersConfig (..))
import Wire.JobSubsystem.Workers (runOneOffJobRunner, runRecurringJobRunner)

runJobWorkers :: JobWorkersConfig -> JobWorkerHandlers -> IO CleanupAction
runJobWorkers JobWorkersConfig {..} JobWorkerHandlers {..} = do
  cleanupRecurring <- runRecurringJobRunner (Proxy @ScheduledJobsRegistry) recurringJobRunnerConfig recurringJobRunnerRunJob
  cleanupDeletion <- runOneOffJobRunner (Proxy @ScheduledJobsRegistry) adminlessDeletionJobRunnerConfig adminlessDeletionJobRunnerRunJob
  cleanupReminder <- runOneOffJobRunner (Proxy @ScheduledJobsRegistry) adminlessReminderJobRunnerConfig adminlessReminderJobRunnerRunJob
  pure $ cleanupRecurring >> cleanupDeletion >> cleanupReminder

interpretJobSubsystem ::
  ( Member JobStore.JobStore r,
    Member (Embed IO) r
  ) =>
  JobSubsystemConfig ->
  InterpreterFor JobSubsystem r
interpretJobSubsystem conf =
  interpret
    \case
      ScheduleAdminlessDeletionJob lusr tid cid scheduledFor -> scheduleAdminlessDeletionJob conf lusr tid cid scheduledFor
      ScheduleAdminlessReminderJob lusr tid cid scheduledFor -> scheduleAdminlessReminderJob conf lusr tid cid scheduledFor
      StartJobWorkers cfg handlers -> embed $ runJobWorkers cfg handlers

scheduleAdminlessDeletionJob ::
  forall r.
  (Member (Embed IO) r, Member JobStore.JobStore r) =>
  JobSubsystemConfig ->
  Local UserId ->
  TeamId ->
  ConvId ->
  UTCTime ->
  Sem r ScheduledJob
scheduleAdminlessDeletionJob JobSubsystemConfig {..} lusr teamId convId scheduledFor = do
  arbiterEnv <-
    embed $
      ArbiterHasql.createHasqlEnv
        (Proxy @ScheduledJobsRegistry)
        jobSubsystemArbiterConnStr
        jobSubsystemSchemaName
  jobId <- embed $ Id <$> UUID.nextRandom
  let job =
        ScheduledJob
          { scheduledJobId = jobId,
            scheduledJobKind = AdminlessDeletion,
            scheduledJobTeamId = teamId,
            scheduledJobConversationId = Just convId,
            scheduledJobScheduledFor = scheduledFor
          }
      arbiterJob =
        (ArbiterCore.defaultGroupedJob adminlessDeletionQueueName (AdminlessDeletionJob teamId convId (tUnqualified lusr)))
          { ArbiterCore.notVisibleUntil = Just scheduledFor
          }
  JobStore.createJob job
  embed $
    void $
      ArbiterHasql.runHasqlDb arbiterEnv $
        void $
          ArbiterCore.insertJob arbiterJob
  pure job

scheduleAdminlessReminderJob ::
  forall r.
  (Member (Embed IO) r, Member JobStore.JobStore r) =>
  JobSubsystemConfig ->
  Local UserId ->
  TeamId ->
  ConvId ->
  UTCTime ->
  Sem r ScheduledJob
scheduleAdminlessReminderJob JobSubsystemConfig {..} lusr teamId convId scheduledFor = do
  arbiterEnv <-
    embed $
      ArbiterHasql.createHasqlEnv
        (Proxy @ScheduledJobsRegistry)
        jobSubsystemArbiterConnStr
        jobSubsystemSchemaName
  jobId <- embed $ Id <$> UUID.nextRandom
  let job =
        ScheduledJob
          { scheduledJobId = jobId,
            scheduledJobKind = AdminlessReminder,
            scheduledJobTeamId = teamId,
            scheduledJobConversationId = Just convId,
            scheduledJobScheduledFor = scheduledFor
          }
      arbiterJob =
        (ArbiterCore.defaultGroupedJob adminlessReminderQueueName (AdminlessReminderJob teamId convId (tUnqualified lusr)))
          { ArbiterCore.notVisibleUntil = Just scheduledFor
          }
  JobStore.createJob job
  embed $
    void $
      ArbiterHasql.runHasqlDb arbiterEnv $
        void $
          ArbiterCore.insertJob arbiterJob
  pure job
