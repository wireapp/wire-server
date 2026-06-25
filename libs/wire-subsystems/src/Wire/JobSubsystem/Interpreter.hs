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
import Data.Time
import Data.UUID.V4 qualified as UUID
import Imports
import Polysemy
import Wire.API.Jobs
import Wire.JobStore qualified as JobStore
import Wire.JobSubsystem (CleanupAction, JobSubsystem (..), JobSubsystemConfig (..), JobWorkersConfig (..))
import Wire.JobSubsystem.Workers (runOneOffJobRunner, runRecurringJobRunner)

runJobWorkers :: JobWorkersConfig -> IO CleanupAction
runJobWorkers JobWorkersConfig {..} = do
  cleanupRecurring <- runRecurringJobRunner (Proxy @ScheduledJobsRegistry) recurringJobRunnerConfig
  cleanupOneOff <- runOneOffJobRunner (Proxy @ScheduledJobsRegistry) oneOffJobRunnerConfig
  pure $ cleanupRecurring >> cleanupOneOff

interpretJobSubsystem ::
  ( Member JobStore.JobStore r,
    Member (Embed IO) r
  ) =>
  JobSubsystemConfig ->
  InterpreterFor JobSubsystem r
interpretJobSubsystem conf =
  interpret
    \case
      ScheduleAdminlessDeletionJob tid cid scheduledFor -> scheduleAdminlessDeletionJob conf tid cid scheduledFor
      StartJobWorkers cfg -> embed $ runJobWorkers cfg

scheduleAdminlessDeletionJob ::
  forall r.
  (Member (Embed IO) r, Member JobStore.JobStore r) =>
  JobSubsystemConfig ->
  TeamId ->
  Maybe ConvId ->
  UTCTime ->
  Sem r ScheduledJob
scheduleAdminlessDeletionJob JobSubsystemConfig {..} teamId convId scheduledFor = do
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
            scheduledJobConversationId = convId,
            scheduledJobScheduledFor = scheduledFor
          }
      arbiterJob =
        (ArbiterCore.defaultGroupedJob adminlessDeletionQueueName AdminlessDeletionJob)
          { ArbiterCore.notVisibleUntil = Just scheduledFor
          }
  JobStore.createJob job
  embed $
    void $
      ArbiterHasql.runHasqlDb arbiterEnv $
        void $
          ArbiterCore.insertJob arbiterJob
  pure job
