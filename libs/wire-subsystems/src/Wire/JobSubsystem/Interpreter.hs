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
    runJobRunner,
  )
where

import Arbiter.Core qualified as ArbiterCore
import Arbiter.Hasql.HasqlDb qualified as ArbiterHasql
import Data.Id
import Data.Proxy (Proxy (..))
import Data.UUID.V4 qualified as UUID
import Imports
import Polysemy
import Wire.API.Jobs
import Wire.JobStore qualified as JobStore
import Wire.JobSubsystem
import Wire.JobSubsystem.Recurring (runRecurringJobRunner)

runJobRunner :: JobRunnerConfig -> IO CleanupAction
runJobRunner = runRecurringJobRunner (Proxy @ScheduledJobsRegistry)

interpretJobSubsystem ::
  ( Member JobStore.JobStore r,
    Member (Embed IO) r
  ) =>
  JobSubsystemConfig ->
  Sem (JobSubsystem : r) a ->
  Sem r a
interpretJobSubsystem JobSubsystemConfig {..} sem = do
  arbiterEnv <-
    embed $
      ArbiterHasql.createHasqlEnv
        (Proxy @ScheduledJobsRegistry)
        jobSubsystemArbiterConnStr
        jobSubsystemSchemaName
  interpret
    ( \case
        RegisterJob job -> JobStore.createJob job
        CancelJob jobId -> JobStore.deleteJob jobId
        CancelJobsByTeamAndKind teamId kind -> JobStore.deleteJobsByTeamAndKind teamId kind
        FindJobById jobId -> JobStore.findJobById jobId
        FindJobsByTeamAndKind teamId kind -> JobStore.findJobsByTeamAndKind teamId kind
        FindJobsByConversationId conversationId -> JobStore.findJobsByConversationId conversationId
        ScheduleAdminlessDeletionJob teamId conversationId scheduledFor -> do
          jobId <- embed $ Id <$> UUID.nextRandom
          let job =
                ScheduledJob
                  { scheduledJobId = jobId,
                    scheduledJobKind = AdminlessDeletion,
                    scheduledJobTeamId = teamId,
                    scheduledJobConversationId = conversationId,
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
        StartJobRunner cfg -> embed $ runRecurringJobRunner (Proxy @ScheduledJobsRegistry) cfg
    )
    sem
