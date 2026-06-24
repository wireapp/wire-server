{-# LANGUAGE TemplateHaskell #-}

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

module Wire.JobSubsystem
  ( CleanupAction,
    JobSubsystemConfig (..),
    JobSubsystem (..),
    JobRunnerConfig,
    registerJob,
    scheduleAdminlessDeletionJob,
    registerAdminlessDeletionJob,
    cancelJob,
    cancelJobsByTeamAndKind,
    findJobById,
    findJobsByConversationId,
    findJobsByTeamAndKind,
    startJobRunner,
  )
where

import Data.ByteString qualified as ByteString
import Data.Time.Clock (UTCTime)
import Imports
import Polysemy
import Data.Id (ConvId, ScheduledJobId, TeamId)
import Wire.API.Jobs
import Wire.JobSubsystem.Recurring (RecurringJobRunnerConfig)

type CleanupAction = IO ()

data JobSubsystemConfig = JobSubsystemConfig
  { jobSubsystemArbiterConnStr :: ByteString.ByteString,
    jobSubsystemSchemaName :: Text
  }

type JobRunnerConfig = RecurringJobRunnerConfig ScheduledJobsRegistry MeetingsCleanupJob

data JobSubsystem m a where
  RegisterJob :: ScheduledJob -> JobSubsystem m ()
  CancelJob :: ScheduledJobId -> JobSubsystem m ()
  CancelJobsByTeamAndKind :: TeamId -> ScheduledJobKind -> JobSubsystem m ()
  FindJobById :: ScheduledJobId -> JobSubsystem m (Maybe ScheduledJob)
  FindJobsByTeamAndKind :: TeamId -> ScheduledJobKind -> JobSubsystem m [ScheduledJob]
  FindJobsByConversationId :: ConvId -> JobSubsystem m [ScheduledJob]
  ScheduleAdminlessDeletionJob :: TeamId -> Maybe ConvId -> UTCTime -> JobSubsystem m ScheduledJob
  StartJobRunner :: JobRunnerConfig -> JobSubsystem m CleanupAction

makeSem ''JobSubsystem

registerAdminlessDeletionJob ::
  (Member JobSubsystem r) =>
  TeamId ->
  Maybe ConvId ->
  UTCTime ->
  Sem r ScheduledJob
registerAdminlessDeletionJob = scheduleAdminlessDeletionJob
