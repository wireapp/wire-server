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
    JobWorkerHandlers (..),
    JobWorkersConfig (..),
    scheduleAdminlessDeletionJob,
    scheduleAdminlessReminderJob,
    startJobWorkers,
  )
where

import Arbiter.Core.Job.Types (JobRead)
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Pool qualified as Pool
import Data.Qualified
import Data.Time.Clock (UTCTime)
import Hasql.Connection qualified as Hasql
import Imports
import Polysemy
import Wire.API.Jobs
import Wire.JobSubsystem.Workers

type CleanupAction = IO ()

data JobSubsystemConfig = JobSubsystemConfig
  { jobSubsystemArbiterPool :: Pool.Pool Hasql.Connection,
    jobSubsystemSchemaName :: Text
  }

data JobWorkersConfig = JobWorkersConfig
  { recurringJobRunnerConfig :: RecurringJobRunnerConfig ScheduledJobsRegistry,
    adminlessDeletionJobRunnerConfig :: OneOffJobRunnerConfig ScheduledJobsRegistry AdminlessDeletionJob,
    adminlessReminderJobRunnerConfig :: OneOffJobRunnerConfig ScheduledJobsRegistry AdminlessReminderJob
  }

data JobWorkerHandlers = JobWorkerHandlers
  { recurringJobRunnerRunJob :: MeetingsCleanupJob -> IO (),
    adminlessDeletionJobRunnerRunJob :: JobRead AdminlessDeletionJob -> IO (),
    adminlessReminderJobRunnerRunJob :: JobRead AdminlessReminderJob -> IO ()
  }

data JobSubsystem m a where
  ScheduleAdminlessDeletionJob :: Local UserId -> TeamId -> ConvId -> UTCTime -> JobSubsystem m ()
  ScheduleAdminlessReminderJob :: Local UserId -> TeamId -> ConvId -> UTCTimeMillis -> UTCTime -> JobSubsystem m ()
  StartJobWorkers :: JobWorkersConfig -> JobWorkerHandlers -> JobSubsystem m CleanupAction

makeSem ''JobSubsystem
