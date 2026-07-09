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
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Qualified
import Data.Time
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports
import Polysemy
import Polysemy.Input (input)
import Wire.API.Jobs
import Wire.JobSubsystem (CleanupAction, JobSubsystem (..), JobSubsystemConfig (..), JobWorkerHandlers (..), JobWorkersConfig (..))
import Wire.JobSubsystem.ArbiterAdapter (WireArbiter, WireArbiterEnv (..), runWireArbiter)
import Wire.JobSubsystem.Workers (runOneOffJobRunner, runRecurringJobRunner)
import Wire.Postgres (PGConstraints)

runJobWorkers :: HasqlPoolExt.Pool -> JobWorkersConfig -> JobWorkerHandlers -> IO CleanupAction
runJobWorkers pool JobWorkersConfig {..} JobWorkerHandlers {..} = do
  cleanupRecurring <- runRecurringJobRunner @ScheduledJobsRegistry pool recurringJobRunnerConfig recurringJobRunnerRunJob
  cleanupDeletion <- runOneOffJobRunner @ScheduledJobsRegistry pool adminlessDeletionJobRunnerConfig adminlessDeletionJobRunnerRunJob
  cleanupReminder <- runOneOffJobRunner @ScheduledJobsRegistry pool adminlessReminderJobRunnerConfig adminlessReminderJobRunnerRunJob
  pure $ cleanupRecurring >> cleanupDeletion >> cleanupReminder

interpretJobSubsystem ::
  (PGConstraints r) =>
  JobSubsystemConfig ->
  InterpreterFor JobSubsystem r
interpretJobSubsystem conf =
  interpret
    \case
      ScheduleAdminlessDeletionJob lusr tid cid scheduledFor -> scheduleAdminlessDeletionJob conf lusr tid cid scheduledFor
      ScheduleAdminlessReminderJob lusr tid cid deletionScheduledFor scheduledFor -> scheduleAdminlessReminderJob conf lusr tid cid deletionScheduledFor scheduledFor
      StartJobWorkers cfg handlers -> do
        pool <- input
        embed $ runJobWorkers pool cfg handlers

scheduleAdminlessDeletionJob ::
  forall r.
  (PGConstraints r) =>
  JobSubsystemConfig ->
  Local UserId ->
  TeamId ->
  ConvId ->
  UTCTime ->
  Sem r ()
scheduleAdminlessDeletionJob JobSubsystemConfig {..} lusr teamId convId scheduledFor = do
  pool <- input
  let arbiterEnv =
        WireArbiterEnv
          { schemaName = jobSubsystemSchemaName,
            connectionPool = pool,
            activeConn = Nothing,
            transactionDepth = 0,
            preparedStatements = False
          }
  let arbiterJob =
        (ArbiterCore.defaultGroupedJob adminlessDeletionQueueName (AdminlessDeletionJob teamId convId (tUnqualified lusr)))
          { ArbiterCore.notVisibleUntil = Just scheduledFor,
            ArbiterCore.maxAttempts = Just 3
          }
  embed $ void $ runWireArbiter arbiterEnv $ ArbiterCore.insertJob @(WireArbiter ScheduledJobsRegistry) arbiterJob

scheduleAdminlessReminderJob ::
  forall r.
  (PGConstraints r) =>
  JobSubsystemConfig ->
  Local UserId ->
  TeamId ->
  ConvId ->
  UTCTimeMillis ->
  UTCTime ->
  Sem r ()
scheduleAdminlessReminderJob JobSubsystemConfig {..} lusr teamId convId deletionScheduledFor scheduledFor = do
  pool <- input @HasqlPoolExt.Pool
  let arbiterEnv =
        WireArbiterEnv
          { schemaName = jobSubsystemSchemaName,
            connectionPool = pool,
            activeConn = Nothing,
            transactionDepth = 0,
            preparedStatements = False
          }
  let arbiterJob =
        (ArbiterCore.defaultGroupedJob adminlessReminderQueueName (AdminlessReminderJob teamId convId (tUnqualified lusr) deletionScheduledFor))
          { ArbiterCore.notVisibleUntil = Just scheduledFor,
            ArbiterCore.maxAttempts = Just 3
          }
  embed $ void $ runWireArbiter arbiterEnv $ ArbiterCore.insertJob @(WireArbiter ScheduledJobsRegistry) arbiterJob
