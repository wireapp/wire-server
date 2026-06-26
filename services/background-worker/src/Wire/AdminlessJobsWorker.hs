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
-- You should have received a copy of the GNU Affero General Public License
-- along with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.AdminlessJobsWorker
  ( runAdminlessDeletionJob,
    runAdminlessReminderJob,
  )
where

import Arbiter.Core.Job.Types (JobRead, notVisibleUntil, payload)
import Data.Id (ConvId, RequestId (..), TeamId)
import Data.List qualified as List
import Data.Qualified (toLocalUnsafe)
import Data.Time.Clock (UTCTime)
import Imports
import Polysemy
import System.Logger qualified as Log
import Wire.API.Jobs (AdminlessDeletionJob (..), AdminlessReminderJob (..), ScheduledJob (..), ScheduledJobKind (..))
import Wire.BackgroundWorker.Env (AppT, Env (..))
import Wire.ConversationSubsystem
import Wire.Effects (runBackgroundWorkerEffects)
import Wire.ExternalAccess.External (ExtEnv)
import Wire.JobStore (JobStore, deleteJob, findJobsByConversationId)

runAdminlessDeletionJob :: ExtEnv -> JobRead AdminlessDeletionJob -> AppT IO ()
runAdminlessDeletionJob extEnv job = do
  env <- ask
  let jobPayload = payload job
  Log.info env.logger $
    Log.msg (Log.val "Running adminless deletion job")
      . Log.field "team_id" (show (adminlessDeletionJobTeamId jobPayload))
      . Log.field "conversation_id" (show (adminlessDeletionJobConversationId jobPayload))
  result <-
    liftIO $
      runBackgroundWorkerEffects env extEnv (RequestId "adminless-deletion") Nothing $
        do
          internalDeleteLocalAdminlessGroup
            (toLocalUnsafe env.federationDomain (adminlessDeletionJobOrigUserId jobPayload))
            (toLocalUnsafe env.federationDomain (adminlessDeletionJobConversationId jobPayload))
          cleanupScheduledJob
            AdminlessDeletion
            (adminlessDeletionJobTeamId jobPayload)
            (adminlessDeletionJobConversationId jobPayload)
            (notVisibleUntil job)
  either (liftIO . fail . show) pure result

runAdminlessReminderJob :: ExtEnv -> JobRead AdminlessReminderJob -> AppT IO ()
runAdminlessReminderJob extEnv job = do
  env <- ask
  let jobPayload = payload job
  Log.info env.logger $
    Log.msg (Log.val "Running adminless reminder job")
      . Log.field "team_id" (show (adminlessReminderJobTeamId jobPayload))
      . Log.field "conversation_id" (show (adminlessReminderJobConversationId jobPayload))
      . Log.field "days_until_deletion" (show (adminlessReminderJobDaysUntilDeletion jobPayload))
  result <-
    liftIO $
      runBackgroundWorkerEffects env extEnv (RequestId "adminless-reminder") Nothing $
        do
          internalNotifyAdminlessReminder
            (toLocalUnsafe env.federationDomain (adminlessReminderJobOrigUserId jobPayload))
            (toLocalUnsafe env.federationDomain (adminlessReminderJobConversationId jobPayload))
            (adminlessReminderJobDaysUntilDeletion jobPayload)
          cleanupScheduledJob
            AdminlessReminder
            (adminlessReminderJobTeamId jobPayload)
            (adminlessReminderJobConversationId jobPayload)
            (notVisibleUntil job)
  either (liftIO . fail . show) pure result

cleanupScheduledJob ::
  (Member JobStore r) =>
  ScheduledJobKind ->
  TeamId ->
  ConvId ->
  Maybe UTCTime ->
  Sem r ()
cleanupScheduledJob _ _ _ Nothing = pure ()
cleanupScheduledJob kind teamId convId (Just scheduledFor) = do
  jobs <- findJobsByConversationId convId
  let matchingJobs =
        List.filter
          ( \job ->
              job.scheduledJobKind == kind
                && job.scheduledJobTeamId == teamId
                && job.scheduledJobConversationId == Just convId
                && job.scheduledJobScheduledFor == scheduledFor
          )
          jobs
  traverse_ (deleteJob . scheduledJobId) matchingJobs
