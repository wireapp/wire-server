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
  ( runAdminlessSetupJob,
    runAdminlessDeletionJob,
    runAdminlessReminderJob,
  )
where

import Arbiter.Core.Exceptions (throwRetryable)
import Arbiter.Core.Job.Types (JobRead, notVisibleUntil, payload)
import Data.Qualified (toLocalUnsafe)
import Imports
import System.Logger qualified as Log
import Wire.API.Jobs (AdminlessDeletionJob (..), AdminlessReminderJob (..), AdminlessSetupJob (..))
import Wire.BackgroundWorker.Env (AppT, Env (..))
import Wire.ConversationSubsystem
import Wire.Effects (runBackgroundWorkerEffects)
import Wire.ExternalAccess.External (ExtEnv)

runAdminlessSetupJob :: ExtEnv -> JobRead AdminlessSetupJob -> AppT IO ()
runAdminlessSetupJob extEnv job = do
  env <- ask
  Log.debug env.logger $
    Log.msg (Log.val "Running adminless setup job")
      . Log.field "team_id" (show job.payload.adminlessSetupJobTeamId)
      . Log.field "orig_user_id" (show job.payload.adminlessSetupJobOrigUserId)
      . Log.field "request_id" (show job.payload.adminlessSetupJobRequestId)
      . Log.field "scheduled_for" (show job.notVisibleUntil)
  result <-
    liftIO $
      runBackgroundWorkerEffects env extEnv job.payload.adminlessSetupJobRequestId Nothing $
        do
          setupAdminlessGroupsCleanup
            (toLocalUnsafe env.federationDomain <$> job.payload.adminlessSetupJobOrigUserId)
            job.payload.adminlessSetupJobTeamId
          Log.debug env.logger $
            Log.msg (Log.val "Adminless setup job finished")
              . Log.field "team_id" (show job.payload.adminlessSetupJobTeamId)
  either (liftIO . throwRetryable) pure result

runAdminlessDeletionJob :: ExtEnv -> JobRead AdminlessDeletionJob -> AppT IO ()
runAdminlessDeletionJob extEnv job = do
  env <- ask
  Log.debug env.logger $
    Log.msg (Log.val "Running adminless deletion job")
      . Log.field "team_id" (show job.payload.adminlessDeletionJobTeamId)
      . Log.field "conversation_id" (show job.payload.adminlessDeletionJobConversationId)
      . Log.field "orig_user_id" (show job.payload.adminlessDeletionJobOrigUserId)
      . Log.field "request_id" (show job.payload.adminlessDeletionJobRequestId)
      . Log.field "scheduled_for" (show job.notVisibleUntil)
  result <-
    liftIO $
      runBackgroundWorkerEffects env extEnv job.payload.adminlessDeletionJobRequestId Nothing $
        do
          Log.debug env.logger $
            Log.msg (Log.val "Adminless deletion job: invoking conversation delete")
              . Log.field "team_id" (show job.payload.adminlessDeletionJobTeamId)
              . Log.field "conversation_id" (show job.payload.adminlessDeletionJobConversationId)
          internalDeleteLocalAdminlessGroup
            (toLocalUnsafe env.federationDomain <$> job.payload.adminlessDeletionJobOrigUserId)
            (toLocalUnsafe env.federationDomain job.payload.adminlessDeletionJobConversationId)
          Log.debug env.logger $
            Log.msg (Log.val "Adminless deletion job finished")
              . Log.field "team_id" (show job.payload.adminlessDeletionJobTeamId)
              . Log.field "conversation_id" (show job.payload.adminlessDeletionJobConversationId)
  either (liftIO . throwRetryable) pure result

runAdminlessReminderJob :: ExtEnv -> JobRead AdminlessReminderJob -> AppT IO ()
runAdminlessReminderJob extEnv job = do
  env <- ask
  Log.debug env.logger $
    Log.msg (Log.val "Running adminless reminder job")
      . Log.field "team_id" (show job.payload.adminlessReminderJobTeamId)
      . Log.field "conversation_id" (show job.payload.adminlessReminderJobConversationId)
      . Log.field "request_id" (show job.payload.adminlessReminderJobRequestId)
      . Log.field "deletion_scheduled_for" (show job.payload.adminlessReminderJobDeletionScheduledFor)
  result <-
    liftIO $
      runBackgroundWorkerEffects env extEnv job.payload.adminlessReminderJobRequestId Nothing $
        do
          internalNotifyAdminlessReminder
            (toLocalUnsafe env.federationDomain <$> job.payload.adminlessReminderJobOrigUserId)
            (toLocalUnsafe env.federationDomain job.payload.adminlessReminderJobConversationId)
            job.payload.adminlessReminderJobDeletionScheduledFor
          Log.debug env.logger $
            Log.msg (Log.val "Adminless reminder job finished")
              . Log.field "team_id" (show job.payload.adminlessReminderJobTeamId)
              . Log.field "conversation_id" (show job.payload.adminlessReminderJobConversationId)
  either (liftIO . throwRetryable) pure result
