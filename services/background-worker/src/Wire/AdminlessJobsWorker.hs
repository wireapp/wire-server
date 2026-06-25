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

module Wire.AdminlessJobsWorker
  ( runAdminlessDeletionJob,
    runAdminlessReminderJob,
  )
where

import Data.Id (RequestId (..))
import Data.Qualified
import Imports
import System.Logger qualified as Log
import Wire.API.Jobs (AdminlessDeletionJob (..), AdminlessReminderJob (..))
import Wire.BackgroundWorker.Env (AppT, Env (..))
import Wire.ConversationSubsystem
import Wire.Effects (runBackgroundWorkerEffects)
import Wire.ExternalAccess.External (ExtEnv)

runAdminlessDeletionJob :: ExtEnv -> AdminlessDeletionJob -> AppT IO ()
runAdminlessDeletionJob extEnv job = do
  env <- ask
  let loc :: a -> Local a
      loc a = toLocalUnsafe env.federationDomain a
  Log.info env.logger $
    Log.msg (Log.val "Running adminless deletion job")
      . Log.field "team_id" (show (adminlessDeletionJobTeamId job))
      . Log.field "conversation_id" (show (adminlessDeletionJobConversationId job))
  result <-
    liftIO $
      runBackgroundWorkerEffects env extEnv (RequestId "adminless-deletion") Nothing $
        internalDeleteLocalAdminlessGroup (loc job.adminlessDeletionJobOrigUserId) (loc job.adminlessDeletionJobConversationId)
  either (liftIO . fail . show) pure result

runAdminlessReminderJob :: AdminlessReminderJob -> AppT IO ()
runAdminlessReminderJob job = do
  env <- ask
  Log.info env.logger $
    Log.msg (Log.val "Running adminless reminder job")
      . Log.field "team_id" (show (adminlessReminderJobTeamId job))
      . Log.field "conversation_id" (show (adminlessReminderJobConversationId job))
