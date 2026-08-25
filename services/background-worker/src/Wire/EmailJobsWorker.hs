-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.EmailJobsWorker
  ( runSendEmailJob,
  )
where

import Arbiter.Core.Exceptions (throwRetryable)
import Arbiter.Core.Job.Types (JobRead, notVisibleUntil, payload)
import Imports
import System.Logger qualified as Log
import Wire.API.Jobs (SendEmailJobPayload (..))
import Wire.BackgroundWorker.Env (AppT, Env (..))
import Wire.Effects (runBackgroundWorkerEffects)
import Wire.EmailSending (sendMail)
import Wire.EmailSending.Queueing (fromSerializableMail)
import Wire.ExternalAccess.External (ExtEnv)

-- | Send one outbound email queued by brig on the Arbiter @emails@ queue.
--
-- The mail record is not trusted: a payload that fails 'fromSerializableMail'
-- is malformed (or adversarial) and is rejected with a warning instead of
-- retried. A failing send surfaces as @'Left' 'Text'@ from
-- 'runBackgroundWorkerEffects' and is rethrown as retryable so Arbiter's
-- bounded retry/backoff (and, eventually, the DLQ) applies.
runSendEmailJob :: ExtEnv -> JobRead SendEmailJobPayload -> AppT IO ()
runSendEmailJob extEnv job = do
  env <- ask
  Log.debug env.logger $
    Log.msg (Log.val "Running send-email job")
      . Log.field "request_id" (show job.payload.sendEmailJobRequestId)
      . Log.field "scheduled_for" (show job.notVisibleUntil)
  case fromSerializableMail job.payload.sendEmailJobMail of
    Left err ->
      Log.warn env.logger $
        Log.msg (Log.val "Rejecting malformed email job")
          . Log.field "request_id" (show job.payload.sendEmailJobRequestId)
          . Log.field "error" err
    Right mail -> do
      result <- liftIO $ runBackgroundWorkerEffects env extEnv job.payload.sendEmailJobRequestId Nothing $ sendMail mail
      either (liftIO . throwRetryable) pure result
