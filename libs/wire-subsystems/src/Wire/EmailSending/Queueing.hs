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
{-# LANGUAGE TemplateHaskell #-}

-- | Queueing effect for outbound email.
--
-- Producers (brig) enqueue the composing payload ('SendEmailRequest': email
-- type, locale and structured inputs) as a 'SendEmail' job into the Arbiter
-- @emails@ queue (a PostgreSQL table managed by Arbiter). The
-- background-worker composes the actual email (template selection, rendering,
-- MIME building) and performs the send; see "Wire.EmailSending.Composer" and
-- "Wire.EmailJobsWorker".
module Wire.EmailSending.Queueing
  ( EmailQueueing (..),
    queueEmail,
    emailViaQueueInterpreter,
  )
where

import Arbiter.Core qualified as ArbiterCore
import Data.Id (RequestId)
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports
import Polysemy (Embed, InterpreterFor, Member, embed, interpret, makeSem)
import Wire.API.BackgroundJobs.Email (SendEmailRequest)
import Wire.API.Jobs (EmailsJobPayload (SendEmail), JobRegistry, SendEmailJobPayload (..))
import Wire.JobSubsystem.ArbiterAdapter (WireArbiter, mkNewWireArbiterEnv, runWireArbiter)

-- | Effect for enqueueing outbound email as a composing payload. Producers use
-- this instead of 'Wire.EmailSending.SendMail': no rendered mail exists on the
-- producer side.
data EmailQueueing m a where
  QueueEmail :: SendEmailRequest -> EmailQueueing m ()

makeSem ''EmailQueueing

-- | Interpret 'EmailQueueing' by inserting a 'SendEmail' job into the Arbiter
-- @emails@ queue.
--
-- The interpreter is self-contained: it runs Arbiter against the producer's
-- shared PostgreSQL pool, so its only effect requirement is 'Embed' 'IO' and it
-- drops into the producer's effect stack exactly where the old direct-send
-- interpreter sat. The table is created by 'runJobMigrations' (run at startup
-- by every service that schedules or executes jobs).
emailViaQueueInterpreter ::
  (Member (Embed IO) r) =>
  RequestId ->
  HasqlPoolExt.Pool ->
  InterpreterFor EmailQueueing r
emailViaQueueInterpreter requestId pool = interpret \case
  QueueEmail request -> do
    let payload =
          SendEmailJobPayload
            { sendEmailJobRequestId = requestId,
              sendEmailJobRequest = request
            }
        -- Bounded attempts: the send is retried by Arbiter with exponential
        -- backoff, and after these attempts the job is moved to the queue's
        -- dead-letter table.
        job =
          (ArbiterCore.defaultJob (SendEmail payload))
            { ArbiterCore.maxAttempts = Just 3
            }
    embed @IO . void $
      runWireArbiter arbiterEnv $
        ArbiterCore.insertJob @EmailsJobPayload @(WireArbiter JobRegistry) job
  where
    arbiterEnv = mkNewWireArbiterEnv ArbiterCore.defaultSchemaName pool
