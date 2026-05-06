-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.BackgroundWorker.Jobs.Registry
  ( dispatchJob,
  )
where

import Imports
import Wire.API.BackgroundJobs (Job (..))
import Wire.BackgroundJobsPublisher.RabbitMQ (interpretBackgroundJobsPublisherRabbitMQ)
import Wire.BackgroundJobsRunner (runJob)
import Wire.BackgroundJobsRunner.Interpreter hiding (runJob)
import Wire.BackgroundWorker.Env (AppT, Env (..))
import Wire.Effects
import Wire.ExternalAccess.External

dispatchJob :: Job -> AppT IO (Either Text ())
dispatchJob job = do
  env <- ask @Env
  let disableTlsV1 = True
  extEnv <- liftIO (initExtEnv disableTlsV1)
  liftIO
    $ runBackgroundWorkerEffects env extEnv job.requestId (Just job.jobId)
      . interpretBackgroundJobsPublisherRabbitMQ job.requestId env.amqpJobsPublisherChannel
      . interpretBackgroundJobsRunner
    $ runJob job
