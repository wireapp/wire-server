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

module Wire.BackgroundJobsPublisher.RabbitMQ where

import Data.Aeson qualified as Aeson
import Data.Id (JobId, RequestId (..), idToText)
import Data.Text.Encoding qualified as T
import Imports
import Network.AMQP qualified as Q
import Polysemy
import Wire.API.BackgroundJobs
import Wire.BackgroundJobsPublisher (BackgroundJobsPublisher (..))

interpretBackgroundJobsPublisherRabbitMQ ::
  (Member (Embed IO) r) =>
  RequestId ->
  MVar Q.Channel ->
  InterpreterFor BackgroundJobsPublisher r
interpretBackgroundJobsPublisherRabbitMQ requestId channelMVar =
  interpret $ \case
    PublishJob jobId jobPayload -> do
      channel <- readMVar channelMVar
      publishJob requestId channel jobId jobPayload

publishJob ::
  ( Member (Embed IO) r
  ) =>
  RequestId ->
  Q.Channel ->
  JobId ->
  JobPayload ->
  Sem r ()
publishJob requestId channel jobId jobPayload = do
  let job =
        Job
          { payload = jobPayload,
            jobId = jobId,
            requestId = requestId
          }
      msg =
        Q.newMsg
          { Q.msgBody = Aeson.encode job,
            Q.msgContentType = Just "application/json",
            Q.msgID = Just (idToText job.jobId),
            Q.msgCorrelationID = Just $ T.decodeUtf8 job.requestId.unRequestId
          }

  liftIO $ do
    ensureBackgroundJobsQueue channel
    -- Passing "" for `exchangeName` publishes to the default exchange in RabbitMQ.
    -- The default exchange routes directly to the queue whose name equals the `routingKey`.
    void $ Q.publishMsg channel "" backgroundJobsRoutingKey msg
