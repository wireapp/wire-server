module Wire.BackgroundJobsPublisher.RabbitMQ where

import Data.Aeson qualified as Aeson
import Data.Id (JobId, RequestId (..), idToText)
import Data.Text.Encoding qualified as T
import Imports
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Input
import Wire.API.BackgroundJobs
import Wire.BackgroundJobsPublisher (BackgroundJobsPublisher (..))
import Wire.BackgroundJobsPublisher.Null (interpretBackgroundJobsPublisherNoConfig)

interpretBackgroundJobsPublisherRabbitMQOptional ::
  ( Member (Embed IO) r
  ) =>
  RequestId ->
  Maybe (MVar Q.Channel) ->
  InterpreterFor BackgroundJobsPublisher r
interpretBackgroundJobsPublisherRabbitMQOptional requestId =
  \case
    Nothing -> interpretBackgroundJobsPublisherNoConfig
    Just channelRef ->
      runInputSem (readMVar channelRef)
        . interpretBackgroundJobsPublisherRabbitMQ requestId
        . raiseUnder

interpretBackgroundJobsPublisherRabbitMQ ::
  ( Member (Embed IO) r,
    Member (Input Q.Channel) r
  ) =>
  RequestId ->
  InterpreterFor BackgroundJobsPublisher r
interpretBackgroundJobsPublisherRabbitMQ requestId =
  interpret $ \case
    PublishJob jobId jobPayload -> do
      channel <- input
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
