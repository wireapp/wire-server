{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE BlockArguments #-}

module Wire.BackendNotificationPusher where

import Control.Monad.Catch
import Control.Retry
import qualified Data.Aeson as A
import Data.Domain
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Imports
import qualified Network.AMQP as Q
import Network.AMQP.Extended
import qualified Network.AMQP.Lifted as QL
import Network.RabbitMqAdmin
import Prometheus
import qualified System.Logger.Class as Log
import Wire.API.Federation.BackendNotifications
import Wire.API.Federation.Client
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Options
import UnliftIO
import qualified System.Logger as Log'
import Network.AMQP (cancelConsumer)

startPushingNotifications ::
  MVar () ->
  Q.Channel ->
  Domain ->
  AppT IO Q.ConsumerTag
startPushingNotifications mvar chan domain = do
  lift $ ensureQueue chan domain
  QL.consumeMsgs chan (routingKey domain) Q.Ack (pushNotification mvar domain)

-- | This class exists to help with testing, making the envelope in unit test is
-- too difficult. So we use fake envelopes in the unit tests.
class RabbitMQEnvelope e where
  ack :: e -> IO ()
  reject :: e -> Bool -> IO ()

instance RabbitMQEnvelope Q.Envelope where
  ack = Q.ackEnv
  reject = Q.rejectEnv

pushNotification :: RabbitMQEnvelope e => MVar () -> Domain -> (Q.Message, e) -> AppT IO ()
pushNotification mvar targetDomain (msg, envelope) = do
  -- Jittered exponential backoff with 10ms as starting delay and 20s as maximum
  -- cumulative delay. When 20s is cumulatively reached, the request will fail.
  --
  -- FUTUREWORK: Pull these numbers into config.
  -- FUTUREWORK: Is this futurework still needed given the new changes?
  -- Limit retries to a max of 20 seconds, as this is what Kubernetes is going
  -- to give us for a SIGTERM shutdown notice by default. If we take longer than
  -- that, it will SIGKILL the pod and there is nothing we can do to stop that.
  -- If we fail after this, the notification will be NACKed, and will be redelivered
  -- by RabbitMQ for another attempt.
  let policy = limitRetriesByCumulativeDelay (20 * 1_000_000) $ fullJitterBackoff 10000
      logErrr willRetry (SomeException e) rs = do
        Log.err $
          Log.msg (Log.val "Exception occurred while pushing notification")
            . Log.field "error" (displayException e)
            . Log.field "domain" (domainText targetDomain)
            . Log.field "willRetry" willRetry
            . Log.field "retryCount" rs.rsIterNumber
        metrics <- asks backendNotificationMetrics
        withLabel metrics.errorCounter (domainText targetDomain) incCounter
        withLabel metrics.stuckQueuesGauge (domainText targetDomain) (flip setGauge 1)
      skipChanThreadKilled _ = Handler $ \(_ :: Q.ChanThreadKilledException) -> pure False
      handlers =
        skipAsyncExceptions
          <> [ skipChanThreadKilled,
               logRetries (const $ pure True) logErrr
             ]
  -- Ensure that the mvars are reset correctly.
  UnliftIO.bracket_ (takeMVar mvar) (putMVar mvar ()) $ do
    -- Ensure that envelopes are acked if recovering still fails.
    -- Otherwise Rabbit is going to think that we are still processing
    -- it and won't send another message as we have set up the channel
    -- to deliver a single message at a time and to wait for confirmation.
    UnliftIO.onException
      (recovering policy handlers (const go))
      -- Reject for redelivery. This is needed for the following reasons
      -- 1) We have a strict time limit when kubernetes sends SIGTERM. We need to stay within that.
      -- 2) Just because we failed to push the notification _now_ doesn't mean we won't be able to
      --    in a few minutes. Requeuing the message will give us another go at it, and the next pod
      --    to gain single exclusive consumer will get this message.
      $ lift $ reject envelope True
  where
    go :: AppT IO ()
    go = case A.eitherDecode @BackendNotification (Q.msgBody msg) of
      Left e -> do
        Log.err $
          Log.msg (Log.val "Failed to parse notification, the notification will be ignored")
            . Log.field "domain" (domainText targetDomain)
            . Log.field "error" e

        -- FUTUREWORK: This rejects the message without any requeueing. This is
        -- dangerous as it could happen that a new type of notification is
        -- introduced and an old instance of this worker is running, in which case
        -- the notification will just get dropped. On the other hand not dropping
        -- this message blocks the whole queue. Perhaps there is a better way to
        -- deal with this.
        lift $ reject envelope False
      Right notif -> do
        ceFederator <- asks (.federatorInternal)
        ceHttp2Manager <- asks http2Manager
        let ceOriginDomain = notif.ownDomain
            ceTargetDomain = targetDomain
            fcEnv = FederatorClientEnv {..}
        liftIO $ either throwM pure =<< sendNotification fcEnv notif.targetComponent notif.path notif.body
        lift $ ack envelope
        metrics <- asks backendNotificationMetrics
        withLabel metrics.pushedCounter (domainText targetDomain) incCounter
        withLabel metrics.stuckQueuesGauge (domainText targetDomain) (flip setGauge 0)

-- FUTUREWORK: Recosider using 1 channel for many consumers. It shouldn't matter
-- for a handful of remote domains.
-- Consumers is passed in explicitly so that cleanup code has a reference to the consumer tags.
startPusher :: IORef (Map Domain (Q.ConsumerTag, MVar ())) -> Q.Channel -> AppT IO ()
startPusher consumersRef chan = do
  -- This ensures that we receive notifications 1 by 1 which ensures they are
  -- delivered in order.
  markAsWorking BackendNotificationPusher
  lift $ Q.qos chan 0 1 False
  BackendNotificationPusherOpts {..} <- asks (.backendNotificationPusher)
  forever $ do
    remoteDomains <- getRemoteDomains
    mapM_ (ensureConsumer consumersRef chan) remoteDomains
    threadDelay (1_000_000 * remotesRefreshInterval)

ensureConsumer :: IORef (Map Domain (Q.ConsumerTag, MVar ())) -> Q.Channel -> Domain -> AppT IO ()
ensureConsumer consumers chan domain = do
  consumerExists <- Map.member domain <$> readIORef consumers
  unless consumerExists $ do
    Log.info $ Log.msg (Log.val "Starting consumer") . Log.field "domain" (domainText domain)
    -- Build an MVar for the consumer. This is used as a flag for when a consumer callback is running.
    mvar <- newMVar ()
    tag <- startPushingNotifications mvar chan domain
    oldTag <- atomicModifyIORef consumers $ \c -> (Map.insert domain (tag, mvar) c, Map.lookup domain c)
    liftIO $ forM_ oldTag $ Q.cancelConsumer chan . fst

getRemoteDomains :: AppT IO [Domain]
getRemoteDomains = do
  -- Jittered exponential backoff with 10ms as starting delay and 60s as max
  -- cumulative delay. When this is reached, the operation fails.
  --
  -- FUTUREWORK: Pull these numbers into config
  let policy = limitRetriesByCumulativeDelay 60_000_000 $ fullJitterBackoff 10000
      logErrr willRetry (SomeException e) rs =
        Log.err $
          Log.msg (Log.val "Exception occurred while refreshig domains")
            . Log.field "error" (displayException e)
            . Log.field "willRetry" willRetry
            . Log.field "retryCount" rs.rsIterNumber
      handlers =
        skipAsyncExceptions
          <> [logRetries (const $ pure True) logErrr]
  recovering policy handlers $ const go
  where
    go :: AppT IO [Domain]
    go = do
      client <- asks rabbitmqAdminClient
      vhost <- asks rabbitmqVHost
      queues <- liftIO $ listQueuesByVHost client vhost
      let notifQueuesSuffixes = mapMaybe (\q -> Text.stripPrefix "backend-notifications." q.name) queues
      catMaybes <$> traverse (\d -> either (\e -> logInvalidDomain d e >> pure Nothing) (pure . Just) $ mkDomain d) notifQueuesSuffixes
    logInvalidDomain d e =
      Log.warn $
        Log.msg (Log.val "Found invalid domain in a backend notifications queue name")
          . Log.field "queue" ("backend-notifications." <> d)
          . Log.field "error" e

startWorker :: RabbitMqAdminOpts -> AppT IO ()
startWorker rabbitmqOpts = do
  env <- ask
  -- AsyncCancelled is used when our `Async ()` is `cancel`led.
  -- This is used in the POSIX signal handlers, so we should catch it
  -- here and clean up our processes, letting them finish if we can.
  -- Passed into running threads so we can cancel consumers and allow
  -- amqp to cleanly finish before we stop the service.
  consumersRef <- newIORef mempty
  let -- cleanup the refs when channels die
      -- This is so we aren't trying to close consumers
      -- that don't exist when the service is shutdown.
      l = logger env
      clearRefs = do
        atomicWriteIORef consumersRef mempty
      cleanup chan = do
        readIORef consumersRef >>= traverse_ \(consumer, mvar) -> do
          Log'.info l $ Log.msg (Log.val "Cancelling") . Log.field "Consumer" consumer
          -- Remove the consumer from the channel so it isn't called again
          cancelConsumer chan consumer
          -- Take from the mvar. This will only unblock when the consumer callback isn't running.
          -- This allows us to wait until the currently running tasks are completed, and new ones
          -- won't be scheduled because we've already removed the callback from the channel.
          Log'.info l $ Log.msg $ Log.val "Taking MVar. Waiting for current operation to finish"
          takeMVar mvar
        -- Close the channel. `extended` will then close the connection, flushing messages to the server.
        Log'.info l $ Log.msg $ Log.val "Closing RabbitMQ channel"
        Q.closeChannel chan
  liftIO $ openConnectionWithRetries env.logger (demoteOpts rabbitmqOpts) $
    RabbitMqHooks
      -- This worker catches AsyncCancelled exceptions
      -- and will gracefully shutdown the channel after
      -- completing it's current task. The exception handling
      -- in `openConnectionWithRetries` won't open a new
      -- connection on an explicit close call.
      { onNewChannel = \chan -> Control.Monad.Catch.handle (\AsyncCancelled -> cleanup chan) $
          runAppT env $ startPusher consumersRef chan,
        onChannelException = \_ -> do
          clearRefs
          runAppT env $ markAsNotWorking BackendNotificationPusher,
        onConnectionClose = do
          clearRefs
          runAppT env $ markAsNotWorking BackendNotificationPusher
      }
