{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

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

startPushingNotifications ::
  Q.Channel ->
  Domain ->
  AppT IO Q.ConsumerTag
startPushingNotifications chan domain = do
  lift $ ensureQueue chan domain
  QL.consumeMsgs chan (routingKey domain) Q.Ack (pushNotification domain)

-- | This class exists to help with testing, making the envelope in unit test is
-- too difficult. So we use fake envelopes in the unit tests.
class RabbitMQEnvelope e where
  ack :: e -> IO ()
  reject :: e -> Bool -> IO ()

instance RabbitMQEnvelope Q.Envelope where
  ack = Q.ackEnv
  reject = Q.rejectEnv

pushNotification :: RabbitMQEnvelope e => Domain -> (Q.Message, e) -> AppT IO ()
pushNotification targetDomain (msg, envelope) = do
  -- Jittered exponential backoff with 10ms as starting delay and 300s as max
  -- delay. When 300s is reached, every retry will happen after 300s.
  --
  -- FUTUREWORK: Pull these numbers into config
  -- Limit retries to a max of 30 seconds, as this is what Kubernetes is going
  -- to give us for a SIGTERM shutdown notice by default. If we take longer than
  -- that, it will SIGKILL the pod and there is nothing we can do to stop that.
  -- Maximum delay of 30 seconds
  -- Jitter backoff
  -- 30 second maximum delay on backoff jitter.
  -- TODO: This capDelay is no longer needed.
  -- If we fail after this, the notification won't be ACKed, and will be redelivered
  -- by RabbitMQ for another attempt.
  let policy = limitRetriesByCumulativeDelay (30 * 1_000_000) $
        capDelay 30_000_000 $
        fullJitterBackoff 10000
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
  -- TODO: DANGEROUS!
  -- This can make the program unresponsive and unkillable outside of SIGKILL.
  -- It needs to be used for _short_ running code, and can easily cause problems
  -- for other code running on the same thread.
  UnliftIO.uninterruptibleMask_ $ recovering policy handlers $ const go
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
startPusher :: IORef (Map Domain Q.ConsumerTag) -> Q.Channel -> AppT IO ()
startPusher consumersRef chan = UnliftIO.handle (\e@AsyncCancelled -> cleanup e) $ do
  -- This ensures that we receive notifications 1 by 1 which ensures they are
  -- delivered in order.
  markAsWorking BackendNotificationPusher
  lift $ Q.qos chan 0 1 False
  BackendNotificationPusherOpts {..} <- asks (.backendNotificationPusher)
  forever $ do
    remoteDomains <- getRemoteDomains
    mapM_ (ensureConsumer consumersRef chan) remoteDomains
    threadDelay (1_000_000 * remotesRefreshInterval)
  where
    -- Close channels when the async thread is cancelled.
    -- The exceptions used by `amqp` to cancel threads will
    -- be caught/masked by workers so they can finish their
    -- current task before dying. We need an IORef holding
    -- references to these consumers so we can cancel them
    -- explicitly.
    cleanup :: AsyncCancelled -> AppT IO ()
    cleanup e = do
      env <- ask
      let l = logger env
      Log'.info l $ Log'.msg (Log'.val "AsyncCancelled exception caught")
      consumers <- readIORef consumersRef
      for_ (Map.elems consumers) $ \conn -> do
        Log'.info l $ Log'.field "Cancelling consumer" $ show conn
        liftIO $ Q.cancelConsumer chan conn
      -- Close the channel. Using this function will ensure
      -- that `connectWithRetries` doesn't reopen it.
      -- Additionally the bracket pattern used in it will
      -- close the connection for us.
      Log'.info l $ Log'.msg (Log'.val "Closing channel")
      liftIO $ Q.closeChannel chan
      throwM e

ensureConsumer :: IORef (Map Domain Q.ConsumerTag) -> Q.Channel -> Domain -> AppT IO ()
ensureConsumer consumers chan domain = do
  consumerExists <- Map.member domain <$> readIORef consumers
  unless consumerExists $ do
    Log.info $ Log.msg (Log.val "Starting consumer") . Log.field "domain" (domainText domain)
    tag <- startPushingNotifications chan domain
    oldTag <- atomicModifyIORef consumers $ \c -> (Map.insert domain tag c, Map.lookup domain c)
    liftIO $ forM_ oldTag $ Q.cancelConsumer chan

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
      clearRefs = atomicWriteIORef consumersRef mempty
  liftIO $ openConnectionWithRetries env.logger (demoteOpts rabbitmqOpts) $
    RabbitMqHooks
      { onNewChannel = \chan -> runAppT env $
          -- This worker catches AsyncCancelled exceptions
          -- and will gracefully shutdown the channel after
          -- completing it's current task. The exception handling
          -- in `openConnectionWithRetries` won't open a new
          -- connection on an explicit close call.
          startPusher consumersRef chan,
        onChannelException = \_ -> do
          clearRefs
          runAppT env $ markAsNotWorking BackendNotificationPusher,
        onConnectionClose = do
          clearRefs
          runAppT env $ markAsNotWorking BackendNotificationPusher
      }
