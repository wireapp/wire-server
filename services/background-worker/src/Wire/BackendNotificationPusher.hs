{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

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
import qualified System.Logger as Log'
import qualified System.Logger.Class as Log
import UnliftIO
import Wire.API.Federation.BackendNotifications
import Wire.API.Federation.Client
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Options

startPushingNotifications ::
  MVar () ->
  Q.Channel ->
  Domain ->
  AppT IO Q.ConsumerTag
startPushingNotifications runningFlag chan domain = do
  lift $ ensureQueue chan domain
  QL.consumeMsgs chan (routingKey domain) Q.Ack (pushNotification runningFlag domain)

-- | This class exists to help with testing, making the envelope in unit test is
-- too difficult. So we use fake envelopes in the unit tests.
class RabbitMQEnvelope e where
  ack :: e -> IO ()
  reject :: e -> Bool -> IO ()

instance RabbitMQEnvelope Q.Envelope where
  ack = Q.ackEnv
  reject = Q.rejectEnv

pushNotification :: RabbitMQEnvelope e => MVar () -> Domain -> (Q.Message, e) -> AppT IO ()
pushNotification runningFlag targetDomain (msg, envelope) = do
  -- Jittered exponential backoff with 10ms as starting delay and 300s as max
  -- delay. When 300s is reached, every retry will happen after 300s.
  --
  -- FUTUREWORK: Pull these numbers into config.s
  let policy = capDelay 300_000_000 $ fullJitterBackoff 10000
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
  -- The revcovering policy where it can loop forever effectively blocks the consumer thread.
  -- This isn't a problem for single active consumer with single message delivery, however it
  -- does cause problems when trying to deregister consumers from the channel. This is because
  -- the internal mechanism to remove a consumer goes via the same notification handling code
  -- as messages from the Rabbit server. If the thread is tied up in the recovery code we
  -- can't cancel the consumer, and the calling code will block until the cancelation message
  -- can be processed.
  -- Luckily, we can async this loop and carry on as usual due to how we have the channel setup.
  void $
    async $
      recovering policy handlers $
        const $
          -- Ensure that the mvars are reset correctly.
          -- takeMVar also has the nice feature of being a second layer of protection
          -- against lazy thread updates in `amqp`. If this somehow gets called while
          -- we are trying to cleanup workers for a shutdown, this will call will block
          -- and prevent the message from being sent out as we are tearing down resources.
          -- This removes one way that a message might be delivered twice.
          UnliftIO.bracket_ (takeMVar runningFlag) (putMVar runningFlag ()) go
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
    -- The cleanup code that runs when the service receives a SIGTERM or SIGINT will wait on these MVars
    -- to allow current messages to finish processing before we close AMQP connections.
    runningFlag <- newMVar ()
    tag <- startPushingNotifications runningFlag chan domain
    oldTag <- atomicModifyIORef consumers $ \c -> (Map.insert domain (tag, runningFlag) c, Map.lookup domain c)
    -- This isn't strictly nessacary. `unless consumerExists` won't
    -- let us come down this path if there is an old consumer.
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

startWorker :: RabbitMqAdminOpts -> AppT IO (IORef (Maybe Q.Channel), IORef (Map Domain (Q.ConsumerTag, MVar ())))
startWorker rabbitmqOpts = do
  env <- ask
  -- These are used in the POSIX signal handlers, so we need to make
  -- cross thread references that we can use to cancel consumers and
  -- wait for current processing steps to finish.
  chanRef <- newIORef Nothing
  consumersRef <- newIORef mempty
  let -- cleanup the refs when channels die
      -- This is so we aren't trying to close consumers
      -- that don't exist when the service is shutdown.
      l = logger env
      clearRefs = do
        atomicWriteIORef chanRef Nothing
        atomicWriteIORef consumersRef mempty
      cleanup = do
        readIORef chanRef >>= traverse_ \chan -> do
          readIORef consumersRef >>= \m -> for_ (Map.assocs m) \(domain, (consumer, runningFlag)) -> do
            Log'.info l $ Log.msg (Log.val "Cancelling consumer") . Log.field "Domain" domain._domainText
            -- Remove the consumer from the channel so it isn't called again
            Q.cancelConsumer chan consumer
            -- Take from the mvar. This will only unblock when the consumer callback isn't running.
            -- This allows us to wait until the currently running tasks are completed, and new ones
            -- won't be scheduled because we've already removed the callback from the channel.
            -- If, for some reason, a consumer is invoked after us cancelling it, taking this MVar
            -- will block that thread from trying to push out the notification. At this point, we're
            -- relying on Rabbit to requeue the message for us as we won't be able to ACK or NACK it.
            -- This helps prevent message redelivery to endpoint services during the brief window between
            -- receiving a message from rabbit, and the signal handler shutting down the AMQP connection
            -- before notification delivery has finalised.
            Log'.info l $ Log.msg $ Log.val "Taking MVar. Waiting for current operation to finish"
            takeMVar runningFlag
          -- Close the channel. `extended` will then close the connection, flushing messages to the server.
          Log'.info l $ Log.msg $ Log.val "Closing RabbitMQ channel"
          Q.closeChannel chan
  -- We can fire and forget this thread because it keeps respawning itself using the 'onConnectionClosedHandler'.
  void $
    async $
      liftIO $
        openConnectionWithRetries env.logger (demoteOpts rabbitmqOpts) $
          RabbitMqHooks
            { -- The exception handling in `openConnectionWithRetries` won't open a new
              -- connection on an explicit close call.
              onNewChannel = \chan -> do
                atomicWriteIORef chanRef $ pure chan
                runAppT env $ startPusher consumersRef chan,
              onChannelException = \_ -> do
                clearRefs
                runAppT env $ markAsNotWorking BackendNotificationPusher,
              onConnectionClose = do
                clearRefs
                runAppT env $ markAsNotWorking BackendNotificationPusher
            }
  pure (chanRef, consumersRef)
