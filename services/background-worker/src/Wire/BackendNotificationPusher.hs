{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.BackendNotificationPusher where

import Control.Arrow
import Control.Monad.Catch
import Control.Retry
import Data.Aeson qualified as A
import Data.Domain
import Data.Id
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Extended
import Network.AMQP.Lifted qualified as QL
import Network.RabbitMqAdmin
import Prometheus
import System.Logger.Class qualified as Log
import UnliftIO
import Wire.API.Federation.API
import Wire.API.Federation.BackendNotifications
import Wire.API.Federation.Client
import Wire.API.Federation.Error
import Wire.API.Federation.Version
import Wire.API.RawJson
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Options
import Wire.BackgroundWorker.Util

startPushingNotifications ::
  MVar () ->
  Q.Channel ->
  Domain ->
  AppT IO Q.ConsumerTag
startPushingNotifications runningFlag chan domain = do
  lift $ ensureQueue chan domain._domainText
  QL.consumeMsgs chan (routingKey domain._domainText) Q.Ack (void . pushNotification runningFlag domain)

pushNotification :: (RabbitMQEnvelope e) => MVar () -> Domain -> (Q.Message, e) -> AppT IO (Async ())
pushNotification runningFlag targetDomain (msg, envelope) = do
  cfg <- asks (.backendNotificationsConfig)
  -- Jittered exponential backoff with 10ms as starting delay and 300s as max
  -- delay. When 300s is reached, every retry will happen after 300s.
  --
  -- FUTUREWORK: Pull these numbers into config.s
  let policy = capDelay cfg.pushBackoffMaxWait $ fullJitterBackoff cfg.pushBackoffMinWait
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
    go = case A.eitherDecode @(PayloadBundle _) (Q.msgBody msg) of
      Left e -> do
        case A.eitherDecode @BackendNotification (Q.msgBody msg) of
          Left eBN -> do
            Log.err $
              Log.msg
                ( Log.val "Cannot parse a queued message as s notification "
                    <> "nor as a bundle; the message will be ignored"
                )
                . Log.field "domain" (domainText targetDomain)
                . Log.field "error-notification" eBN
                . Log.field
                  "error-bundle"
                  e
            -- FUTUREWORK: This rejects the message without any requeueing. This is
            -- dangerous as it could happen that a new type of notification is
            -- introduced and an old instance of this worker is running, in which case
            -- the notification will just get dropped. On the other hand not dropping
            -- this message blocks the whole queue. Perhaps there is a better way to
            -- deal with this.
            lift $ reject envelope False
          Right notif -> do
            -- FUTUREWORK: Drop support for parsing it as a
            -- single notification as soon as we can guarantee
            -- that the message queue does not contain any
            -- 'BackendNotification's anymore.
            ceFederator <- asks (.federatorInternal)
            ceHttp2Manager <- asks http2Manager
            let ceOriginDomain = notif.ownDomain
                ceTargetDomain = targetDomain
                ceOriginRequestId = fromMaybe (RequestId "N/A") notif.requestId
                cveEnv = FederatorClientEnv {..}
                cveVersion = Just V0 -- V0 is assumed for non-versioned queue messages
                fcEnv = FederatorClientVersionedEnv {..}
            sendNotificationIgnoringVersionMismatch fcEnv notif.targetComponent notif.path notif.body
            lift $ ack envelope
            metrics <- asks backendNotificationMetrics
            withLabel metrics.pushedCounter (domainText targetDomain) incCounter
            withLabel metrics.stuckQueuesGauge (domainText targetDomain) (flip setGauge 0)
      Right bundle -> do
        federator <- asks (.federatorInternal)
        manager <- asks http2Manager
        let env =
              FederatorClientEnv
                { ceOriginDomain = ownDomain . NE.head $ bundle.notifications,
                  ceTargetDomain = targetDomain,
                  ceFederator = federator,
                  ceHttp2Manager = manager,
                  ceOriginRequestId =
                    fromMaybe (RequestId "N/A") . (.requestId) . NE.head $ bundle.notifications
                }
        remoteVersions :: Set Int <-
          liftIO
            -- use versioned client with no version set: since we are manually
            -- performing version negotiation, we don't want the client to
            -- negotiate a version for us
            ( runVersionedFederatorClient @'Brig (unversionedEnv env) $
                fedClientIn @'Brig @"api-version" ()
            )
            >>= \case
              Left e -> do
                Log.err $
                  Log.msg (Log.val "Failed to get supported API versions")
                    . Log.field "domain" (domainText targetDomain)
                    . Log.field "error" (displayException e)
                throwM e
              Right vi -> pure . Set.fromList . vinfoSupported $ vi

        -- compute the best usable version in a notification
        let bestVersion = bodyVersions >=> flip latestCommonVersion remoteVersions
        case pairedMaximumOn bestVersion (toList (notifications bundle)) of
          (_, Nothing) ->
            Log.fatal $
              Log.msg (Log.val "No federation API version in common, the notification will be ignored")
                . Log.field "domain" (domainText targetDomain)
          (notif, cveVersion) -> do
            ceFederator <- asks (.federatorInternal)
            ceHttp2Manager <- asks http2Manager
            let ceOriginDomain = notif.ownDomain
                ceTargetDomain = targetDomain
                ceOriginRequestId = fromMaybe (RequestId "N/A") notif.requestId
                cveEnv = FederatorClientEnv {..}
                fcEnv = FederatorClientVersionedEnv {..}
            sendNotificationIgnoringVersionMismatch fcEnv notif.targetComponent notif.path notif.body
            lift $ ack envelope
            metrics <- asks backendNotificationMetrics
            withLabel metrics.pushedCounter (domainText targetDomain) incCounter
            withLabel metrics.stuckQueuesGauge (domainText targetDomain) (flip setGauge 0)

sendNotificationIgnoringVersionMismatch ::
  FederatorClientVersionedEnv ->
  Component ->
  Text ->
  RawJson ->
  AppT IO ()
sendNotificationIgnoringVersionMismatch env comp path body =
  liftIO (sendNotification env comp path body) >>= \case
    Left (FederatorClientVersionNegotiationError v) -> do
      Log.fatal $
        Log.msg (Log.val "Federator version negotiation error")
          . Log.field "domain" (domainText env.cveEnv.ceTargetDomain)
          . Log.field "error" (show v)
      pure ()
    Left e -> throwM e
    Right () -> pure ()

-- | Find the pair that maximises b.
pairedMaximumOn :: (Ord b) => (a -> b) -> [a] -> (a, b)
pairedMaximumOn f = maximumBy (compare `on` snd) . map (id &&& f)

-- FUTUREWORK: Recosider using 1 channel for many consumers. It shouldn't matter
-- for a handful of remote domains.
-- Consumers is passed in explicitly so that cleanup code has a reference to the consumer tags.
startPusher :: IORef (Map Domain (Q.ConsumerTag, MVar ())) -> Q.Channel -> AppT IO ()
startPusher consumersRef chan = do
  -- This ensures that we receive notifications 1 by 1 which ensures they are
  -- delivered in order.
  markAsWorking BackendNotificationPusher
  lift $ Q.qos chan 0 1 False
  -- Make sure threads aren't dangling if/when this async thread is killed
  let cleanup :: (Exception e, MonadThrow m, MonadIO m) => e -> m ()
      cleanup e = do
        consumers <- liftIO $ readIORef consumersRef
        traverse_ (liftIO . Q.cancelConsumer chan . fst) $ Map.elems consumers
        throwM e
  timeBeforeNextRefresh <- asks (.backendNotificationsConfig.remotesRefreshInterval)
  -- If this thread is cancelled, catch the exception, kill the consumers, and carry on.
  -- FUTUREWORK?:
  -- If this throws an exception on the Chan / in the forever loop, the exception will
  -- bubble all the way up and kill the pod. Kubernetes should restart the pod automatically.
  flip
    UnliftIO.catches
    [ Handler $ cleanup @SomeException,
      Handler $ cleanup @SomeAsyncException
    ]
    $ forever
    $ do
      remotes <- getRemoteDomains
      ensureConsumers consumersRef chan remotes
      threadDelay timeBeforeNextRefresh

ensureConsumers :: IORef (Map Domain (Q.ConsumerTag, MVar ())) -> Q.Channel -> [Domain] -> AppT IO ()
ensureConsumers consumers chan domains = do
  keys' <- Set.fromList . Map.keys <$> readIORef consumers
  let domains' = Set.fromList domains
      droppedDomains = Set.difference keys' domains'
  -- Loop over all of the new domains. We can check for existing consumers and add new ones.
  traverse_ (ensureConsumer consumers chan) domains
  -- Loop over all of the dropped domains. These need to be cancelled as they are no longer
  -- on the domain list.
  traverse_ (cancelConsumer consumers chan) droppedDomains

cancelConsumer :: IORef (Map Domain (Q.ConsumerTag, MVar ())) -> Q.Channel -> Domain -> AppT IO ()
cancelConsumer consumers chan domain = do
  Log.info $ Log.msg (Log.val "Stopping consumer") . Log.field "domain" (domainText domain)
  -- The ' version of atomicModifyIORef is strict in the function update and is useful
  -- for not leaking memory.
  atomicModifyIORef' consumers (\c -> (Map.delete domain c, Map.lookup domain c))
    >>= liftIO . traverse_ (Q.cancelConsumer chan . fst)

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
      clearRefs = do
        atomicWriteIORef chanRef Nothing
        atomicWriteIORef consumersRef mempty
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
