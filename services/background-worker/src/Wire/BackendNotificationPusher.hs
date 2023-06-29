{-# LANGUAGE RecordWildCards #-}

module Wire.BackendNotificationPusher where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Retry
import qualified Data.Aeson as A
import Data.Domain
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Imports
import Network.AMQP (cancelConsumer)
import qualified Network.AMQP as Q
import qualified Network.AMQP.Lifted as QL
import Network.RabbitMqAdmin
import qualified System.Logger.Class as Log
import Wire.API.Federation.BackendNotifications
import Wire.API.Federation.Client
import Wire.API.Routes.FederationDomainConfig
import Wire.BackgroundWorker.Env
import Wire.BackgroundWorker.Util

startPushingNotifications ::
  Q.Channel ->
  Domain ->
  AppT IO Q.ConsumerTag
startPushingNotifications chan domain = do
  lift $ ensureQueue chan domain._domainText
  QL.consumeMsgs chan (routingKey domain._domainText) Q.Ack (pushNotification domain)

pushNotification :: RabbitMQEnvelope e => Domain -> (Q.Message, e) -> AppT IO ()
pushNotification targetDomain (msg, envelope) = do
  -- Jittered exponential backoff with 10ms as starting delay and 300s as max
  -- delay. When 300s is reached, every retry will happen after 300s.
  --
  -- FUTUREWORK: Pull these numbers into config
  let policy = capDelay 300_000_000 $ fullJitterBackoff 10000
      logErrr willRetry (SomeException e) rs =
        Log.err $
          Log.msg (Log.val "Exception occurred while pushing notification")
            . Log.field "error" (displayException e)
            . Log.field "domain" (domainText targetDomain)
            . Log.field "willRetry" willRetry
            . Log.field "retryCount" rs.rsIterNumber
      skipChanThreadKilled _ = Handler $ \(_ :: Q.ChanThreadKilledException) -> pure False
      handlers =
        skipAsyncExceptions
          <> [ skipChanThreadKilled,
               logRetries (const $ pure True) logErrr
             ]
  recovering policy handlers $ const go
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

-- FUTUREWORK: Recosider using 1 channel for many consumers. It shouldn't matter
-- for a handful of remote domains.
startWorker :: Q.Channel -> AppT IO (Async ())
startWorker chan = do
  -- This ensures that we receive notifications 1 by 1 which ensures they are
  -- delivered in order.
  lift $ Q.qos chan 0 1 False
  env <- ask
  consumersRef <- newIORef mempty
  -- Make sure threads aren't dangling if/when this async thread is killed
  let cleanup :: Exception e => e -> IO ()
      cleanup e = do
        consumers <- readIORef consumersRef
        traverse_ (cancelConsumer chan) $ Map.elems consumers
        throwM e

  -- If this thread is cancelled, catch the exception, kill the consumers, and carry on.
  -- FUTUREWORK?:
  -- If this throws an exception on the Chan / in the forever loop, the exception will
  -- bubble all the way up and kill the pod. Kubernetes should restart the pod automatically.
  liftIO $ async $ flip catches
    [ Handler $ cleanup @SomeException
    , Handler $ cleanup @SomeAsyncException
    ] $ runAppT env $ do
    -- Get an initial set of domains from the sync thread
    -- The Chan that we will be waiting on isn't initialised with a
    -- value until the domain update loop runs the callback for the
    -- first time.
    initRemotes <- liftIO $ readIORef env.remoteDomains
    -- Get an initial set of consumers for the domains pulled from the IORef
    -- so that we aren't just sitting around not doing anything for a bit at
    -- the start.
    ensureConsumers consumersRef chan $ domain <$> initRemotes.remotes
    -- Wait for updates to the domains, this is where the bulk of the action
    -- is going to take place
    forever $ do
      -- Wait for a new set of domains. This is a blocking action
      -- so we will only move past here when we get a new set of domains.
      -- It is a bit nicer than having another timeout value, as Brig is
      -- already providing one in the domain update message.
      chanRemotes <- liftIO $ readChan env.remoteDomainsChan
      -- Make new consumers for the new domains, clean up old ones from the consumer map.
      ensureConsumers consumersRef chan $ domain <$> chanRemotes.remotes

ensureConsumers :: IORef (Map Domain Q.ConsumerTag) -> Q.Channel -> [Domain] -> AppT IO ()
ensureConsumers consumers chan domains = do
  keys' <- Set.fromList . Map.keys <$> readIORef consumers
  let domains' = Set.fromList domains
      droppedDomains = Set.difference keys' domains'
  -- Loop over all of the new domains. We can check for existing consumers and add new ones.
  traverse_ (ensureConsumer consumers chan) domains
  -- Loop over all of the dropped domains. These need to be cancelled as they are no longer
  -- on the domain list.
  traverse_ (cancelConsumer' consumers chan) droppedDomains

cancelConsumer' :: IORef (Map Domain Q.ConsumerTag) -> Q.Channel -> Domain -> AppT IO ()
cancelConsumer' consumers chan domain = do
  Log.info $ Log.msg (Log.val "Stopping consumer") . Log.field "domain" (domainText domain)
  -- The ' version of atomicModifyIORef is strict in the function update and is useful
  -- for not leaking memory.
  atomicModifyIORef' consumers (\c -> (Map.delete domain c, Map.lookup domain c))
    >>= liftIO . traverse_ (Q.cancelConsumer chan)

ensureConsumer :: IORef (Map Domain Q.ConsumerTag) -> Q.Channel -> Domain -> AppT IO ()
ensureConsumer consumers chan domain = do
  consumerExists <- Map.member domain <$> readIORef consumers
  unless consumerExists $ do
    Log.info $ Log.msg (Log.val "Starting consumer") . Log.field "domain" (domainText domain)
    tag <- startPushingNotifications chan domain
    -- TODO: Check if the map is spine strict. This strict call might not be needed.
    -- The ' version of atomicModifyIORef is strict in the function update and is useful
    -- for not leaking memory.
    oldTag <- atomicModifyIORef' consumers $ \c -> (Map.insert domain tag c, Map.lookup domain c)
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
