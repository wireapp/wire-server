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
  -- TODO: DANGEROUS!
  -- This can make the program unresponsive and unkillable outside of SIGKILL.
  -- It needs to be used for _short_ running code, and can easily cause
  recovering policy handlers $ const $ UnliftIO.uninterruptibleMask_ go
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
startPusher :: IORef Bool -> IORef (Map Domain Q.ConsumerTag) -> Q.Channel -> AppT IO ()
startPusher running consumers chan = do
  -- This ensures that we receive notifications 1 by 1 which ensures they are
  -- delivered in order.
  markAsWorking BackendNotificationPusher
  lift $ Q.qos chan 0 1 False
  BackendNotificationPusherOpts {..} <- asks (.backendNotificationPusher)
  let go = do
        remoteDomains <- getRemoteDomains
        mapM_ (ensureConsumer consumers chan) remoteDomains
        threadDelay (1_000_000 * remotesRefreshInterval)
  -- On each iteration, check that we haven't been signalled
  -- to stop the service.
  forever $ readIORef running >>= flip when go

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
  pure [Domain "example.com", Domain "example.net"]
  -- Jittered exponential backoff with 10ms as starting delay and 60s as max
  -- cumulative delay. When this is reached, the operation fails.
  --
  -- FUTUREWORK: Pull these numbers into config
  -- let policy = limitRetriesByCumulativeDelay 60_000_000 $ fullJitterBackoff 10000
  --     logErrr willRetry (SomeException e) rs =
  --       Log.err $
  --         Log.msg (Log.val "Exception occurred while refreshig domains")
  --           . Log.field "error" (displayException e)
  --           . Log.field "willRetry" willRetry
  --           . Log.field "retryCount" rs.rsIterNumber
  --     handlers =
  --       skipAsyncExceptions
  --         <> [logRetries (const $ pure True) logErrr]
  -- recovering policy handlers $ const go
  -- where
  --   go :: AppT IO [Domain]
  --   go = do
  --     client <- asks rabbitmqAdminClient
  --     vhost <- asks rabbitmqVHost
  --     queues <- liftIO $ listQueuesByVHost client vhost
  --     let notifQueuesSuffixes = mapMaybe (\q -> Text.stripPrefix "backend-notifications." q.name) queues
  --     catMaybes <$> traverse (\d -> either (\e -> logInvalidDomain d e >> pure Nothing) (pure . Just) $ mkDomain d) notifQueuesSuffixes
  --   logInvalidDomain d e =
  --     Log.warn $
  --       Log.msg (Log.val "Found invalid domain in a backend notifications queue name")
  --         . Log.field "queue" ("backend-notifications." <> d)
  --         . Log.field "error" e

startWorker :: RabbitMqAdminOpts -> AppT IO ()
startWorker rabbitmqOpts = do
  env <- ask
  -- AsyncCancelled is used when our `Async ()` is `cancel`led.
  -- This is used in the POSIX signal handlers, so we should catch it
  -- here and clean up our processes, letting them finish if we can.
  chanRef <- newIORef Nothing
  -- Passed into running threads to stop them from setting up
  -- new processing consumers when we are trying to shutdown.
  runningRef <- newIORef True
  -- Passed into running threads so we can cancel consumers and allow
  -- amqp to cleanly finish before we stop the service.
  consumersRef <- newIORef mempty
  let -- cleanup the refs when channels die
      clearRefs = do
        putStrLn "clearRefs called"
        atomicWriteIORef chanRef Nothing
        atomicWriteIORef consumersRef mempty
      -- Close channels when the async thread is cancelled.
      -- The exceptions used by `amqp` to cancel threads will
      -- be caught/masked by workers so they can finish their
      -- current task before dying. We need an IORef holding
      -- references to these consumers so we can cancel them
      -- explicitly.
      cleanup = do
        let l = logger env
        Log'.info l $ Log'.msg (Log'.val "startWorker cleanup called")
        putStrLn "startWorker cleanup called"
        m <- readIORef chanRef
        for_ m $ \chan -> do
          putStrLn "Cancelling channel"
          consumers <- readIORef consumersRef
          for_ (Map.elems consumers) $ \conn -> do
            putStrLn $ "cancelling consumer " <> show conn
            Q.cancelConsumer chan conn
          Log'.info l $ Log'.msg (Log'.val "cleaned up consumers")
          putStrLn "cleaned up consumers"
          -- Close the channel. Using this function will ensure
          -- that `connectWithRetries` doesn't reopen it.
          -- Additionally the bracket pattern used in it will
          -- close the connection for us.
          Q.closeChannel chan
          Log'.info l $ Log'.msg (Log'.val "closed channel")
          putStrLn "closed channel"
  liftIO $ Control.Monad.Catch.handle (\AsyncCancelled -> cleanup) $
    openConnectionWithRetries env.logger (demoteOpts rabbitmqOpts) $
    RabbitMqHooks
      { onNewChannel = \chan ->  do
          atomicWriteIORef chanRef $ pure chan
          runAppT env $ startPusher runningRef consumersRef chan,
        onChannelException = \_ -> do
          clearRefs
          runAppT env $ markAsNotWorking BackendNotificationPusher,
        onConnectionClose = do
          clearRefs
          runAppT env $ markAsNotWorking BackendNotificationPusher
      }
