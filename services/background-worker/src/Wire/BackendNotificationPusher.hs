{-# LANGUAGE RecordWildCards #-}

module Wire.BackendNotificationPusher where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Retry
import qualified Data.Aeson as A
import Data.Domain
import Imports
import Network.AMQP (ConsumerTag, cancelConsumer)
import qualified Network.AMQP as Q
import qualified Network.AMQP.Lifted as QL
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
  -- Jittered exponential backoff with 10ms as starting delay and
  -- 300s as max delay.
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
        ceFederator <- asks federatorInternal
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
  let go :: [ConsumerTag] -> IO ()
      go consumers = do
        -- Wait for a new set of domains
        chanRemotes <- readChan $ env.remoteDomainsChan
        -- Cancel all of the existing consumers
        traverse_ (cancelConsumer chan) consumers
        -- Make new consumers for the new domains
        consumers' <- traverse (runAppT env . startPushingNotifications chan) $ domain <$> chanRemotes.remotes
        -- Repeat
        go consumers'
  initRemotes <- liftIO $ readIORef env.remoteDomains
  consumersRef <- newIORef []
  let cleanup :: AsyncCancelled -> IO ()
      cleanup e = do
        consumers <- readIORef consumersRef
        traverse_ (cancelConsumer chan) consumers
        throwM e
  -- If this thread is cancelled, catch the exception, kill the consumers, and carry on.
  liftIO $ async $ handle cleanup $ do
    -- Get an initial set of consumers for the domains pulled from the IORef
    consumers <- traverse (runAppT env . startPushingNotifications chan) $ domain <$> initRemotes.remotes
    atomicWriteIORef consumersRef consumers
    -- Loop on waiting for new domains, tearing down consumers, and building new ones
    go consumers
