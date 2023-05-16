{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.BackendNotificationPusher where

import Control.Monad.Catch
import Control.Retry
import qualified Data.Aeson as A
import Data.Domain
import Imports
import qualified Network.AMQP as Q
import qualified Network.AMQP.Lifted as QL
import qualified System.Logger.Class as Log
import Wire.API.Federation.API
import Wire.API.Federation.BackendNotifications
import Wire.API.Federation.Client
import Wire.BackgroundWorker.Env

-- TODO(elland): This calls the callback for next notification even if one fails,
-- implement some sort of blocking, which causes push back so memory doesn't
-- blow up.
startPushingNotifications ::
  Q.Channel ->
  Domain ->
  AppT IO Q.ConsumerTag
startPushingNotifications chan domain = do
  lift $ ensureQueue chan domain
  QL.consumeMsgs chan (routingKey domain) Q.Ack (pushNotification domain)

pushNotification :: Domain -> (Q.Message, Q.Envelope) -> AppT IO ()
pushNotification targetDomain (msg, envelope) = do
  let handlers =
        [ Handler $ \(e :: Q.ChanThreadKilledException) -> throwM e,
          Handler $ \(SomeException e) -> do
            -- TODO(elland): This error likely means that no new notifications
            -- will be retrieved from RabbitMQ as the envelope will not have
            -- been acked. Perhaps we should restart this consumer.
            Log.err $
              Log.msg (Log.val "Unexpected exception occured while pushing notification")
                . Log.field "error" (displayException e)
                . Log.field "domain" (domainText targetDomain)
            throwM e
        ]
  flip catches handlers $ case A.eitherDecode @BackendNotification (Q.msgBody msg) of
    Left e -> do
      Log.err $
        Log.msg (Log.val "Invalid notification for backend")
          . Log.field "domain" (domainText targetDomain)
          . Log.field "error" e

      -- [Reject Messages]
      --
      -- FUTUREWORK: This rejects the message without any requeueing. This is
      -- dangerous as it could happen that a new type of notification is
      -- introduced and an old instance of this worker is running, in which case
      -- the notification will just get dropped. On the other hand not dropping
      -- this message blocks the whole queue. Perhaps there is a better way to
      -- deal with this.
      lift $ Q.rejectEnv envelope False
    Right notif -> do
      case notificationTarget notif.content of
        Brig -> do
          ceFederator <- asks federatorInternal
          ceHttp2Manager <- asks http2Manager
          let ceOriginDomain = notif.ownDomain
              ceTargetDomain = targetDomain
          let fcEnv = FederatorClientEnv {..}
              -- Jittered exponential backoff with 10ms as starting delay and
              -- 300s as max delay.
              policy = capDelay 300_000_000 $ fullJitterBackoff 10000
              shouldRetry status eithRes = do
                case eithRes of
                  Right () -> pure False
                  Left e -> do
                    -- Logging at 'error' level is probably too much in case a
                    -- backend is down. This is 'info' while we test this
                    -- functionality. Maybe this should be demeoted to 'debug'.
                    Log.info $
                      Log.msg (Log.val "Failed to push notification, will retry")
                        . Log.field "domain" (domainText targetDomain)
                        . Log.field "error" (displayException e)
                        . Log.field "retryCount" status.rsIterNumber
                    pure True
          void $ retrying policy shouldRetry (const $ lift $ sendNotificationBrig fcEnv notif.content)
          lift $ Q.ackEnv envelope
        c -> do
          Log.err $
            Log.msg (Log.val "Notifications for component not implmented")
              . Log.field "component" (show c)
          -- See Note [Reject Messages]
          lift $ Q.rejectEnv envelope False

-- FUTUREWORK: Recosider using 1 channel for many consumers. It shouldn't matter
-- for a handful of remote domains.
startWorker :: [Domain] -> Q.Channel -> AppT IO ()
startWorker remoteDomains chan = do
  lift $ Q.qos chan 0 1 False
  mapM_ (startPushingNotifications chan) remoteDomains
  forever $ threadDelay maxBound
