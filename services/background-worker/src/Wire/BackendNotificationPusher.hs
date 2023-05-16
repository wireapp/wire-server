{-# LANGUAGE RecordWildCards #-}

module Wire.BackendNotificationPusher where

import Control.Monad.Catch
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
  case A.eitherDecode @BackendNotification (Q.msgBody msg) of
    Left e ->
      Log.err $
        Log.msg (Log.val "Invalid notification for backend")
          . Log.field "domain" (domainText targetDomain)
          . Log.field "error" e
    Right notif -> do
      case notificationTarget notif.content of
        Brig -> do
          ceFederator <- asks federatorInternal
          ceHttp2Manager <- asks http2Manager
          let ceOriginDomain = notif.ownDomain
              ceTargetDomain = targetDomain
          let fcEnv = FederatorClientEnv {..}
          lift (sendNotification fcEnv notif.content)
            -- TODO(elland): Deal with this error
            >>= either throwM pure
          lift $ Q.ackEnv envelope
        _ -> undefined

startWorker :: [Domain] -> Q.Channel -> AppT IO ()
startWorker remoteDomains chan = do
  lift $ Q.qos chan 0 1 False
  mapM_ (startPushingNotifications chan) remoteDomains
  forever $ threadDelay maxBound
