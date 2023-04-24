module Wire.BackendNotificationPusher where

import Control.Exception
import qualified Data.Aeson as A
import Data.Domain
import Imports
import qualified Network.AMQP as Q
import Wire.API.Federation.API
import Wire.API.Federation.Client
import Wire.API.Federation.Notifications
import Wire.BackendNotificationPusher.Env
import Wire.BackendNotificationPusher.Options

-- TODO: This calls the callback for next notification even if one fails,
-- implement some sort of blocking, which causes push back so memory doesn't
-- blow up.
startPushingNotifications ::
  Domain ->
  ReaderT Env IO Q.ConsumerTag
startPushingNotifications domain = do
  chan <- readIORef =<< asks rabbitMqChannel
  lift $ ensureQueue chan domain
  env <- ask
  lift $ Q.consumeMsgs chan (routingKey domain) Q.Ack (pushNotification env domain)

pushNotification :: Env -> Domain -> (Q.Message, Q.Envelope) -> IO ()
pushNotification env targetDomain (msg, envelope) = do
  case A.eitherDecode @BackendNotification (Q.msgBody msg) of
    Left e -> putStrLn $ "Invalid message for backend " <> show targetDomain <> ", error: " <> show e
    Right notif -> do
      case notificationTarget notif.content of
        Brig -> do
          let fcEnv =
                FederatorClientEnv
                  { ceOriginDomain = notif.ownDomain,
                    ceTargetDomain = targetDomain,
                    ceFederator = env.federatorInternal,
                    ceHttp2Manager = env.http2Manager
                  }
          liftIO (sendNotification fcEnv notif.content)
            -- TODO: Deal with this error
            >>= either throwIO pure
          Q.ackEnv envelope
        _ -> undefined

run :: Opts -> IO ()
run opts = do
  env <- mkEnv opts
  -- TODO: Watch these and respawn if needed
  flip runReaderT env $ mapM_ startPushingNotifications opts.remoteDomains
  forever $ threadDelay maxBound
