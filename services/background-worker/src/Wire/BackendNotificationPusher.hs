module Wire.BackendNotificationPusher where

import Control.Exception
import qualified Data.Aeson as A
import Data.Domain
import Imports
import qualified Network.AMQP as Q
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
  ReaderT Env IO Q.ConsumerTag
startPushingNotifications chan domain = do
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
            -- TODO(elland): Deal with this error
            >>= either throwIO pure
          Q.ackEnv envelope
        _ -> undefined

startWorker :: Env -> [Domain] -> Q.Channel -> IO ()
startWorker env remoteDomains chan = do
  -- TODO(elland): Watch these and respawn if needed
  flip runReaderT env $ mapM_ (startPushingNotifications chan) remoteDomains
  forever $ threadDelay maxBound
