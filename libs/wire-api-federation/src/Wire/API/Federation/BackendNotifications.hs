module Wire.API.Federation.BackendNotifications where

import Data.Aeson
import Data.Domain
import qualified Data.Map as Map
import Imports
import qualified Network.AMQP as Q
import qualified Network.AMQP.Types as Q
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Federation.Client
import Wire.API.Federation.Error

data BackendNotificationContent
  = OnUserDeletedConnections UserDeletedConnectionsNotification
  deriving (Generic)

-- TODO(elland): use schema-profunctor, or not, who cares what this serialises to?
instance ToJSON BackendNotificationContent

instance FromJSON BackendNotificationContent

data BackendNotification = BackendNotification
  { ownDomain :: Domain,
    content :: BackendNotificationContent
  }
  deriving (Generic)

instance ToJSON BackendNotification

instance FromJSON BackendNotification

notificationTarget :: BackendNotificationContent -> Component
notificationTarget (OnUserDeletedConnections _) = Brig

sendNotificationBrig :: FederatorClientEnv -> BackendNotificationContent -> IO (Either FederatorClientError ())
sendNotificationBrig env (OnUserDeletedConnections notif) = do
  runFederatorClient env $ void $ fedClient @'Brig @"on-user-deleted-connections" notif

enqueue :: Q.Channel -> Domain -> BackendNotification -> Q.DeliveryMode -> IO ()
enqueue chan domain notif deliveryMode = do
  let msg =
        Q.newMsg
          { Q.msgBody = encode notif,
            Q.msgDeliveryMode = Just deliveryMode,
            Q.msgContentType = Just "application/json"
          }
      -- Empty string means default exchange
      exchange = ""
  ensureQueue chan domain
  void $ Q.publishMsg chan exchange (routingKey domain) msg

routingKey :: Domain -> Text
routingKey d = "backend-notifications." <> domainText d

-- | If you ever change this function and modify
-- queue parameters, know that it will start failing in the
-- next release! So be prepared to write migrations.
ensureQueue :: Q.Channel -> Domain -> IO ()
ensureQueue chan domain = do
  let opts =
        Q.QueueOpts
          { Q.queueName = routingKey domain,
            Q.queuePassive = False,
            Q.queueDurable = True,
            Q.queueExclusive = False,
            Q.queueAutoDelete = False,
            Q.queueHeaders =
              Q.FieldTable $
                Map.fromList
                  [ ("x-single-active-consumer", Q.FVBool True),
                    ("x-queue-type", Q.FVString "quorum")
                  ]
          }
  void $ Q.declareQueue chan opts
