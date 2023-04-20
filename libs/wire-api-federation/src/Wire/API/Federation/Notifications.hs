module Wire.API.Federation.Notifications where

import Data.Aeson
import Data.Domain
import qualified Data.Map as Map
import Imports
import qualified Network.AMQP as Q
import qualified Network.AMQP.Types as Q
import Wire.API.Federation.API.Brig
import Wire.API.Federation.Component

data BackendNotification
  = OnUserDeletedConnections UserDeletedConnectionsNotification

instance ToJSON BackendNotification where
  toJSON (OnUserDeletedConnections userDeleteConns) =
    object
      [ "type" .= String "OnUserDeletedConnections",
        "notification" .= toJSON userDeleteConns
      ]

notificationTarget :: BackendNotification -> Component
notificationTarget (OnUserDeletedConnections _) = Brig

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

-- | If you ever change this function, know that it will start failing in the
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
