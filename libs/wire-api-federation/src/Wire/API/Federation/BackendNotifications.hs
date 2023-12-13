{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.API.Federation.BackendNotifications where

import Control.Exception
import Control.Monad.Except
import Data.Aeson
import Data.Domain
import Data.Id (RequestId)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.Lazy.Encoding qualified as TL
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Types qualified as Q
import Servant
import Servant.Client.Core
import Wire.API.Federation.API.Common
import Wire.API.Federation.Client
import Wire.API.Federation.Component
import Wire.API.Federation.Error
import Wire.API.RawJson

-- | NOTE: Stored in RabbitMQ, any changes to serialization of this object could cause
-- notifications to get lost.
data BackendNotification = BackendNotification
  { ownDomain :: Domain,
    targetComponent :: Component,
    path :: Text,
    -- | Using RawJson here allows the backend notification pusher to not parse
    -- this body, which could be very large and completely useless to the
    -- pusher. This also makes development less clunky as we don't have to
    -- create a sum type here for all types of notifications that could exist.
    body :: RawJson,
    requestId :: RequestId
  }
  deriving (Show, Eq)

instance ToJSON BackendNotification where
  toJSON notif =
    object
      [ "ownDomain" .= notif.ownDomain,
        "targetComponent" .= notif.targetComponent,
        "path" .= notif.path,
        "body" .= TL.decodeUtf8 notif.body.rawJsonBytes,
        "requestId" .= notif.requestId
      ]

instance FromJSON BackendNotification where
  parseJSON = withObject "BackendNotification" $ \o ->
    BackendNotification
      <$> o .: "ownDomain"
      <*> o .: "targetComponent"
      <*> o .: "path"
      <*> (RawJson . TL.encodeUtf8 <$> o .: "body")
      <*> o .: "requestId"

type BackendNotificationAPI = Capture "name" Text :> ReqBody '[JSON] RawJson :> Post '[JSON] EmptyResponse

sendNotification :: FederatorClientEnv -> Component -> Text -> RawJson -> IO (Either FederatorClientError ())
sendNotification env component path body =
  case component of
    Brig -> go @'Brig
    Galley -> go @'Galley
    Cargohold -> go @'Cargohold
  where
    withoutFirstSlash :: Text -> Text
    withoutFirstSlash (Text.stripPrefix "/" -> Just t) = t
    withoutFirstSlash t = t

    go :: forall c. (KnownComponent c) => IO (Either FederatorClientError ())
    go =
      runFederatorClient env . void $
        clientIn (Proxy @BackendNotificationAPI) (Proxy @(FederatorClient c)) (withoutFirstSlash path) body

enqueue :: Q.Channel -> RequestId -> Domain -> Domain -> Q.DeliveryMode -> FedQueueClient c a -> IO a
enqueue channel requestId originDomain targetDomain deliveryMode (FedQueueClient action) =
  runReaderT action FedQueueEnv {..}

routingKey :: Text -> Text
routingKey t = "backend-notifications." <> t

-- Shared values for both brig and background worker so they are
-- kept in sync about what types they are expecting and where
-- they are stored in Rabbit.
type DefederationDomain = Domain

defederationQueue :: Text
defederationQueue = "delete-federation"

-- | If you ever change this function and modify
-- queue parameters, know that it will start failing in the
-- next release! So be prepared to write migrations.
ensureQueue :: Q.Channel -> Text -> IO ()
ensureQueue chan queue = do
  let opts =
        Q.QueueOpts
          { Q.queueName = routingKey queue,
            Q.queuePassive = False,
            Q.queueDurable = True,
            Q.queueExclusive = False,
            Q.queueAutoDelete = False,
            Q.queueHeaders =
              Q.FieldTable $
                Map.fromList
                  -- single-active-consumer is used because it is order
                  -- preserving, especially into databases and to remote servers,
                  -- exactly what we are doing here!
                  -- Without single active consumer, messages will be delivered
                  -- round-robbin to all consumers, but then we lose effect-ordering
                  -- due to processing and network times.
                  [ ("x-single-active-consumer", Q.FVBool True),
                    ("x-queue-type", Q.FVString "quorum")
                  ]
          }
  void $ Q.declareQueue chan opts

-- * Internal machinery

-- | Reads a servant request and puts the information in relevant RabbitMQ
-- queue. Perhaps none of this should be servant code anymore. But it is here to
-- allow smooth transition to RabbitMQ based notification pushing.
--
-- Use 'Wire.API.Federation.API.fedQueueClient' to create an action and pass it
-- to 'enqueue'
newtype FedQueueClient c a = FedQueueClient (ReaderT FedQueueEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader FedQueueEnv)

data FedQueueEnv = FedQueueEnv
  { channel :: Q.Channel,
    originDomain :: Domain,
    targetDomain :: Domain,
    deliveryMode :: Q.DeliveryMode,
    requestId :: RequestId
  }

data EnqueueError = EnqueueError String
  deriving (Show)

instance Exception EnqueueError
