{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.API.Federation.BackendNotifications where

import Control.Exception
import Control.Monad.Except
import Data.Aeson
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Text.Encoding
import qualified Data.Text.Lazy.Encoding as TL
import Imports
import qualified Network.AMQP as Q
import qualified Network.AMQP.Types as Q
import Network.HTTP.Types
import Servant
import Servant.Client
import Servant.Client.Core
import Servant.Types.SourceT
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
    body :: RawJson
  }
  deriving (Show, Eq)

instance ToJSON BackendNotification where
  toJSON notif =
    object
      [ "ownDomain" .= notif.ownDomain,
        "targetComponent" .= notif.targetComponent,
        "path" .= notif.path,
        "body" .= TL.decodeUtf8 notif.body.rawJsonBytes
      ]

instance FromJSON BackendNotification where
  parseJSON = withObject "BackendNotification" $ \o ->
    BackendNotification
      <$> o .: "ownDomain"
      <*> o .: "targetComponent"
      <*> o .: "path"
      <*> (RawJson . TL.encodeUtf8 <$> o .: "body")

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

    go :: forall c. KnownComponent c => IO (Either FederatorClientError ())
    go =
      runFederatorClient env . void $
        clientIn (Proxy @BackendNotificationAPI) (Proxy @(FederatorClient c)) (withoutFirstSlash path) body

enqueue :: Q.Channel -> Domain -> Domain -> Q.DeliveryMode -> FedQueueClient c () -> IO ()
enqueue channel originDomain targetDomain deliveryMode (FedQueueClient action) =
  runReaderT action FedQueueEnv {..}

routingKey :: Text -> Text
routingKey d = "backend-notifications." <> d

-- Listing the queue part here so that all
-- services that need to use this queue have a
-- common source of truth for its name.
defederateQueue :: Text
defederateQueue = "defederate"

-- | If you ever change this function and modify
-- queue parameters, know that it will start failing in the
-- next release! So be prepared to write migrations.
ensureQueue :: Q.Channel -> Text -> IO ()
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

-- * Internal machinery

-- | Reads a servant request and puts the information in relevant RabbitMQ
-- queue. Perhaps none of this should be servant code anymore. But it is here to
-- allow smooth transition to RabbitMQ based notification pushing.
--
-- Use 'Wire.API.Federation.API.fedQueueClient' to create and action and pass it
-- to 'enqueue'
newtype FedQueueClient c a = FedQueueClient (ReaderT FedQueueEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader FedQueueEnv)

data FedQueueEnv = FedQueueEnv
  { channel :: Q.Channel,
    originDomain :: Domain,
    targetDomain :: Domain,
    deliveryMode :: Q.DeliveryMode
  }

data EnqueueError = EnqueueError String
  deriving (Show)

instance Exception EnqueueError

instance KnownComponent c => RunClient (FedQueueClient c) where
  runRequestAcceptStatus :: Maybe [Status] -> Request -> FedQueueClient c Response
  runRequestAcceptStatus _ req = do
    env <- ask
    bodyLBS <- case requestBody req of
      Just (RequestBodyLBS lbs, _) -> pure lbs
      Just (RequestBodyBS bs, _) -> pure (LBS.fromStrict bs)
      Just (RequestBodySource src, _) -> liftIO $ do
        errOrRes <- runExceptT $ runSourceT src
        either (throwIO . EnqueueError) (pure . mconcat) errOrRes
      Nothing -> pure mempty
    let notif =
          BackendNotification
            { ownDomain = env.originDomain,
              targetComponent = componentVal @c,
              path = decodeUtf8 $ LBS.toStrict $ Builder.toLazyByteString req.requestPath,
              body = RawJson bodyLBS
            }
    let msg =
          Q.newMsg
            { Q.msgBody = encode notif,
              Q.msgDeliveryMode = Just (env.deliveryMode),
              Q.msgContentType = Just "application/json"
            }
        -- Empty string means default exchange
        exchange = ""
    liftIO $ do
      ensureQueue env.channel $ domainText $ env.targetDomain
      void $ Q.publishMsg env.channel exchange (routingKey $ domainText env.targetDomain) msg
    pure $
      Response
        { responseHttpVersion = http20,
          responseStatusCode = status200,
          responseHeaders = Seq.singleton (hContentType, "application/json"),
          responseBody = "{}"
        }
  throwClientError :: ClientError -> FedQueueClient c a
  throwClientError = liftIO . throwIO
