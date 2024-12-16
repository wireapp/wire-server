-- | Perhaps this module should be a separate package and published to hackage.
module Network.RabbitMqAdmin where

import Data.Aeson as Aeson
import Imports
import Servant
import Servant.Client
import Servant.Client.Generic

type RabbitMqBasicAuth = BasicAuth "RabbitMq Management" BasicAuthData

type VHost = Text

type QueueName = Text

data Page a = Page {items :: [a], page :: Int, pageCount :: Int}
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (Page a) where
  parseJSON =
    genericParseJSON $
      defaultOptions
        { fieldLabelModifier = camelTo2 '_'
        }

instance (ToJSON a) => ToJSON (Page a) where
  toJSON =
    genericToJSON $
      defaultOptions
        { fieldLabelModifier = camelTo2 '_'
        }

-- | Upstream Docs:
-- https://rawcdn.githack.com/rabbitmq/rabbitmq-server/v3.12.0/deps/rabbitmq_management/priv/www/api/index.html
data AdminAPI route = AdminAPI
  { listQueuesByVHost ::
      route
        :- "api"
          :> "queues"
          :> Capture "vhost" VHost
          :> QueryParam' '[Required, Strict] "name" Text
          :> QueryParam' '[Required, Strict] "use_regex" Bool
          :> QueryParam' '[Required, Strict] "page_size" Int
          :> QueryParam' '[Required, Strict] "page" Int
          :> Get '[JSON] (Page Queue),
    deleteQueue ::
      route
        :- "api"
          :> "queues"
          :> Capture "vhost" VHost
          :> Capture "queue" QueueName
          :> DeleteNoContent,
    listConnectionsByVHost ::
      route
        :- "api"
          :> "vhosts"
          :> Capture "vhost" Text
          :> "connections"
          :> Get '[JSON] [Connection],
    deleteConnection ::
      route
        :- "api"
          :> "connections"
          :> Capture "name" Text
          :> DeleteNoContent
  }
  deriving (Generic)

data AuthenticatedAPI route = AuthenticatedAPI
  { api ::
      route
        :- RabbitMqBasicAuth
          :> ToServant AdminAPI AsApi
  }
  deriving (Generic)

jsonOptions :: Aeson.Options
jsonOptions = defaultOptions {fieldLabelModifier = camelTo2 '_'}

data Queue = Queue {name :: Text, vhost :: Text}
  deriving (Show, Eq, Generic)

instance FromJSON Queue

instance ToJSON Queue

data Connection = Connection
  { userProvidedName :: Maybe Text,
    name :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Connection where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Connection where
  toJSON = genericToJSON jsonOptions

adminClient :: BasicAuthData -> AdminAPI (AsClientT ClientM)
adminClient ba = fromServant $ clientWithAuth.api ba
  where
    clientWithAuth :: AuthenticatedAPI (AsClientT ClientM)
    clientWithAuth = genericClient
