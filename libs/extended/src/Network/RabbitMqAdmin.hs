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

-- | Upstream Docs:
-- https://rawcdn.githack.com/rabbitmq/rabbitmq-server/v3.12.0/deps/rabbitmq_management/priv/www/api/index.html
data AdminAPI route = AdminAPI
  { -- | NOTE: This endpoint can be made paginated, but that complicates
    -- consumer code a little. This might be needed for performance tuning
    -- later, but perhaps not.
    listQueuesByVHost ::
      route
        :- "api"
          :> "queues"
          :> Capture "vhost" VHost
          :> QueryParam "name" Text
          :> QueryParam "use_regex" Bool
          :> Get '[JSON] [Queue],
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
