-- | Perhaps this module should be a separate package and published to hackage.
module Network.RabbitMqAdmin where

import Data.Aeson
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
          :> Get '[JSON] [Queue],
    getQueue ::
      route
        :- "api"
          :> "queues"
          :> Capture "vhost" VHost
          :> Capture "queue" Text
          :> Get '[JSON] Queue,
    deleteQueue ::
      route
        :- "api"
          :> "queues"
          :> Capture "vhost" VHost
          :> Capture "queue" QueueName
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

data Queue = Queue {name :: Text, vhost :: Text, status :: Maybe Text}
  deriving (Show, Eq, Generic)

instance FromJSON Queue

instance ToJSON Queue

adminClient :: BasicAuthData -> AdminAPI (AsClientT ClientM)
adminClient ba = fromServant $ clientWithAuth.api ba
  where
    clientWithAuth :: AuthenticatedAPI (AsClientT ClientM)
    clientWithAuth = genericClient
