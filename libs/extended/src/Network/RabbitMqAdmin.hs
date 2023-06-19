-- | Perhaps this module should be a separate package and published to hackage.
module Network.RabbitMqAdmin where

import Data.Aeson
import Fcf.Combinators
import Imports
import Servant
import Servant.Client
import Servant.Client.Generic
import Servant.QueryParam.Client.Record ()
import Servant.QueryParam.Record

type RabbitMqBasicAuth = BasicAuth "RabbitMq Management" BasicAuthData

type VHost = Text

-- | Upstream Docs:
-- https://rawcdn.githack.com/rabbitmq/rabbitmq-server/v3.12.0/deps/rabbitmq_management/priv/www/api/index.html#pagination
data AdminAPI route = AdminAPI
  { listQueues ::
      route
        :- "api"
          :> "queues"
          :> Get '[JSON] [Queue],
    listQueuesByVHost ::
      route
        :- "api"
          :> "queues"
          :> Capture "vhost" VHost
          :> RecordParam Pure PaginationParams
          :> Get '[JSON] [Queue]
  }
  deriving (Generic)

data AuthenticatedAPI route = AuthenticatedAPI
  { api ::
      route
        :- RabbitMqBasicAuth
          :> ToServant AdminAPI AsApi
  }
  deriving (Generic)

data Queue = Queue {name :: Text, vhost :: Text}
  deriving (Show, Eq, Generic)

instance FromJSON Queue

-- | NOTE: This type uses snake case for its atrributes so it is less convoluted
-- to translate them to query params, which are expected to be in snake case.
data PaginationParams = PaginationParams
  { page :: Maybe Word,
    page_size :: Maybe Word,
    name :: Maybe Text,
    use_regex :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

noPaginationParams :: PaginationParams
noPaginationParams = PaginationParams Nothing Nothing Nothing Nothing

adminClient :: BasicAuthData -> AdminAPI (AsClientT ClientM)
adminClient ba = fromServant $ clientWithAuth.api ba
  where
    clientWithAuth :: AuthenticatedAPI (AsClientT ClientM)
    clientWithAuth = genericClient
