module Wire.BackendNotificationPusher.Options where

import Data.Aeson
import Data.Domain
import Imports
import Util.Options

data Opts = Opts
  { federatorInternal :: !Endpoint,
    rabbitMQ :: !RabbitMQOpts,
    remoteBackends :: [Domain]
  }
  deriving (Show, Generic)

instance FromJSON Opts

data RabbitMQOpts = RabbitMQOpts
  { host :: !String,
    port :: !Int,
    vHost :: !Text
  }
  deriving (Show, Generic)

instance FromJSON RabbitMQOpts
