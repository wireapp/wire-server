module Wire.BackendNotificationPusher.Options where

import Data.Aeson
import Data.Domain
import Imports
import Util.Options

data Opts = Opts
  { federatorInternal :: !Endpoint,
    rabbitMq :: !RabbitMqOpts,
    remoteDomains :: [Domain]
  }
  deriving (Show, Generic)

instance FromJSON Opts

data RabbitMqOpts = RabbitMqOpts
  { host :: !String,
    port :: !Int,
    vHost :: !Text
  }
  deriving (Show, Generic)

instance FromJSON RabbitMqOpts
