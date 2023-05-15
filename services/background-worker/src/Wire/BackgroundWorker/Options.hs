module Wire.BackgroundWorker.Options where

import Data.Aeson
import Data.Domain
import Imports
import Util.Options
import System.Logger.Extended

data Opts = Opts 
  { logLevel :: !Level,
    logFormat :: !(Maybe (Last LogFormat)),
    federatorInternal :: !Endpoint,
    rabbitmq :: !RabbitMqOpts,
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
