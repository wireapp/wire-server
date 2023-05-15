module Wire.BackgroundWorker.Options where

import Data.Aeson
import Data.Domain
import Imports
import System.Logger.Extended
import Util.Options

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
