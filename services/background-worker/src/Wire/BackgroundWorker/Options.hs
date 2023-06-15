module Wire.BackgroundWorker.Options where

import Data.Aeson
import Data.Domain
import Imports
import Network.AMQP.Extended
import System.Logger.Extended
import Util.Options

data Opts = Opts
  { logLevel :: !Level,
    logFormat :: !(Maybe (Last LogFormat)),
    federatorInternal :: !Endpoint,
    galley :: !Endpoint,
    rabbitmq :: !RabbitMqOpts,
    remoteDomains :: [Domain]
  }
  deriving (Show, Generic)

instance FromJSON Opts
