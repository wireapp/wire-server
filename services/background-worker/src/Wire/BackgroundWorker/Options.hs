module Wire.BackgroundWorker.Options where

import Data.Aeson
import Imports
import Network.AMQP.Extended
import System.Logger.Extended
import Util.Options

data Opts = Opts
  { logLevel :: !Level,
    logFormat :: !(Maybe (Last LogFormat)),
    federatorInternal :: !Endpoint,
    rabbitmq :: !RabbitMqAdminOpts
  }
  deriving (Show, Generic)

instance FromJSON Opts
