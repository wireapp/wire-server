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
    galley :: !Endpoint,
    brig :: !Endpoint,
    rabbitmq :: !RabbitMqOpts,
    defederationTimeout :: Maybe Int -- Seconds, Nothing for no timeout
  }
  deriving (Show, Generic)

instance FromJSON Opts
