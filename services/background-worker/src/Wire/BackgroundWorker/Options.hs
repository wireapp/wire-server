module Wire.BackgroundWorker.Options where

import Data.Aeson
import Imports
import Network.AMQP.Extended
import System.Logger.Extended
import Util.Options

data Opts = Opts
  { logLevel :: !Level,
    logFormat :: !(Maybe (Last LogFormat)),
    backgroundWorker :: !Endpoint,
    federatorInternal :: !Endpoint,
    galley :: !Endpoint,
    brig :: !Endpoint,
    defederationTimeout :: Maybe Int, -- Seconds, Nothing for no timeout
    rabbitmq :: !RabbitMqAdminOpts
  }
  deriving (Show, Generic)

instance FromJSON Opts
