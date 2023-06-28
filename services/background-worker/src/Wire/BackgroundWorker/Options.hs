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
    rabbitmq :: !RabbitMqAdminOpts,
    backendNotificationPusher :: !BackendNotificationPusherOpts
  }
  deriving (Show, Generic)

instance FromJSON Opts

data BackendNotificationPusherOpts = BackendNotificationPusherOpts
  { -- | seconds, how often should the remotes be refreshed.
    remotesRefreshInterval :: !Int
  }
  deriving (Show, Generic)

instance FromJSON BackendNotificationPusherOpts
