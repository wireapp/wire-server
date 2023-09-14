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
    backgroundWorker :: !Endpoint,
    federatorInternal :: !Endpoint,
    rabbitmq :: !RabbitMqAdminOpts,
    galley :: !Endpoint,
    brig :: !Endpoint,
    -- | Seconds, Nothing for no timeout
    defederationTimeout :: Maybe Int,
    backendNotificationPusher :: BackendNotificationsConfig,
    federationDomain :: !Domain,
    cassandra :: !DBSettings
  }
  deriving (Show, Generic)

data DBSettings = DBSettings
  { brig :: !CassandraOpts,
    galley :: !CassandraOpts
  }
  deriving (Show, Generic)

instance FromJSON DBSettings

instance FromJSON Opts

data BackendNotificationsConfig = BackendNotificationsConfig
  { -- | Minimum amount of time (in microseconds) to wait before doing the first
    -- retry in pushing a notification. Futher retries are done in a jittered
    -- exponential way.
    -- https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/
    pushBackoffMinWait :: Int,
    -- | Upper limit on amount of time (in microseconds) to wait before retrying
    -- any notification. This exists to ensure that exponential back-off doesn't
    -- cause wait times to be very big.
    pushBackoffMaxWait :: Int
  }
  deriving (Show, Generic)

instance FromJSON BackendNotificationsConfig
