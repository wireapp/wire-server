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
    rabbitmq :: !RabbitMqOpts,
    -- | Seconds, Nothing for no timeout
    defederationTimeout :: Maybe Int,
    backendNotificationPusher :: BackendNotificationsConfig
  }
  deriving (Show, Generic)

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
    pushBackoffMaxWait :: Int,
    -- | The list of remotes is refreshed at an interval. This value in
    -- microseconds decides the interval for polling.
    remotesRefreshInterval :: Int
  }
  deriving (Show, Generic)

instance FromJSON BackendNotificationsConfig

newtype RabbitMqOpts = RabbitMqOpts {unRabbitMqOpts :: Either AmqpEndpoint RabbitMqAdminOpts}
  deriving (Show)

instance FromJSON RabbitMqOpts where
  parseJSON v =
    RabbitMqOpts
      <$> ( (Right <$> parseJSON v)
              <|> (Left <$> parseJSON v)
          )
