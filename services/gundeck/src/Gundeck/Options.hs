{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Gundeck.Options where

import Control.Lens hiding (Level)
import Data.Aeson.TH
import Data.Yaml (FromJSON)
import Gundeck.Aws.Arn
import Imports
import Network.AMQP.Extended
import System.Logger.Extended (Level, LogFormat)
import Util.Options
import Util.Options.Common
import Wire.API.Routes.Version

newtype NotificationTTL = NotificationTTL
  {notificationTTLSeconds :: Word32}
  deriving (Eq, Ord, Show, Generic, FromJSON)

data AWSOpts = AWSOpts
  { -- | AWS account
    _account :: !Account,
    -- | AWS region name
    _region :: !Region,
    -- | Environment name to scope ARNs to
    _arnEnv :: !ArnEnv,
    -- | SQS queue name
    _queueName :: !Text,
    _sqsEndpoint :: !AWSEndpoint,
    _snsEndpoint :: !AWSEndpoint
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''AWSOpts

makeLenses ''AWSOpts

data Settings = Settings
  { -- | Number of connections to keep open in the http-client pool
    _httpPoolSize :: !Int,
    -- | TTL (seconds) of stored notifications
    _notificationTTL :: !NotificationTTL,
    -- | Use this option to group push notifications and send them in bulk to Cannon, instead
    -- of in individual requests
    _bulkPush :: !Bool,
    -- | Maximum number of concurrent threads calling SNS.
    _maxConcurrentNativePushes :: !(Maybe MaxConcurrentNativePushes),
    -- | Maximum number of parallel requests to SNS and cassandra
    -- during native push processing (per incoming push request)
    -- defaults to unbounded, if unset.
    _perNativePushConcurrency :: !(Maybe Int),
    -- | The amount of time in milliseconds to wait after reading from an SQS queue
    -- returns no message, before asking for messages from SQS again.
    -- defaults to 'defSqsThrottleMillis'.
    -- When using real SQS from AWS, throttling isn't needed as much, since using
    --   SQS.rmWaitTimeSeconds (Just 20) in Gundeck.Aws.listen
    -- ensures that there is only one request every 20 seconds.
    -- However, that parameter is not honoured when using fake-sqs
    -- (where throttling can thus make sense)
    _sqsThrottleMillis :: !(Maybe Int),
    _disabledAPIVersions :: !(Set VersionExp),
    -- | Maximum number of bytes loaded into memory when fetching (referenced) payloads.
    -- Gundeck will return a truncated page if the whole page's payload sizes would exceed this limit in total.
    -- Inlined payloads can cause greater payload sizes to be loaded into memory regardless of this setting.
    _maxPayloadLoadSize :: Maybe Int32,
    -- | Cassandra page size for fetching notifications. Does not directly
    -- effect the page size request in the client API. A lower number will
    -- reduce the amount by which setMaxPayloadLoadSize is exceeded when loading
    -- notifications from the database if notifications have inlined payloads.
    _internalPageSize :: Maybe Int32
  }
  deriving (Show, Generic)

data MaxConcurrentNativePushes = MaxConcurrentNativePushes
  { -- | more than this number of threads will not be allowed
    _hard :: !(Maybe Int),
    -- | more than this number of threads will be warned about
    _soft :: !(Maybe Int)
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''MaxConcurrentNativePushes

makeLenses ''MaxConcurrentNativePushes

data RedisConnectionMode
  = Master
  | Cluster
  deriving (Show, Generic)

deriveJSON defaultOptions {constructorTagModifier = map toLower} ''RedisConnectionMode

data RedisEndpoint = RedisEndpoint
  { _host :: !Text,
    _port :: !Word16,
    _connectionMode :: !RedisConnectionMode,
    _enableTls :: !Bool,
    -- | When not specified, use system CA bundle
    _tlsCa :: !(Maybe FilePath),
    -- | When 'True', uses TLS but does not verify hostname or CA or validity of
    -- the cert. Not recommended to set to 'True'.
    _insecureSkipVerifyTls :: !Bool
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''RedisEndpoint

makeLenses ''RedisEndpoint

makeLenses ''Settings

deriveFromJSON toOptionFieldName ''Settings

data Opts = Opts
  { -- | Hostname and port to bind to
    _gundeck :: !Endpoint,
    _brig :: !Endpoint,
    _cassandra :: !CassandraOpts,
    _redis :: !RedisEndpoint,
    _redisAdditionalWrite :: !(Maybe RedisEndpoint),
    _rabbitmq :: !RabbitMqOpts,
    _aws :: !AWSOpts,
    _discoUrl :: !(Maybe Text),
    _settings :: !Settings,
    -- Logging

    -- | Log level (Debug, Info, etc)
    _logLevel :: !Level,
    -- | Use netstrings encoding:
    --   <http://cr.yp.to/proto/netstrings.txt>
    _logNetStrings :: !(Maybe (Last Bool)),
    _logFormat :: !(Maybe (Last LogFormat))
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Opts

makeLenses ''Opts

defSqsThrottleMillis :: Int
defSqsThrottleMillis = 500
