-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.BackgroundWorker.Options where

import Data.Aeson
import Data.Domain (Domain)
import Data.Misc
import Data.Range (Range)
import GHC.Generics
import Hasql.Pool.Extended
import Imports
import Network.AMQP.Extended
import System.Logger.Extended
import Util.Options
import Wire.ConversationStore (PostgresMigrationOpts)

data Opts = Opts
  { logLevel :: !Level,
    logFormat :: !(Maybe (Last LogFormat)),
    backgroundWorker :: !Endpoint,
    federatorInternal :: !Endpoint,
    brig :: Endpoint,
    gundeck :: Endpoint,
    rabbitmq :: !RabbitMqOpts,
    -- | Seconds, Nothing for no timeout
    defederationTimeout :: Maybe Int,
    backendNotificationPusher :: BackendNotificationsConfig,
    cassandra :: CassandraOpts,
    cassandraGalley :: CassandraOpts,
    cassandraBrig :: CassandraOpts,
    -- | Postgresql settings, the key values must be in libpq format.
    -- https://www.postgresql.org/docs/17/libpq-connect.html#LIBPQ-PARAMKEYWORDS
    postgresql :: !(Map Text Text),
    postgresqlPassword :: !(Maybe FilePathSecrets),
    postgresqlPool :: !PoolConfig,
    postgresMigration :: !PostgresMigrationOpts,
    migrateConversations :: Bool,
    backgroundJobs :: BackgroundJobsConfig,
    federationDomain :: Domain
  }
  deriving (Show, Generic)
  deriving (FromJSON) via Generically Opts

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
  deriving (FromJSON) via Generically BackendNotificationsConfig

newtype RabbitMqOpts = RabbitMqOpts {unRabbitMqOpts :: Either AmqpEndpoint RabbitMqAdminOpts}
  deriving (Show)

instance FromJSON RabbitMqOpts where
  parseJSON v =
    RabbitMqOpts
      <$> ( (Right <$> parseJSON v)
              <|> (Left <$> parseJSON v)
          )

data BackgroundJobsConfig = BackgroundJobsConfig
  { -- | Maximum parallel jobs processed by this process
    concurrency :: Range 1 1000 Int,
    -- | Per-attempt timeout (seconds)
    jobTimeout :: Duration,
    -- | Total attempts including first run
    maxAttempts :: Range 1 1000 Int
  }
  deriving (Show, Generic)
  deriving (FromJSON) via Generically BackgroundJobsConfig
