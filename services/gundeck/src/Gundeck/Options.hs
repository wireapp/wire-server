-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gundeck.Options where

import Control.Lens hiding (Level)
import Data.Aeson.TH
import Data.Yaml (FromJSON)
import Gundeck.Aws.Arn
import Imports
import System.Logger.Extended (Level, LogFormat)
import Util.Options
import Util.Options.Common

newtype NotificationTTL
  = NotificationTTL
      {notificationTTLSeconds :: Word32}
  deriving (Eq, Ord, Show, Generic, FromJSON)

data AWSOpts
  = AWSOpts
      { -- | AWS account
        _awsAccount :: !Account,
        -- | AWS region name
        _awsRegion :: !Region,
        -- | Environment name to scope ARNs to
        _awsArnEnv :: !ArnEnv,
        -- | SQS queue name
        _awsQueueName :: !Text,
        _awsSqsEndpoint :: !AWSEndpoint,
        _awsSnsEndpoint :: !AWSEndpoint
      }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''AWSOpts

makeLenses ''AWSOpts

data Settings
  = Settings
      { -- | Number of connections to keep open in the http-client pool
        _setHttpPoolSize :: !Int,
        -- | TTL (seconds) of stored notifications
        _setNotificationTTL :: !NotificationTTL,
        -- | Use this option to group push notifications and send them in bulk to Cannon, instead
        -- of in individual requests
        _setBulkPush :: !Bool,
        -- | Maximum number of concurrent threads calling SNS.
        _setMaxConcurrentNativePushes :: !(Maybe MaxConcurrentNativePushes),
        -- | Maximum number of parallel requests to SNS and cassandra
        -- during native push processing (per incoming push request)
        -- defaults to unbounded, if unset.
        _setPerNativePushConcurrency :: !(Maybe Int)
      }
  deriving (Show, Generic)

data MaxConcurrentNativePushes
  = MaxConcurrentNativePushes
      { -- | more than this number of threads will not be allowed
        _limitHard :: !(Maybe Int),
        -- | more than this number of threads will be warned about
        _limitSoft :: !(Maybe Int)
      }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Settings

makeLenses ''Settings

deriveFromJSON toOptionFieldName ''MaxConcurrentNativePushes

makeLenses ''MaxConcurrentNativePushes

data Opts
  = Opts
      { -- | Hostname and port to bind to
        _optGundeck :: !Endpoint,
        _optCassandra :: !CassandraOpts,
        _optRedis :: !Endpoint,
        _optAws :: !AWSOpts,
        _optDiscoUrl :: !(Maybe Text),
        _optSettings :: !Settings,
        -- Logging

        -- | Log level (Debug, Info, etc)
        _optLogLevel :: !Level,
        -- | Use netstrings encoding:
        --   <http://cr.yp.to/proto/netstrings.txt>
        _optLogNetStrings :: !(Maybe (Last Bool)),
        _optLogFormat :: !(Maybe (Last LogFormat))
      }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Opts

makeLenses ''Opts
