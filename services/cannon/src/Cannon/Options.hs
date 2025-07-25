{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

module Cannon.Options
  ( host,
    port,
    cannon,
    gundeck,
    externalHost,
    externalHostFile,
    logLevel,
    logNetStrings,
    logFormat,
    drainOpts,
    wSOpts,
    rabbitmq,
    cassandraOpts,
    rabbitMqMaxConnections,
    rabbitMqMaxChannels,
    notificationTTL,
    Opts (..),
    gracePeriodSeconds,
    millisecondsBetweenBatches,
    minBatchSize,
    disabledAPIVersions,
    DrainOpts,
    WSOpts (..),
    validateOpts,
  )
where

import Cassandra.Options (CassandraOpts)
import Control.Lens (makeFields)
import Data.Aeson
import Data.Aeson.APIFieldJsonTH
import Data.Default
import Imports
import Network.AMQP.Extended (AmqpEndpoint)
import System.Logger.Extended (Level, LogFormat)
import Wire.API.Routes.Version

data Cannon = Cannon
  { _cannonHost :: !String,
    _cannonPort :: !Word16,
    _cannonExternalHost :: !(Maybe Text),
    _cannonExternalHostFile :: !(Maybe FilePath)
  }
  deriving (Eq, Show, Generic)

makeFields ''Cannon

deriveApiFieldJSON ''Cannon

data Gundeck = Gundeck
  { _gundeckHost :: !Text,
    _gundeckPort :: !Word16
  }
  deriving (Eq, Show, Generic)

makeFields ''Gundeck

deriveApiFieldJSON ''Gundeck

data WSOpts = WSOpts
  { activityTimeout :: Int,
    pongTimeout :: Int
  }
  deriving (Eq, Show, Generic)

instance Default WSOpts where
  def =
    WSOpts
      { activityTimeout = 30000000,
        pongTimeout = 30000000
      }

instance FromJSON WSOpts where
  parseJSON = withObject "WSOpts" $ \o ->
    WSOpts
      <$> o .:? "activityTimeout" .!= (def :: WSOpts).activityTimeout
      <*> o .:? "pongTimeout" .!= (def :: WSOpts).pongTimeout

data DrainOpts = DrainOpts
  { -- | Maximum amount of time draining should take. Must not be set to 0.
    _drainOptsGracePeriodSeconds :: Word64,
    -- | Maximum amount of time between batches, this speeds up draining in case
    -- there are not many users connected. Must not be set to 0.
    _drainOptsMillisecondsBetweenBatches :: Word64,
    -- | Batch size is calculated considering actual number of websockets and
    -- gracePeriod. If this number is too small, '_drainOptsMinBatchSize' is
    -- used.
    _drainOptsMinBatchSize :: Word64
  }
  deriving (Eq, Show, Generic)

makeFields ''DrainOpts

deriveApiFieldJSON ''DrainOpts

data Opts = Opts
  { _optsCannon :: !Cannon,
    _optsGundeck :: !Gundeck,
    _optsRabbitmq :: !AmqpEndpoint,
    _optsLogLevel :: !Level,
    _optsLogNetStrings :: !(Maybe (Last Bool)),
    _optsLogFormat :: !(Maybe (Last LogFormat)),
    _optsDrainOpts :: DrainOpts,
    _optsWSOpts :: WSOpts,
    _optsDisabledAPIVersions :: !(Set VersionExp),
    _optsCassandraOpts :: !CassandraOpts,
    -- | Maximum number of rabbitmq connections. Must be strictly positive.
    _optsRabbitMqMaxConnections :: Int,
    -- | Maximum number of rabbitmq channels per connection. Must be strictly positive.
    _optsRabbitMqMaxChannels :: Int,
    _optsNotificationTTL :: Int
  }
  deriving (Show, Generic)

makeFields ''Opts

validateOpts :: Opts -> IO ()
validateOpts opts = do
  when (opts._optsRabbitMqMaxConnections <= 0) $ do
    fail "rabbitMqMaxConnections must be strictly positive"
  when (opts._optsRabbitMqMaxChannels <= 0) $ do
    fail "rabbitMqMaxChannels must be strictly positive"

instance FromJSON Opts where
  parseJSON = withObject "CannonOpts" $ \o ->
    Opts
      <$> o .: "cannon"
      <*> o .: "gundeck"
      <*> o .: "rabbitmq"
      <*> o .: "logLevel"
      <*> o .:? "logNetStrings"
      <*> o .:? "logFormat"
      <*> o .: "drainOpts"
      <*> o .:? "wsOpts" .!= def
      <*> o .: "disabledAPIVersions"
      <*> o .: "cassandra"
      <*> o .:? "rabbitMqMaxConnections" .!= 1000
      <*> o .:? "rabbitMqMaxChannels" .!= 300
      <*> o .: "notificationTTL"
