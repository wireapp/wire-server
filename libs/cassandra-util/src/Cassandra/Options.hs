{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Cassandra.Options where

import Data.Aeson.TH
import Data.Text qualified as T
import Imports

data Endpoint = Endpoint
  { host :: !Text,
    port :: !Word16
  }
  deriving (Show, Generic)

deriveJSON defaultOptions ''Endpoint

data CassandraOpts = CassandraOpts
  { endpoint :: !Endpoint,
    keyspace :: !Text,
    -- | If this option is unset, use all available nodes.
    -- If this option is set, use only cassandra nodes in the given datacentre
    --
    -- This option is most likely only necessary during a cassandra DC migration
    -- FUTUREWORK: remove this option again, or support a datacentre migration feature
    filterNodesByDatacentre :: !(Maybe Text),
    tlsCa :: Maybe FilePath
  }
  deriving (Show, Generic)

deriveFromJSON defaultOptions ''CassandraOpts

toPulsarUrl :: Endpoint -> String
toPulsarUrl e = "pulsar://" <> T.unpack e.host <> ":" <> show e.port
