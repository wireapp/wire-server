{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module LegalholdClients.Types where

import Cassandra as C
import Control.Lens
import Data.Id
import Data.Text.Strict.Lens
import Data.Time
import Database.CQL.Protocol hiding (Result)
import Imports
import Options.Applicative
import Wire.API.User.Client (ClientType)

data CassandraSettings = CassandraSettings
  { host :: String,
    port :: Int,
    keyspace :: C.Keyspace
  }

data Opts = Opts
  { brigDb :: CassandraSettings
  }

optsParser :: Parser Opts
optsParser =
  Opts <$> brigCassandraParser

brigCassandraParser :: Parser CassandraSettings
brigCassandraParser =
  CassandraSettings
    <$> strOption
      ( long "brig-cassandra-host"
          <> metavar "HOST"
          <> help "Cassandra Host for brig"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "brig-cassandra-port"
          <> metavar "PORT"
          <> help "Cassandra Port for brig"
          <> value 9042
          <> showDefault
      )
    <*> ( C.Keyspace
            . view packed
            <$> strOption
              ( long "brig-cassandra-keyspace"
                  <> metavar "STRING"
                  <> help "Cassandra Keyspace for brig"
                  <> value "brig_test"
                  <> showDefault
              )
        )

data ClientRow = ClientRow
  { id :: ClientId,
    user :: UserId,
    clientType :: ClientType,
    createdAt :: UTCTime,
    lastActive :: Maybe UTCTime
  }
  deriving (Generic)

recordInstance ''ClientRow

data ClientInfo = ClientInfo
  { client :: ClientRow,
    team :: Maybe TeamId
  }
  deriving (Generic)

toCsvRow :: ClientInfo -> String
toCsvRow ci =
  let cr = ci.client
   in maybe "N/A" show ci.team <> "," <> show (clientToText cr.id) <> "," <> show cr.createdAt <> "," <> maybe "N/A" show cr.lastActive
