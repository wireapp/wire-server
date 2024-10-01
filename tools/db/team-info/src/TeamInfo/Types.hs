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

module TeamInfo.Types where

import Cassandra as C
import Control.Lens
import Data.Id
import Data.LegalHold (UserLegalHoldStatus)
import Data.Text.Strict.Lens
import Data.Time
import Database.CQL.Protocol hiding (Result)
import Imports
import Options.Applicative

data CassandraSettings = CassandraSettings
  { host :: String,
    port :: Int,
    keyspace :: C.Keyspace
  }

data Opts = Opts
  { brigDb :: CassandraSettings,
    galleyDb :: CassandraSettings,
    teamId :: TeamId
  }

optsParser :: Parser Opts
optsParser =
  Opts
    <$> brigCassandraParser
    <*> galleyCassandraParser
    <*> ( option
            auto
            ( long "team-id"
                <> short 't'
                <> metavar "ID"
                <> help "Team ID"
            )
        )

galleyCassandraParser :: Parser CassandraSettings
galleyCassandraParser =
  CassandraSettings
    <$> strOption
      ( long "galley-cassandra-host"
          <> metavar "HOST"
          <> help "Cassandra Host for galley"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "galley-cassandra-port"
          <> metavar "PORT"
          <> help "Cassandra Port for galley"
          <> value 9043
          <> showDefault
      )
    <*> ( C.Keyspace
            . view packed
            <$> strOption
              ( long "galley-cassandra-keyspace"
                  <> metavar "STRING"
                  <> help "Cassandra Keyspace for galley"
                  <> value "galley_test"
                  <> showDefault
              )
        )

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

data TeamMemberRow = TeamMemberRow
  { id :: UserId,
    legalhold :: Maybe UserLegalHoldStatus
  }
  deriving (Show, Generic)

recordInstance ''TeamMemberRow

data TeamMember = TeamMember
  { id :: UserId,
    legalhold :: Maybe UserLegalHoldStatus,
    lastActive :: Maybe UTCTime
  }
  deriving (Generic)

-- output as csv
instance Show TeamMember where
  show tm = show tm.id <> "," <> maybe " " show tm.legalhold <> "," <> maybe " " show tm.lastActive
