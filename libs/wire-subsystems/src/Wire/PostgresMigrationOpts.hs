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

module Wire.PostgresMigrationOpts where

import Data.Aeson
import Data.Text qualified as Text
import Imports

data StorageLocation
  = -- | Use when solely using Cassandra
    CassandraStorage
  | -- | Use while migration to postgresql. Using this option does not trigger
    --   the migration. Newly created data is stored in Postgresql.
    --   Once this has been turned on, it MUST NOT be made CassandraStorage ever
    --   again.
    MigrationToPostgresql
  | -- | Use after migrating to postgresql
    PostgresqlStorage
  deriving (Show)

instance FromJSON StorageLocation where
  parseJSON = withText "StorageLocation" $ \case
    "cassandra" -> pure CassandraStorage
    "migration-to-postgresql" -> pure MigrationToPostgresql
    "postgresql" -> pure PostgresqlStorage
    x -> fail $ "Invalid storage location: " <> Text.unpack x <> ". Valid options: cassandra, postgresql, migration-to-postgresql"

data PostgresMigrationOpts = PostgresMigrationOpts
  { conversation :: StorageLocation,
    conversationCodes :: StorageLocation,
    teamFeatures :: StorageLocation
  }
  deriving (Show)

instance FromJSON PostgresMigrationOpts where
  parseJSON = withObject "PostgresMigrationOpts" $ \o ->
    PostgresMigrationOpts
      <$> o .: "conversation"
      <*> o .: "conversationCodes"
      <*> o .: "teamFeatures"
