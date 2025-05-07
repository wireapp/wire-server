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

module Options where

import Cassandra qualified as C
import Data.Text qualified as Text
import Imports
import Options.Applicative
import Selector

data Settings = Settings
  { casGalley :: !CassandraSettings,
    granularity :: Int,
    feature :: Text,
    selector :: Maybe Selector,
    update :: Maybe UpdateOperation
  }
  deriving (Show)

data CassandraSettings = CassandraSettings
  { hosts :: !String,
    port :: !Word16,
    keyspace :: !C.Keyspace
  }
  deriving (Show)

settingsParser :: Parser Settings
settingsParser =
  Settings
    <$> cassandraSettingsParser "galley"
    <*> option
      auto
      ( long "granularity"
          <> metavar "INT"
          <> help "Number of analysed teams for status report"
          <> value 10000
          <> showDefault
      )
    <*> strOption
      ( long "feature"
          <> short 'f'
          <> metavar "FEATURE"
          <> help "Name of the feature"
      )
    <*> optional
      ( option
          (eitherReader parseSelector)
          ( long "selector"
              <> help "Select configs based on their values like 'status=enabled', 'status=enabled && lockStatus=locked && config.defaultProtocol=1'"
          )
      )
    <*> optional
      ( option
          (eitherReader parseUpdateOperation)
          ( long "update"
              <> help "Update configs, example: 'status=disabled', ''"
          )
      )

cassandraSettingsParser :: String -> Parser CassandraSettings
cassandraSettingsParser ks =
  CassandraSettings
    <$> strOption
      ( long ("cassandra-host-" ++ ks)
          <> metavar "HOST"
          <> help ("Cassandra Host for: " ++ ks)
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long ("cassandra-port-" ++ ks)
          <> metavar "PORT"
          <> help ("Cassandra Port for: " ++ ks)
          <> value 9042
          <> showDefault
      )
    <*> ( C.Keyspace
            <$> strOption
              ( long ("cassandra-keyspace-" ++ ks)
                  <> metavar "STRING"
                  <> help ("Cassandra Keyspace for: " ++ ks)
                  <> value (Text.pack $ ks <> "_test")
                  <> showDefault
              )
        )
