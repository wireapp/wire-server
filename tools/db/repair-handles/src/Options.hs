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

module Options where

import Cassandra hiding (Set)
import Data.Id
import Data.Text qualified as T
import Data.UUID
import Imports
import Options.Applicative hiding (action)
import Types

settingsParser :: Parser Settings
settingsParser =
  Settings
    <$> cassandraSettingsParser "brig"
    <*> cassandraSettingsParser "galley"
    <*> switch (short 'n' <> long "dry-run")
    <*> switch (short 'd' <> long "debug")
    <*> option auto (short 's' <> long "page-size" <> value 1000)
    <*> (Id . parseUUID <$> strArgument (metavar "TEAM-UUID"))

parseUUID :: (HasCallStack) => String -> UUID
parseUUID = fromJust . Data.UUID.fromString

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
    <*> ( Keyspace . T.pack
            <$> strOption
              ( long ("cassandra-keyspace-" ++ ks)
                  <> metavar "STRING"
                  <> help ("Cassandra Keyspace for: " ++ ks)
                  <> value (ks ++ "_test")
                  <> showDefault
              )
        )
