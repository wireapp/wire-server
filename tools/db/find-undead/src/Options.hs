{-# LANGUAGE OverloadedStrings #-}

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

import Cassandra qualified as C
import Data.Text qualified as Text
import Imports
import Options.Applicative

data MigratorSettings = MigratorSettings
  { setCasBrig :: CassandraSettings,
    setESBrig :: ElasticSettings
  }
  deriving (Show)

data CassandraSettings = CassandraSettings
  { cHosts :: !String,
    cPort :: !Word16,
    cKeyspace :: !C.Keyspace
  }
  deriving (Show)

data ElasticSettings = ElasticSettings
  { esHost :: !String,
    esPort :: !Word16,
    esIndex :: !String,
    esMapping :: !String
  }
  deriving (Show)

settingsParser :: Parser MigratorSettings
settingsParser =
  MigratorSettings
    <$> cassandraSettingsParser "brig"
    <*> esSettingsParser

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
    <*> ( C.Keyspace . Text.pack
            <$> strOption
              ( long ("cassandra-keyspace-" ++ ks)
                  <> metavar "STRING"
                  <> help ("Cassandra Keyspace for: " ++ ks)
                  <> value (ks ++ "_test")
                  <> showDefault
              )
        )

esSettingsParser :: Parser ElasticSettings
esSettingsParser =
  ElasticSettings
    <$> strOption (long "es-host" <> value "localhost")
    <*> option auto (long "es-port" <> value 9200)
    <*> strOption (long "es-index" <> value "directory_test")
    <*> strOption (long "es-mapping" <> value "user")
