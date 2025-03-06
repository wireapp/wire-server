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

module Options
  ( setCasGalley,
    setGranularity,
    cHosts,
    cPort,
    cKeyspace,
    settingsParser,
  )
where

import Cassandra qualified as C
import Control.Lens
import Data.Text.Strict.Lens
import Imports
import Options.Applicative

data MigratorSettings = MigratorSettings
  { _setCasGalley :: !CassandraSettings,
    _setGranularity :: Int
  }
  deriving (Show)

data CassandraSettings = CassandraSettings
  { _cHosts :: !String,
    _cPort :: !Word16,
    _cKeyspace :: !C.Keyspace
  }
  deriving (Show)

makeLenses ''MigratorSettings

makeLenses ''CassandraSettings

settingsParser :: Parser MigratorSettings
settingsParser =
  MigratorSettings
    <$> cassandraSettingsParser "galley"
    <*> option
      auto
      ( long "granularity"
          <> metavar "INT"
          <> help "Number of migrated teams for status report"
          <> value 10000
          <> showDefault
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
    <*> ( C.Keyspace . view packed
            <$> strOption
              ( long ("cassandra-keyspace-" ++ ks)
                  <> metavar "STRING"
                  <> help ("Cassandra Keyspace for: " ++ ks)
                  <> value (ks ++ "_test")
                  <> showDefault
              )
        )
