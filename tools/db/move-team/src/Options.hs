{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Options
  ( setCasBrig,
    setCasGalley,
    setCasSpar,
    setCasGundeck,
    cHosts,
    cPort,
    cKeyspace,
    settingsParser,
    CommandSettings (..),
  )
where

import Cassandra qualified as C
import Control.Lens
import Data.Text.Strict.Lens
import Data.UUID
import Imports
import Options.Applicative

data CassandraSettings = CassandraSettings
  { _cHosts :: !String,
    _cPort :: !Word16,
    _cKeyspace :: !C.Keyspace
  }
  deriving (Show)

data ConnectionSettings = ConnectionSettings
  { _setCasBrig :: !CassandraSettings,
    _setCasGalley :: !CassandraSettings,
    _setCasSpar :: !CassandraSettings,
    _setCasGundeck :: !CassandraSettings
  }
  deriving (Show)

data CommandSettings
  = Export !UUID !FilePath !ConnectionSettings
  | Import !FilePath !ConnectionSettings
  | DebugExportFull !FilePath !ConnectionSettings

makeLenses ''ConnectionSettings

makeLenses ''CassandraSettings

connectionParser :: Parser ConnectionSettings
connectionParser =
  ConnectionSettings
    <$> cassandraSettingsParser "brig"
    <*> cassandraSettingsParser "galley"
    <*> cassandraSettingsParser "spar"
    <*> cassandraSettingsParser "gundeck"

settingsParser :: Parser CommandSettings
settingsParser =
  subparser
    ( command "export" (info (helper <*> exportParser) fullDesc)
        <> command "import" (info (helper <*> importParser) fullDesc)
        <> command "debug-export-full" (info (helper <*> debugExportFullParser) fullDesc)
    )
  where
    exportParser :: Parser CommandSettings
    exportParser =
      ( Export . parseUUID
          <$> strOption
            ( long "teamid"
                <> metavar "TEAMID"
                <> help "team id"
            )
      )
        <*> strOption
          ( long "target-path"
              <> metavar "PATH"
              <> help "Directory for export files"
              <> value "/tmp/export-team/"
              <> showDefault
          )
        <*> connectionParser

    importParser :: Parser CommandSettings
    importParser =
      Import
        <$> strOption
          ( long "source-path"
              <> metavar "PATH"
              <> help "Directory with exported files"
              <> value "/tmp/export-team/"
              <> showDefault
          )
        <*> connectionParser

    debugExportFullParser :: Parser CommandSettings
    debugExportFullParser =
      DebugExportFull
        <$> strOption
          ( long "target-path"
              <> metavar "PATH"
              <> help "Directory for export files"
              <> value "/tmp/export-team/"
              <> showDefault
          )
        <*> connectionParser

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

parseUUID :: (HasCallStack) => String -> UUID
parseUUID = fromJust . Data.UUID.fromString
