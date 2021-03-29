{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
    setCasSpar,
    cHosts,
    cPort,
    cKeyspace,
    commandParser,
    Command (..),
  )
where

import qualified Cassandra as C
import Control.Lens
import Data.Id
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
    _setCasSpar :: !CassandraSettings
  }
  deriving (Show)

data Command
  = Change TeamId FilePath !ConnectionSettings

makeLenses ''ConnectionSettings

makeLenses ''CassandraSettings

connectionParser :: Parser ConnectionSettings
connectionParser =
  ConnectionSettings
    <$> cassandraSettingsParser "brig"
    <*> cassandraSettingsParser "spar"

commandParser :: Parser Command
commandParser =
  subparser
    ( command "change" (info (helper <*> changeParser) fullDesc)
    )
  where
    changeParser :: Parser Command
    changeParser =
      Change
        <$> ( parseUUID
                <$> strOption
                  ( long "teamid"
                      <> metavar "TEAMID"
                      <> help "team id"
                  )
            )
        <*> strOption
          ( long "mapping-csv"
              <> metavar "PATH"
              <> help "CSV with email, new_external_id"
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

parseUUID :: HasCallStack => String -> TeamId
parseUUID = Id . fromJust . Data.UUID.fromString
