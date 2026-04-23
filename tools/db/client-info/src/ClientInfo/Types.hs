{-# LANGUAGE DeriveGeneric #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module ClientInfo.Types where

import Cassandra as C
import Control.Lens
import Data.Id (UserId)
import Data.Text.Strict.Lens
import Imports
import Options.Applicative
import Util.Timeout (UTCTime)

newtype InputFile = InputFile {unInputFile :: FilePath}

newtype ClientId = ClientId {unClientId :: Text}
  deriving newtype (Show)

data ClientRow = ClientRow
  { userId :: UserId,
    clientId :: ClientId,
    clientModel :: Maybe Text,
    clientLabel :: Maybe Text,
    lastActive :: Maybe UTCTime
  }
  deriving (Show, Generic)

data CassandraSettings = CassandraSettings
  { host :: String,
    port :: Int,
    keyspace :: C.Keyspace
  }

data Opts = Opts
  { brigDb :: CassandraSettings,
    inputFile :: InputFile,
    searchTerms :: Maybe Text
  }

optsParser :: Parser Opts
optsParser =
  Opts
    <$> brigCassandraParser
    <*> ( InputFile
            <$> strOption
              ( long "input"
                  <> short 'i'
                  <> metavar "FILE"
                  <> help "File containing user UUIDs (one per line)"
              )
        )
    <*> optional
      ( strOption
          ( long "search-terms"
              <> short 's'
              <> metavar "CSV"
              <> help "Comma-separated search terms to match label/model (case-insensitive)"
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
