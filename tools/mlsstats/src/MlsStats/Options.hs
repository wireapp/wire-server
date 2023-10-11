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

module MlsStats.Options
  ( Opts (..),
    CassandraSettings (..),
    S3Settings (..),
    optsParser,
  )
where

import Amazonka
import Cassandra qualified as C
import Data.Text qualified as Text
import Imports
import Options.Applicative
import Options.Applicative.Types (readerAsk)
import Util.Options

data Opts = Opts
  { cassandraSettings :: CassandraSettings,
    s3Settings :: S3Settings
  }
  deriving (Show, Generic)

data CassandraSettings = CassandraSettings
  { brigHost :: String,
    brigPort :: Word16,
    brigKeyspace :: C.Keyspace,
    galleyHost :: String,
    galleyPort :: Word16,
    galleyKeyspace :: C.Keyspace,
    pageSize :: Int32
  }
  deriving (Show)

data S3Settings = S3Settings
  { endpoint :: AWSEndpoint,
    region :: Maybe Text,
    addressingStyle :: S3AddressingStyle,
    bucketName :: Text,
    bucketDir :: Maybe String
  }
  deriving (Show)

optsParser :: Parser Opts
optsParser =
  Opts
    <$> cassandraSettingsParser
    <*> s3SettingsParser

cassandraSettingsParser :: Parser CassandraSettings
cassandraSettingsParser =
  CassandraSettings
    <$> strOption
      ( long "brig-cassandra-host"
          <> metavar "HOST"
          <> help "Cassandra host for Brig"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "brig-cassandra-port"
          <> metavar "PORT"
          <> help "Cassandra port for Brig"
          <> value 9042
          <> showDefault
      )
    <*> ( C.Keyspace . Text.pack
            <$> strOption
              ( long "brig-cassandra-keyspace"
                  <> metavar "STRING"
                  <> help "Cassandra Keyspaces for Brig"
                  <> value ("brig_test")
                  <> showDefault
              )
        )
    <*> strOption
      ( long "galley-cassandra-host"
          <> metavar "HOST"
          <> help "Cassandra host for Galley"
          <> value "localhost"
          <> showDefault
      )
    <*> option
      auto
      ( long "galley-cassandra-port"
          <> metavar "PORT"
          <> help "Cassandra port for Brig"
          <> value 9042
          <> showDefault
      )
    <*> ( C.Keyspace . Text.pack
            <$> strOption
              ( long "galley-cassandra-keyspace"
                  <> metavar "STRING"
                  <> help "Cassandra Keyspaces for Galley"
                  <> value ("galley_test")
                  <> showDefault
              )
        )
    <*> option
      auto
      ( long "cassandra-pagesize"
          <> metavar "PAGESIZE"
          <> help "Cassandra pagesize for queries"
          <> value 1024
          <> showDefault
      )

s3SettingsParser :: Parser S3Settings
s3SettingsParser =
  S3Settings
    <$> option
      parseAWSEndpoint
      ( long "s3-endpoint"
          <> metavar "URL"
          <> help "S3 endpoint"
          <> value (AWSEndpoint "localhost" False 4570)
          <> showDefault
      )
    <*> optional
      ( strOption
          ( long "s3-region"
              <> metavar "S3REGION"
              <> help "S3 region"
          )
      )
    <*> option
      addressingStyleParser
      ( long "s3-addressing-style"
          <> metavar "ADDRESSINGSTYLE"
          <> help "S3 addressing style (path for minio)"
          <> value S3AddressingStylePath
          <> showDefault
      )
    <*> strOption
      ( long "s3-bucket-name"
          <> metavar "BUCKET"
          <> help "S3 bucket"
      )
    <*> optional
      ( strOption
          ( long "s3-bucket-dir"
              <> metavar "DIRECTORY"
              <> help "S3 bucket directory"
          )
      )

addressingStyleParser :: ReadM S3AddressingStyle
addressingStyleParser = do
  readerAsk >>= \case
    "path" -> pure S3AddressingStylePath
    "auto" -> pure S3AddressingStyleAuto
    "virtual" -> pure S3AddressingStyleVirtual
    _ -> readerError "unknown S3 addressing style"
