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

data Settings = Settings
  { setCasBrig :: CassandraSettings,
    setIncosistenciesFile :: FilePath
  }
  deriving (Show)

data CassandraSettings = CassandraSettings
  { cHosts :: !String,
    cPort :: !Word16,
    cKeyspace :: !C.Keyspace
  }
  deriving (Show)

data Command
  = DanglingHandles (Maybe (FilePath, Bool))
  | HandleLessUsers
  | DanglingUserKeys (Maybe (FilePath, Bool))
  | EmailUnparseableUsers
  | MissingEmailUserKeys (Maybe (FilePath, Bool))

optionsParser :: Parser (Command, Settings)
optionsParser = (,) <$> commandParser <*> settingsParser

commandParser :: Parser Command
commandParser =
  subparser $
    danglingHandlesCommand <> handleLessUsersCommand <> danglingKeysCommand <> unparseableEmailsCommand <> missingEmailsCommand

danglingHandlesCommand :: Mod CommandFields Command
danglingHandlesCommand = command "dangling-handles" (info (DanglingHandles <$> optional (inputFileRepairParser "handles")) (progDesc "find handle which shouldn't be claimed"))

danglingKeysCommand :: Mod CommandFields Command
danglingKeysCommand = command "dangling-keys" (info (DanglingUserKeys <$> optional (inputFileRepairParser "keys")) (progDesc "find keys which shouldn't be there"))

missingEmailsCommand :: Mod CommandFields Command
missingEmailsCommand = command "missing-email-keys" (info (MissingEmailUserKeys <$> optional (inputFileRepairParser "emails")) (progDesc "find missing email keys (users with emails inside user table but not inside user_keys table)"))

unparseableEmailsCommand :: Mod CommandFields Command
unparseableEmailsCommand = command "unparseable-emails" (info (pure EmailUnparseableUsers) (progDesc "find users with an email stored that cannot be parsed)"))

handleLessUsersCommand :: Mod CommandFields Command
handleLessUsersCommand = command "handle-less-users" (info (pure HandleLessUsers) (progDesc "find users which have a handle in the user table but not in the user_handle table"))

settingsParser :: Parser Settings
settingsParser =
  Settings
    <$> cassandraSettingsParser "brig"
    <*> inconsistenciesFileParser

inputFileRepairParser :: String -> Parser (FilePath, Bool)
inputFileRepairParser s =
  (,) <$> inputFileParser s <*> repairDataParser

inputFileParser :: String -> Parser FilePath
inputFileParser s =
  strOption
    ( long "input-file"
        <> help ("file containing list of " <> s <> " separated by new lines")
        <> metavar "FILEPATH"
    )

repairDataParser :: Parser Bool
repairDataParser =
  switch
    ( long "repair-data"
        <> help "Automatically repair data"
    )

inconsistenciesFileParser :: Parser FilePath
inconsistenciesFileParser =
  strOption
    ( long "inconsistencies-file"
        <> help "File to output the found inconsistencies"
        <> metavar "FILEPATH"
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
    <*> ( C.Keyspace . Text.pack
            <$> strOption
              ( long ("cassandra-keyspace-" ++ ks)
                  <> metavar "STRING"
                  <> help ("Cassandra Keyspace for: " ++ ks)
                  <> value (ks ++ "_test")
                  <> showDefault
              )
        )
