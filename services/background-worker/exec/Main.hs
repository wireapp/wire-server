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

module Main where

import Imports
import OpenSSL (withOpenSSL)
import Options.Applicative
import Util.Options
import Wire.BackgroundWorker

configPathsParser :: FilePath -> FilePath -> Parser (FilePath, FilePath)
configPathsParser backgroundWorkerConfigPath defaultGalleyConfigPath =
  (,)
    <$> strOption
      ( long "config-file"
          <> short 'c'
          <> help "Config file to load"
          <> showDefault
          <> value backgroundWorkerConfigPath
      )
    <*> strOption
      ( long "galley-config-file"
          <> help "Galley config file to load"
          <> showDefault
          <> value defaultGalleyConfigPath
      )

main :: IO ()
main = withOpenSSL $ do
  let desc = "Background Worker"
      backgroundWorkerConfigPath = "/etc/wire/background-worker/conf/background-worker.yaml"
      defaultGalleyConfigPath = "/etc/wire/galley/conf/galley.yaml"
  (config, galleyConfig) <-
    execParser
      $ info
        (configPathsParser backgroundWorkerConfigPath defaultGalleyConfigPath <**> helper)
        (header desc <> fullDesc)
  backgroundWorkerOptions <- decodeConfigFile config
  galleyOptions <- decodeConfigFile galleyConfig
  run backgroundWorkerOptions galleyOptions
