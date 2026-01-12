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

module Main
  ( main,
  )
where

import Brig.Index.Eval
import Brig.Index.Options
import Brig.Options qualified as BrigOpts
import Data.Yaml qualified as Yaml
import Imports
import Options.Applicative
import System.Exit
import System.Logger.Class qualified as Log

main :: IO ()
main = do
  (configFile, cmd) <- execParser (info (helper <*> mainParser) desc)
  lgr <- initLogger
  brigOpts <- loadBrigConfig configFile
  runCommand lgr brigOpts cmd
  exitSuccess
  where
    desc =
      header "brig-index"
        <> progDesc "Brig Search Index Utilities"
        <> fullDesc
    initLogger =
      Log.new -- TODO: use mkLogger'?
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings

-- | Load brig configuration from YAML file
loadBrigConfig :: FilePath -> IO BrigOpts.Opts
loadBrigConfig configFile =
  Yaml.decodeFileEither configFile >>= \case
    Left e ->
      fail $
        "Failed to parse configuration file "
          <> configFile
          <> ": "
          <> show e
    Right o -> pure o

-- | Main parser: config file + command
mainParser :: Parser (FilePath, Command)
mainParser =
  (,)
    <$> configFileParser
    <*> commandParser

configFileParser :: Parser FilePath
configFileParser =
  strOption
    ( long "config-file"
        <> short 'c'
        <> help "Path to brig.yaml configuration file"
        <> showDefault
        <> value "/etc/wire/brig/conf/brig.yaml"
    )
