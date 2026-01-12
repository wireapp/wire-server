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
import Data.Yaml qualified as Yaml
import Imports
import Options.Applicative
import System.Exit
import System.Logger.Class qualified as Log

main :: IO ()
main = do
  opts <- execParser (info (helper <*> mainParser) desc)
  lgr <- initLogger
  case opts of
    -- YAML config mode: read from brig.yaml
    YamlMode configFile yamlCmd -> do
      brigIndexOpts <-
        Yaml.decodeFileEither configFile >>= \case
          Left e ->
            fail $
              "Failed to parse configuration file "
                <> configFile
                <> ": "
                <> show e
          Right o -> pure o
      cmd <- yamlCmdToCommand brigIndexOpts yamlCmd
      runCommand lgr cmd
    -- CLI mode: use traditional command-line arguments (backward compatible)
    CliMode cmd -> runCommand lgr cmd
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

-- | Commands that work with brig.yaml configuration
data YamlCommand
  = YamlCreate IndexSettings
  | YamlReset IndexSettings
  | YamlReindex
  | YamlReindexSameOrNewer
  | YamlUpdateMapping
  | YamlMigrate
  deriving (Show)

-- | Main options: either YAML config mode or CLI mode
data MainOpts
  = YamlMode FilePath YamlCommand
  | CliMode Command
  deriving (Show)

mainParser :: Parser MainOpts
mainParser =
  hsubparser
    ( command
        "yaml"
        ( info
            (yamlModeParser <**> helper)
            (progDesc "Run using brig.yaml configuration")
        )
    )
    <|> fmap CliMode commandParser

yamlModeParser :: Parser MainOpts
yamlModeParser =
  YamlMode
    <$> configFileParser
    <*> yamlCommandParser

configFileParser :: Parser FilePath
configFileParser =
  strOption
    ( long "config-file"
        <> short 'c'
        <> help "Path to brig.yaml configuration file"
        <> showDefault
        <> value "/etc/wire/brig/conf/brig.yaml"
    )

yamlCommandParser :: Parser YamlCommand
yamlCommandParser =
  hsubparser
    ( command
        "create"
        ( info
            (YamlCreate <$> indexSettingsParser)
            (progDesc "Create the ES user index, if it doesn't already exist")
        )
        <> command
          "reset"
          ( info
              (YamlReset <$> indexSettingsParser)
              (progDesc "Delete and re-create the ES user index. Only works on a test index (directory_test).")
          )
        <> command
          "reindex"
          ( info
              (pure YamlReindex)
              (progDesc "Reindex all users from Cassandra if there is a new version")
          )
        <> command
          "reindex-if-same-or-newer"
          ( info
              (pure YamlReindexSameOrNewer)
              (progDesc "Reindex all users from Cassandra, even if the version has not changed")
          )
        <> command
          "update-mapping"
          ( info
              (pure YamlUpdateMapping)
              (progDesc "Update mapping of the user index")
          )
        <> command
          "migrate-data"
          ( info
              (pure YamlMigrate)
              (progDesc "Migrate data in Elasticsearch")
          )
    )

indexSettingsParser :: Parser IndexSettings
indexSettingsParser =
  IndexSettings
    <$> option
      auto
      ( long "elasticsearch-shards"
          <> metavar "INT"
          <> help "Number of Shards for the Elasticsearch Index"
          <> value 5
          <> showDefault
      )
    <*> option
      auto
      ( long "elasticsearch-replicas"
          <> metavar "INT"
          <> help "Number of Replicas for the Elasticsearch Index"
          <> value 2
          <> showDefault
      )
    <*> option
      auto
      ( long "elasticsearch-refresh-interval"
          <> metavar "SECONDS"
          <> help "Refresh interval for the Elasticsearch Index in seconds"
          <> value 5
          <> showDefault
      )
    <*> optional
      ( strOption
          ( long "delete-template"
              <> metavar "TEMPLATE_NAME"
              <> help "Delete this ES template before creating a new index"
          )
      )

-- | Convert YamlCommand to Command using the options from brig.yaml
yamlCmdToCommand :: BrigIndexOpts -> YamlCommand -> IO Command
yamlCmdToCommand opts cmd = case cmd of
  YamlCreate idxSettings -> do
    esSettings <- getElasticSettings opts idxSettings
    pure $ Create esSettings opts.galley
  YamlReset idxSettings -> do
    esSettings <- getElasticSettings opts idxSettings
    pure $ Reset esSettings opts.galley
  YamlReindex -> do
    esSettings <- getElasticSettings opts defaultIndexSettings
    let casSettings = cassandraSettingsFromBrig opts.cassandra
    pure $ Reindex esSettings casSettings opts.galley
  YamlReindexSameOrNewer -> do
    esSettings <- getElasticSettings opts defaultIndexSettings
    let casSettings = cassandraSettingsFromBrig opts.cassandra
    pure $ ReindexSameOrNewer esSettings casSettings opts.galley
  YamlUpdateMapping -> do
    connSettings <- liftEither $ esConnectionSettingsFromBrig opts.elasticsearch
    pure $ UpdateMapping connSettings opts.galley
  YamlMigrate -> do
    esSettings <- getElasticSettings opts defaultIndexSettings
    let casSettings = cassandraSettingsFromBrig opts.cassandra
    pure $ Migrate esSettings casSettings opts.galley
  where
    liftEither :: Either String a -> IO a
    liftEither = either fail pure

    getElasticSettings :: BrigIndexOpts -> IndexSettings -> IO ElasticSettings
    getElasticSettings o idxSettings =
      liftEither $ elasticSettingsFromBrig o.elasticsearch idxSettings
