{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}
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

module Brig.Index.Options
  ( Command (..),
    ElasticIndexSettings (..),
    esIndexShardCount,
    esIndexReplicas,
    esIndexRefreshInterval,
    esDeleteTemplate,
    commandParser,
    mkCreateIndexSettings,
    ReindexFromAnotherIndexSettings (..),
    reindexDestIndex,
    reindexTimeoutSeconds,
  )
where

import Brig.Index.Types (CreateIndexSettings (..))
import Control.Lens
import Data.Text.Strict.Lens
import Data.Time.Clock (NominalDiffTime)
import Database.Bloodhound qualified as ES
import Imports
import Options.Applicative

-- | Commands for brig-index. Connection settings (ES, Cassandra, Galley) come from brig.yaml.
-- Index-specific settings (shards, replicas) come from CLI only where needed.
data Command
  = Create ElasticIndexSettings
  | Reset
  | Reindex
  | ReindexSameOrNewer
  | UpdateMapping
  | Migrate
  | ReindexFromAnotherIndex ReindexFromAnotherIndexSettings
  deriving (Show)

-- | Index-specific settings that come from CLI. Connection settings come from brig.yaml.
data ElasticIndexSettings = ElasticIndexSettings
  { _esIndexShardCount :: Int,
    _esIndexReplicas :: ES.ReplicaCount,
    _esIndexRefreshInterval :: NominalDiffTime,
    _esDeleteTemplate :: Maybe ES.TemplateName
  }
  deriving (Show)

-- | Settings for ReindexFromAnotherIndex command.
-- ES connection settings come from brig.yaml; only destination index and timeout come from CLI.
data ReindexFromAnotherIndexSettings = ReindexFromAnotherIndexSettings
  { _reindexDestIndex :: ES.IndexName,
    _reindexTimeoutSeconds :: Int
  }
  deriving (Show)

makeLenses ''ElasticIndexSettings

makeLenses ''ReindexFromAnotherIndexSettings

mkCreateIndexSettings :: ElasticIndexSettings -> CreateIndexSettings
mkCreateIndexSettings es =
  CreateIndexSettings
    [ ES.NumberOfReplicas $ _esIndexReplicas es,
      ES.RefreshInterval $ _esIndexRefreshInterval es
    ]
    (_esIndexShardCount es)
    (_esDeleteTemplate es)

--------------------------------------------------------------------------------
-- Parsers

elasticIndexSettingsParser :: Parser ElasticIndexSettings
elasticIndexSettingsParser =
  ElasticIndexSettings
    <$> indexShardCountParser
    <*> indexReplicaCountParser
    <*> indexRefreshIntervalParser
    <*> templateParser
  where
    indexShardCountParser =
      option
        auto
        ( long "elasticsearch-shards"
            <> metavar "INT"
            <> help "Number of Shards for the Elasticsearch Index."
            <> value 1
            <> showDefault
        )
    indexReplicaCountParser =
      ES.ReplicaCount
        <$> option
          auto
          ( long "elasticsearch-replicas"
              <> metavar "INT"
              <> help "Number of Replicas for the Elasticsearch Index."
              <> value 1
              <> showDefault
          )
    indexRefreshIntervalParser =
      fromInteger
        <$> option
          auto
          ( long "elasticsearch-refresh-interval"
              <> metavar "SECONDS"
              <> help "Refresh interval for the Elasticsearch Index in seconds"
              <> value 1
              <> showDefault
          )
    templateParser :: Parser (Maybe ES.TemplateName) =
      ES.TemplateName
        <$$> optional
          ( option
              str
              ( long "delete-template"
                  <> metavar "TEMPLATE_NAME"
                  <> help "Delete this ES template before creating a new index"
              )
          )

-- | Parser for ReindexFromAnotherIndex.
-- ES connection settings come from brig.yaml; only destination index and timeout from CLI.
reindexToAnotherIndexSettingsParser :: Parser ReindexFromAnotherIndexSettings
reindexToAnotherIndexSettingsParser =
  ReindexFromAnotherIndexSettings
    <$> ( ES.IndexName . view packed
            <$> strOption
              ( long "destination-index"
                  <> metavar "STRING"
                  <> help "Elasticsearch index name to reindex to"
              )
        )
    <*> option
      auto
      ( long "timeout"
          <> metavar "SECONDS"
          <> help "Number of seconds to wait for reindexing to complete. The reindexing will not be cancelled when this timeout expires."
          <> value 600
          <> showDefault
      )

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "create"
        ( info
            (Create <$> elasticIndexSettingsParser)
            (progDesc "Create the ES user index, if it doesn't already exist.")
        )
        <> command
          "update-mapping"
          ( info
              (pure UpdateMapping)
              (progDesc "Update mapping of the user index.")
          )
        <> command
          "reset"
          ( info
              (pure Reset)
              (progDesc "Delete and re-create the ES user index. Only works on a test index (index name ending with '_test').")
          )
        <> command
          "reindex"
          ( info
              (pure Reindex)
              (progDesc "Reindex all users from Cassandra if there is a new version.")
          )
        <> command
          "reindex-if-same-or-newer"
          ( info
              (pure ReindexSameOrNewer)
              (progDesc "Reindex all users from Cassandra, even if the version has not changed.")
          )
        <> command
          "migrate-data"
          ( info
              (pure Migrate)
              (progDesc "Migrate data in elastic search")
          )
        <> command
          "reindex-from-another-index"
          ( info
              (ReindexFromAnotherIndex <$> reindexToAnotherIndexSettingsParser)
              ( progDesc
                  "Reindex data from an index to another. More about migrating to a new index here: https://github.com/wireapp/wire-server/blob/develop/docs/reference/elastic-search.md"
              )
          )
    )
