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
    toESServer,
    ReindexFromAnotherIndexSettings (..),
    reindexEsServer,
    reindexEsIndex,
    reindexEsCaCert,
    reindexEsInsecureSkipVerifyTls,
    reindexEsCredentials,
    reindexDestIndex,
    reindexTimeoutSeconds,
  )
where

import Brig.Index.Types (CreateIndexSettings (..))
import Control.Lens
import Data.ByteString.Lens
import Data.Text.Strict.Lens
import Data.Time.Clock (NominalDiffTime)
import Database.Bloodhound qualified as ES
import Imports
import Options.Applicative
import URI.ByteString
import URI.ByteString.QQ
import Util.Options (FilePathSecrets)

-- | Commands for brig-index. Connection settings (ES, Cassandra, Galley) come from brig.yaml.
-- Index-specific settings (shards, replicas, prefix for test indices) come from CLI.
data Command
  = Create ElasticIndexSettings
  | Reset Text -- ^ Index prefix for test indices (actual index will be PREFIX_test)
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

makeLenses ''ElasticIndexSettings

-- | Connection settings for ReindexFromAnotherIndex command.
-- This command operates on arbitrary ES servers/indices specified via CLI args,
-- not from brig.yaml, so it needs its own settings type.
data ReindexFromAnotherIndexSettings = ReindexFromAnotherIndexSettings
  { _reindexEsServer :: URIRef Absolute,
    _reindexEsIndex :: ES.IndexName,
    _reindexEsCaCert :: Maybe FilePath,
    _reindexEsInsecureSkipVerifyTls :: Bool,
    _reindexEsCredentials :: Maybe FilePathSecrets,
    _reindexDestIndex :: ES.IndexName,
    _reindexTimeoutSeconds :: Int
  }
  deriving (Show)

makeLenses ''ReindexFromAnotherIndexSettings

mkCreateIndexSettings :: ElasticIndexSettings -> CreateIndexSettings
mkCreateIndexSettings es =
  CreateIndexSettings
    [ ES.NumberOfReplicas $ _esIndexReplicas es,
      ES.RefreshInterval $ _esIndexRefreshInterval es
    ]
    (_esIndexShardCount es)
    (_esDeleteTemplate es)

toESServer :: URIRef Absolute -> ES.Server
toESServer =
  ES.Server
    . view utf8
    . serializeURIRef'
    . set pathL mempty
    . set queryL mempty
    . set fragmentL mempty

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

indexPrefixParser :: Parser Text
indexPrefixParser =
  strOption
    ( long "elasticsearch-index-prefix"
        <> metavar "PREFIX"
        <> help "Elasticsearch Index Prefix. The actual index name will be PREFIX_test."
        <> value "directory"
        <> showDefault
    )

-- | Parser for ReindexFromAnotherIndex, which needs full ES connection settings
-- from CLI args since it operates on arbitrary indices not configured in brig.yaml.
reindexToAnotherIndexSettingsParser :: Parser ReindexFromAnotherIndexSettings
reindexToAnotherIndexSettingsParser =
  ReindexFromAnotherIndexSettings
    <$> elasticServerParser
    <*> indexNameParser
    <*> caCertParser
    <*> verifyCaParser
    <*> credentialsPathParser
    <*> ( ES.IndexName . view packed
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
  where
    elasticServerParser =
      option
        url
        ( long "elasticsearch-server"
            <> metavar "URL"
            <> help "Base URL of the Elasticsearch Server."
            <> value [uri|https://localhost:9200|]
            <> showDefaultWith (view unpackedChars . serializeURIRef')
        )
      where
        url =
          eitherReader
            (over _Left show . parseURI strictURIParserOptions . view packedChars)

    indexNameParser =
      ES.IndexName . view packed
        <$> strOption
          ( long "elasticsearch-index"
              <> metavar "STRING"
              <> help "Elasticsearch Index Name."
              <> value "directory_test"
              <> showDefault
          )

    caCertParser =
      optional
        ( option
            str
            ( long "elasticsearch-ca-cert"
                <> metavar "FILE"
                <> help "Path to CA Certificate for TLS validation, system CA bundle is used when unspecified"
            )
        )

    verifyCaParser =
      flag
        False
        True
        ( long "elasticsearch-insecure-skip-tls-verify"
            <> help "Skip TLS verification when connecting to Elasticsearch (not recommended)"
        )

    credentialsPathParser =
      optional
        ( strOption
            ( long "elasticsearch-credentials"
                <> metavar "FILE"
                <> help "Location of a file containing the Elasticsearch credentials"
            )
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
              (Reset <$> indexPrefixParser)
              (progDesc "Delete and re-create the ES user index. Only works on a test index (PREFIX_test).")
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
