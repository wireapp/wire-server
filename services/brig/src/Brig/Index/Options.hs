{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}

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

module Brig.Index.Options
  ( Command (..),
    ElasticSettings,
    esServer,
    esIndex,
    esIndexShardCount,
    esIndexReplicas,
    esIndexRefreshInterval,
    esDeleteTemplate,
    CassandraSettings,
    cHost,
    cPort,
    cKeyspace,
    localElasticSettings,
    localCassandraSettings,
    commandParser,
    mkCreateIndexSettings,
    toESServer,
    ReindexFromAnotherIndexSettings,
    reindexDestIndex,
    reindexSrcIndex,
    reindexEsServer,
    reindexTimeoutSeconds,
  )
where

import Brig.Index.Types (CreateIndexSettings (..))
import qualified Cassandra as C
import Control.Lens
import Data.ByteString.Lens
import Data.Text.Strict.Lens
import Data.Time.Clock (NominalDiffTime)
import qualified Database.Bloodhound as ES
import Imports
import Options.Applicative
import URI.ByteString
import URI.ByteString.QQ

data Command
  = Create ElasticSettings
  | Reset ElasticSettings
  | Reindex ElasticSettings CassandraSettings
  | ReindexSameOrNewer ElasticSettings CassandraSettings
  | -- | 'ElasticSettings' has shards and other settings that are not needed here.
    UpdateMapping (URIRef Absolute) ES.IndexName
  | Migrate ElasticSettings CassandraSettings
  | ReindexFromAnotherIndex ReindexFromAnotherIndexSettings
  deriving (Show)

data ElasticSettings = ElasticSettings
  { _esServer :: URIRef Absolute,
    _esIndex :: ES.IndexName,
    _esIndexShardCount :: Int,
    _esIndexReplicas :: ES.ReplicaCount,
    _esIndexRefreshInterval :: NominalDiffTime,
    _esDeleteTemplate :: Maybe ES.TemplateName
  }
  deriving (Show)

data CassandraSettings = CassandraSettings
  { _cHost :: String,
    _cPort :: Word16,
    _cKeyspace :: C.Keyspace
  }
  deriving (Show)

data ReindexFromAnotherIndexSettings = ReindexFromAnotherIndexSettings
  { _reindexEsServer :: URIRef Absolute,
    _reindexSrcIndex :: ES.IndexName,
    _reindexDestIndex :: ES.IndexName,
    _reindexTimeoutSeconds :: Int
  }
  deriving (Show)

makeLenses ''ElasticSettings

makeLenses ''CassandraSettings

makeLenses ''ReindexFromAnotherIndexSettings

mkCreateIndexSettings :: ElasticSettings -> CreateIndexSettings
mkCreateIndexSettings es =
  CreateIndexSettings
    [ ES.NumberOfReplicas $ _esIndexReplicas es,
      ES.RefreshInterval $ _esIndexRefreshInterval es
    ]
    (_esIndexShardCount es)
    (_esDeleteTemplate es)

localElasticSettings :: ElasticSettings
localElasticSettings =
  ElasticSettings
    { _esServer = [uri|http://localhost:9200|],
      _esIndex = ES.IndexName "directory_test",
      _esIndexShardCount = 1,
      _esIndexReplicas = ES.ReplicaCount 1,
      _esIndexRefreshInterval = 1,
      _esDeleteTemplate = Nothing
    }

localCassandraSettings :: CassandraSettings
localCassandraSettings =
  CassandraSettings
    { _cHost = "localhost",
      _cPort = 9042,
      _cKeyspace = C.Keyspace "brig_test"
    }

elasticServerParser :: Parser (URIRef Absolute)
elasticServerParser =
  option
    url
    ( long "elasticsearch-server"
        <> metavar "URL"
        <> help "Base URL of the Elasticsearch Server."
        <> value (view esServer localElasticSettings)
        <> showDefaultWith (view unpackedChars . serializeURIRef')
    )
  where
    url =
      eitherReader
        (over _Left show . parseURI strictURIParserOptions . view packedChars)

restrictedElasticSettingsParser :: Parser ElasticSettings
restrictedElasticSettingsParser = do
  server <- elasticServerParser
  pure $ localElasticSettings & esServer .~ server

indexNameParser :: Parser ES.IndexName
indexNameParser =
  ES.IndexName . view packed
    <$> strOption
      ( long "elasticsearch-index"
          <> metavar "STRING"
          <> help "Elasticsearch Index Name."
          <> value (view (esIndex . _IndexName . unpacked) localElasticSettings)
          <> showDefault
      )

elasticSettingsParser :: Parser ElasticSettings
elasticSettingsParser =
  ElasticSettings
    <$> elasticServerParser
    <*> indexNameParser
    <*> indexShardCountParser
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
        <$$> ( optional
                 ( option
                     str
                     ( long "delete-template"
                         <> metavar "TEMPLATE_NAME"
                         <> help "Delete this ES template before creating a new index"
                     )
                 )
             )

cassandraSettingsParser :: Parser CassandraSettings
cassandraSettingsParser =
  CassandraSettings
    <$> ( strOption
            ( long "cassandra-host"
                <> metavar "HOST"
                <> help "Cassandra Host."
                <> value (_cHost localCassandraSettings)
                <> showDefault
            )
        )
    <*> option
      auto
      ( long "cassandra-port"
          <> metavar "PORT"
          <> help "Cassandra Port."
          <> value (_cPort localCassandraSettings)
          <> showDefault
      )
    <*> ( C.Keyspace . view packed
            <$> strOption
              ( long "cassandra-keyspace"
                  <> metavar "STRING"
                  <> help "Cassandra Keyspace."
                  <> value (view (cKeyspace . _Keyspace . unpacked) localCassandraSettings)
                  <> showDefault
              )
        )

reindexToAnotherIndexSettingsParser :: Parser ReindexFromAnotherIndexSettings
reindexToAnotherIndexSettingsParser =
  ReindexFromAnotherIndexSettings
    <$> elasticServerParser
    <*> ( ES.IndexName . view packed
            <$> strOption
              ( long "source-index"
                  <> metavar "STRING"
                  <> help "Elasticsearch index name to reindex from"
              )
        )
    <*> ( ES.IndexName . view packed
            <$> strOption
              ( long "destination-index"
                  <> metavar "STRING"
                  <> help "Elasticsearch index name to reindex to"
              )
        )
    <*> ( option
            auto
            ( long "timeout"
                <> metavar "SECONDS"
                <> help "Number of seconds to wait for reindexing to complete. The reindexing will not be cancelled when this timeout expires."
                <> value 600
                <> showDefault
            )
        )

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command
        "create"
        ( info
            (Create <$> elasticSettingsParser)
            (progDesc ("Create the ES user index, if it doesn't already exist. "))
        )
        <> command
          "update-mapping"
          ( info
              (UpdateMapping <$> elasticServerParser <*> indexNameParser)
              (progDesc "Update mapping of the user index.")
          )
        <> command
          "reset"
          ( info
              (Reset <$> restrictedElasticSettingsParser)
              (progDesc ("Delete and re-create the ES user index. Only works on a test index (directory_test)."))
          )
        <> command
          "reindex"
          ( info
              (Reindex <$> elasticSettingsParser <*> cassandraSettingsParser)
              (progDesc "Reindex all users from Cassandra if there is a new version.")
          )
        <> command
          "reindex-if-same-or-newer"
          ( info
              (ReindexSameOrNewer <$> elasticSettingsParser <*> cassandraSettingsParser)
              (progDesc "Reindex all users from Cassandra, even if the version has not changed.")
          )
        <> command
          "migrate-data"
          ( info
              (Migrate <$> elasticSettingsParser <*> cassandraSettingsParser)
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

_IndexName :: Iso' ES.IndexName Text
_IndexName = iso (\(ES.IndexName n) -> n) ES.IndexName

_Keyspace :: Iso' C.Keyspace Text
_Keyspace = iso C.unKeyspace C.Keyspace

toESServer :: URIRef Absolute -> ES.Server
toESServer =
  ES.Server
    . view utf8
    . serializeURIRef'
    . set pathL mempty
    . set queryL mempty
    . set fragmentL mempty
