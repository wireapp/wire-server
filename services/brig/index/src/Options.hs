{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}

module Options
  ( Command (..),
    ElasticSettings,
    esServer,
    esIndex,
    esIndexShardCount,
    esIndexReplicas,
    esIndexRefreshInterval,
    CassandraSettings,
    cHost,
    cPort,
    cKeyspace,
    localElasticSettings,
    localCassandraSettings,
    commandParser,
    mkCreateIndexSettings,
  )
where

import qualified Cassandra as C
import Control.Lens
import Data.ByteString.Lens
import Data.Text.Strict.Lens
import Data.Time.Clock (NominalDiffTime)
import qualified Database.V5.Bloodhound as ES
import Imports
import Options.Applicative
import URI.ByteString
import URI.ByteString.QQ

data Command
  = Create ElasticSettings
  | Reset ElasticSettings
  | Reindex ElasticSettings CassandraSettings
  deriving (Show)

data ElasticSettings
  = ElasticSettings
      { _esServer :: URIRef Absolute,
        _esIndex :: ES.IndexName,
        _esIndexShardCount :: Int,
        _esIndexReplicas :: ES.ReplicaCount,
        _esIndexRefreshInterval :: NominalDiffTime
      }
  deriving (Show)

data CassandraSettings
  = CassandraSettings
      { _cHost :: String,
        _cPort :: Word16,
        _cKeyspace :: C.Keyspace
      }
  deriving (Show)

makeLenses ''ElasticSettings

makeLenses ''CassandraSettings

mkCreateIndexSettings :: ElasticSettings -> ([ES.UpdatableIndexSetting], Int)
mkCreateIndexSettings es =
  ( [ ES.NumberOfReplicas $ _esIndexReplicas es,
      ES.RefreshInterval $ _esIndexRefreshInterval es
    ],
    _esIndexShardCount es
  )

localElasticSettings :: ElasticSettings
localElasticSettings =
  ElasticSettings
    { _esServer = [uri|http://localhost:9200|],
      _esIndex = ES.IndexName "directory_test",
      _esIndexShardCount = 1,
      _esIndexReplicas = ES.ReplicaCount 1,
      _esIndexRefreshInterval = 1 -- seconds
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

elasticSettingsParser :: Parser ElasticSettings
elasticSettingsParser =
  ElasticSettings
    <$> elasticServerParser
    <*> indexNameParser
    <*> indexShardCountParser
    <*> indexReplicaCountParser
    <*> indexRefreshIntervalParser
  where
    indexNameParser =
      ES.IndexName . view packed
        <$> strOption
          ( long "elasticsearch-index"
              <> metavar "STRING"
              <> help "Elasticsearch Index Name."
              <> value (view (esIndex . _IndexName . unpacked) localElasticSettings)
              <> showDefault
          )
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
          "reset"
          ( info
              (Reset <$> restrictedElasticSettingsParser)
              (progDesc ("Delete and re-create the ES user index. Only works on a test index (directory_test)."))
          )
        <> command
          "reindex"
          ( info
              (Reindex <$> elasticSettingsParser <*> cassandraSettingsParser)
              (progDesc "Reindex all users from Cassandra.")
          )
    )

_IndexName :: Iso' ES.IndexName Text
_IndexName = iso (\(ES.IndexName n) -> n) ES.IndexName

_Keyspace :: Iso' C.Keyspace Text
_Keyspace = iso C.unKeyspace C.Keyspace
