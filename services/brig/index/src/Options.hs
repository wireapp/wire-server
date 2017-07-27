{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Options
    ( Command (..)

    , ElasticSettings
    , esServer
    , esIndex
    , esIndexSettings

    , CassandraSettings
    , cHost
    , cPort
    , cKeyspace

    , localElasticSettings
    , localCassandraSettings

    , commandParser
    )
where

import qualified Cassandra              as C
import           Control.Lens
import           Data.Bifunctor
import           Data.ByteString.Lens
import           Data.Monoid
import           Data.Text              (Text)
import           Data.Text.Strict.Lens
import           Data.Word
import qualified Database.V5.Bloodhound as ES
import           Options.Applicative
import           URI.ByteString
import           URI.ByteString.QQ

data Command
    = Create  ElasticSettings
    | Reset   ElasticSettings
    | Reindex ElasticSettings CassandraSettings
    deriving Show

data ElasticSettings = ElasticSettings
    { _esServer        :: URIRef Absolute
    , _esIndex         :: ES.IndexName
    , _esIndexSettings :: ES.IndexSettings
    } deriving Show

data CassandraSettings = CassandraSettings
    { _cHost     :: String
    , _cPort     :: Word16
    , _cKeyspace :: C.Keyspace
    } deriving Show

makeLenses ''ElasticSettings
makeLenses ''CassandraSettings

localElasticSettings :: ElasticSettings
localElasticSettings = ElasticSettings
    { _esServer        = [uri|http://localhost:9200|]
    , _esIndex         = ES.IndexName "directory_test"
    , _esIndexSettings = ES.IndexSettings
        { ES.indexShards   = ES.ShardCount 1
        , ES.indexReplicas = ES.ReplicaCount 1
        }
    }

localCassandraSettings :: CassandraSettings
localCassandraSettings = CassandraSettings
    { _cHost     = "localhost"
    , _cPort     = 9042
    , _cKeyspace = C.Keyspace "brig_test"
    }

elasticSettingsParser :: Parser ElasticSettings
elasticSettingsParser = ElasticSettings
    <$> option url
        ( long "elasticsearch-server"
       <> metavar "URL"
       <> help    "Base URL of the Elasticsearch Server."
       <> value   (view esServer localElasticSettings)
       <> showDefaultWith (view unpackedChars . serializeURIRef')
        )
    <*> ( ES.IndexName . view packed <$>
          strOption
          ( long    "elasticsearch-index"
         <> metavar "STRING"
         <> help    "Elasticsearch Index Name."
         <> value   (view (esIndex . _IndexName . unpacked) localElasticSettings)
         <> showDefault
          )
        )
    <*> indexSettingsParser
  where
    url = eitherReader
        (first show . parseURI strictURIParserOptions . view packedChars)

    indexSettingsParser = ES.IndexSettings
        <$> ( ES.ShardCount <$>
              option auto
              ( long    "elasticsearch-shards"
             <> metavar "INT"
             <> help    "Number of Shards for the Elasticsearch Index."
             <> value   1
             <> showDefault
              )
            )
        <*> ( ES.ReplicaCount <$>
              option auto
              ( long    "elasticsearch-replicas"
             <> metavar "INT"
             <> help    "Number of Replicas for the Elasticsearch Index."
             <> value   1
             <> showDefault
              )
            )

cassandraSettingsParser :: Parser CassandraSettings
cassandraSettingsParser = CassandraSettings
    <$> ( strOption
          ( long    "cassandra-host"
         <> metavar "HOST"
         <> help    "Cassandra Host."
         <> value   (_cHost localCassandraSettings)
         <> showDefault
          )
        )
    <*> option auto
        ( long    "cassandra-port"
       <> metavar "PORT"
       <> help    "Cassandra Port."
       <> value   (_cPort localCassandraSettings)
       <> showDefault
        )
    <*> ( C.Keyspace . view packed <$>
          strOption
          ( long    "cassandra-keyspace"
         <> metavar "STRING"
         <> help    "Cassandra Keyspace."
         <> value   (view (cKeyspace . _Keyspace . unpacked) localCassandraSettings)
         <> showDefault
          )
        )

commandParser :: Parser Command
commandParser = hsubparser
    ( command "create"
        (info (Create <$> pure localElasticSettings)
            (progDesc ("Create the ES user index, if it doesn't already exist. " <> lo)))
   <> command "reset"
        (info (Reset <$> pure localElasticSettings)
            (progDesc ("Delete and re-create the ES user index. " <> lo)))
   <> command "reindex"
        (info (Reindex <$> elasticSettingsParser <*> cassandraSettingsParser)
            (progDesc "Reindex all users from Cassandra."))
    )
  where
    lo = "Only works on a local test index (directory_test)."

_IndexName :: Iso' ES.IndexName Text
_IndexName = iso (\(ES.IndexName n) -> n) ES.IndexName

_Keyspace :: Iso' C.Keyspace Text
_Keyspace = iso C.unKeyspace C.Keyspace

