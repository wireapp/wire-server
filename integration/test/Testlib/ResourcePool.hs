module Testlib.ResourcePool
  ( ResourcePool,
    BackendResource (..),
    DynamicBackendConfig (..),
    backendResources,
    createBackendResourcePool,
    acquireResources,
  )
where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.Aeson
import Data.Function ((&))
import Data.Functor
import Data.IORef
import Data.Set qualified as Set
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple
import Data.Word
import Database.CQL.IO
import Database.CQL.IO qualified as Cas
import Database.CQL.Protocol qualified as Cas
import GHC.Generics hiding (R)
import System.IO
import Prelude

data ResourcePool a = ResourcePool
  { sem :: QSemN,
    resources :: IORef (Set.Set a),
    onAcquire :: a -> IO ()
  }

acquireResources :: forall m a. (Ord a, MonadIO m, MonadMask m) => Int -> ResourcePool a -> Codensity m [a]
acquireResources n pool = Codensity $ \f -> bracket acquire release $ \s -> do
  liftIO $ mapM_ pool.onAcquire s
  f $ Set.toList s
  where
    release :: Set.Set a -> m ()
    release s =
      liftIO $ do
        atomicModifyIORef pool.resources $ (,()) . Set.union s
        signalQSemN pool.sem (length s)

    acquire :: m (Set.Set a)
    acquire = liftIO $ do
      waitQSemN pool.sem n
      atomicModifyIORef pool.resources $ swap . Set.splitAt n

cleanupDB :: String -> Word16 -> BackendResource -> IO ()
cleanupDB cassandraHost cassandraPort br = do
  let settings =
        Cas.setContacts cassandraHost []
          . Cas.setPortNumber (fromIntegral cassandraPort)
          . Cas.setMaxConnections 4
          . Cas.setPoolStripes 4
          . Cas.setSendTimeout 3
          . Cas.setResponseTimeout 10
          . Cas.setProtocolVersion Cas.V4
          $ Cas.defSettings
  client <- Cas.init settings
  -- TODO: Also cleanup elasticsearch, redis and RabbitMQ
  Cas.runClient client $ do
    cleanupCassandra $ fromString br.berBrigKeyspace
    cleanupCassandra $ fromString br.berGalleyKeyspace
    cleanupCassandra $ fromString br.berSparKeyspace
    cleanupCassandra $ fromString br.berGundeckKeyspace

type KeyspaceName = Text

type TableName = Text

cleanupCassandra :: MonadClient m => KeyspaceName -> m ()
cleanupCassandra keyspace = do
  tables <-
    map (\t -> keyspace <> fromString "." <> t)
      . filter (/= fromString "meta")
      <$> listTables keyspace
  mapM_ truncateTable tables

listTables :: MonadClient m => KeyspaceName -> m [TableName]
listTables keyspace = do
  let params = QueryParams One False (Identity keyspace) Nothing Nothing Nothing Nothing
  map runIdentity <$> query cql params
  where
    cql :: PrepQuery R (Identity KeyspaceName) (Identity TableName)
    cql = fromString "SELECT table_name FROM system_schema.tables where keyspace_name = ?"

truncateTable :: MonadClient m => TableName -> m ()
truncateTable table = do
  let params = QueryParams One False () Nothing Nothing Nothing Nothing
  write cql params
  where
    -- Cannot use `?` here because CQL doesn't allow using it for table name.
    cql :: PrepQuery W () ()
    cql = fromString $ "TRUNCATE TABLE " <> Text.unpack table

createBackendResourcePool :: String -> Word16 -> [DynamicBackendConfig] -> IO (ResourcePool BackendResource)
createBackendResourcePool cassandraHost cassandraPort dynConfs =
  let resources = backendResources dynConfs
   in ResourcePool
        <$> newQSemN (length dynConfs)
        <*> newIORef resources
        <*> pure (cleanupDB cassandraHost cassandraPort)

data BackendResource = BackendResource
  { berBrigKeyspace :: String,
    berGalleyKeyspace :: String,
    berSparKeyspace :: String,
    berGundeckKeyspace :: String,
    berElasticsearchIndex :: String,
    berFederatorInternal :: Word16,
    berFederatorExternal :: Word16,
    berDomain :: String,
    berAwsUserJournalQueue :: String,
    berAwsPrekeyTable :: String,
    berAwsS3Bucket :: String,
    berAwsQueueName :: String,
    berBrigInternalEvents :: String,
    berEmailSMSSesQueue :: String,
    berEmailSMSEmailSender :: String,
    berGalleyJournal :: String,
    berVHost :: String,
    berNginzSslPort :: Word16
  }
  deriving (Show, Eq, Ord)

data DynamicBackendConfig = DynamicBackendConfig
  { domain :: String,
    federatorExternalPort :: Word16
  }
  deriving (Show, Generic)

instance FromJSON DynamicBackendConfig

backendResources :: [DynamicBackendConfig] -> Set.Set BackendResource
backendResources dynConfs =
  (zip dynConfs [1 ..])
    <&> ( \(dynConf, i) ->
            BackendResource
              { berBrigKeyspace = "brig_test_dyn_" <> show i,
                berGalleyKeyspace = "galley_test_dyn_" <> show i,
                berSparKeyspace = "spar_test_dyn_" <> show i,
                berGundeckKeyspace = "gundeck_test_dyn_" <> show i,
                berElasticsearchIndex = "directory_dyn_" <> show i <> "_test",
                berFederatorInternal = federatorInternalPort i,
                berFederatorExternal = dynConf.federatorExternalPort,
                berDomain = dynConf.domain,
                berAwsUserJournalQueue = "integration-user-events.fifo" <> suffix i,
                berAwsPrekeyTable = "integration-brig-prekeys" <> suffix i,
                berAwsS3Bucket = "dummy-bucket" <> suffix i,
                berAwsQueueName = "integration-gundeck-events" <> suffix i,
                berBrigInternalEvents = "integration-brig-events-internal" <> suffix i,
                berEmailSMSSesQueue = "integration-brig-events" <> suffix i,
                berEmailSMSEmailSender = "backend-integration" <> suffix i <> "@wire.com",
                berGalleyJournal = "integration-team-events.fifo" <> suffix i,
                berVHost = dynConf.domain,
                berNginzSslPort = mkNginzSslPort i
              }
        )
    & Set.fromList
  where
    suffix :: Word16 -> String
    suffix i = show $ i + 2

    mkNginzSslPort :: Word16 -> Word16
    mkNginzSslPort i = 8443 + ((1 + i) * 1000)

    -- Fixed internal port for federator, e.g. for dynamic backends: 1 -> 10097, 2 -> 11097, etc.
    federatorInternalPort :: Num a => a -> a
    federatorInternalPort i = 8097 + ((1 + i) * 1000)
