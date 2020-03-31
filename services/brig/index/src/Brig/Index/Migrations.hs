module Brig.Index.Migrations (migrate) where

import Brig.Index.Migrations.Types
import qualified Brig.Index.Migrations.V1_ReIndexForSearchTeamMembers as V1_ReIndexForSearchTeamMembers
import qualified Brig.Index.Options as Opts
import qualified Cassandra as C
import qualified Cassandra.Settings as C
import Control.Lens ((^.), view)
import Control.Monad.Catch (Exception, MonadThrow, finally, throwM)
import Data.Aeson ((.=), Value, object)
import qualified Data.Metrics as Metrics
import qualified Data.Text as Text
import qualified Database.Bloodhound as ES
import Imports
import qualified Network.HTTP.Client as HTTP
import System.Logger.Class (Logger)
import qualified System.Logger.Class as Log

migrate :: Logger -> Opts.ElasticSettings -> Opts.CassandraSettings -> IO ()
migrate l es cas = do
  env <- mkEnv l es cas
  finally (go env) (cleanup env)
  where
    go env =
      runMigrationAction env $ do
        failIfIndexAbsent (es ^. Opts.esIndex)
        createMigrationsIndexIfNotPresent
        runMigrations [V1_ReIndexForSearchTeamMembers.migration]

indexName :: ES.IndexName
indexName = ES.IndexName "wire_brig_migrations"

indexMappingName :: ES.MappingName
indexMappingName = ES.MappingName "wire_brig_migrations"

indexMapping :: Value
indexMapping =
  object
    [ "properties"
        .= object
          ["migration_version" .= object ["index" .= True, "type" .= ("integer" :: Text)]]
    ]

mkEnv :: Logger -> Opts.ElasticSettings -> Opts.CassandraSettings -> IO Env
mkEnv l es cas =
  Env
    <$> initES
      <*> initCassandra
      <*> initLogger
      <*> Metrics.metrics
      <*> (pure $ view Opts.esIndex es)
  where
    initES =
      ES.mkBHEnv
        (Opts.toESServer (es ^. Opts.esServer))
        <$> HTTP.newManager HTTP.defaultManagerSettings
    initCassandra =
      C.init
        $ C.setLogger (C.mkLogger l)
          . C.setContacts (view Opts.cHost cas) []
          . C.setPortNumber (fromIntegral (view Opts.cPort cas))
          . C.setKeyspace (view Opts.cKeyspace cas)
          . C.setProtocolVersion C.V4
        $ C.defSettings
    initLogger = pure l

createMigrationsIndexIfNotPresent :: (MonadThrow m, MonadIO m, ES.MonadBH m) => m ()
createMigrationsIndexIfNotPresent =
  do
    unlessM (ES.indexExists indexName) $
      ES.createIndexWith [] 1 indexName
        >>= throwIfNotCreated CreateMigrationIndexFailed
    ES.putMapping indexName indexMappingName indexMapping
      >>= throwIfNotCreated PutMappingFailed
  where
    throwIfNotCreated err response =
      unless (ES.isSuccess response)
        $ throwM
        $ err (show response)

failIfIndexAbsent :: (MonadThrow m, MonadIO m, ES.MonadBH m) => ES.IndexName -> m ()
failIfIndexAbsent targetIndex =
  unlessM
    (ES.indexExists targetIndex)
    (throwM $ TargetIndexAbsent targetIndex)

-- | Runs only the migrations which need to run
runMigrations :: [Migration] -> MigrationActionT IO ()
runMigrations migrations = do
  vmax <- latestMigrationVersion
  let pendingMigrations = filter (\m -> version m > vmax) migrations
  if null pendingMigrations
    then info "No new migrations."
    else info "New migrations found."
  mapM_ runMigration pendingMigrations

runMigration :: Migration -> MigrationActionT IO ()
runMigration (Migration ver txt mig) = do
  info $ "Running: [" <> show (migrationVersion ver) <> "] " <> Text.unpack txt
  mig
  persistVersion ver

persistVersion :: (Monad m, MonadThrow m, MonadIO m) => MigrationVersion -> MigrationActionT m ()
persistVersion v =
  let docId = ES.DocId . Text.pack . show $ migrationVersion v
   in do
        persistResponse <- ES.indexDocument indexName indexMappingName ES.defaultIndexDocumentSettings v docId
        case ES.isCreated persistResponse of
          False -> throwM $ PersistVersionFailed v $ show persistResponse
          True -> pure ()

latestMigrationVersion :: (Monad m, MonadThrow m, MonadIO m) => MigrationActionT m MigrationVersion
latestMigrationVersion = do
  resp <- ES.parseEsResponse =<< ES.searchByIndex indexName (ES.mkSearch Nothing Nothing)
  result <- either (throwM . FetchMigrationVersionsFailed . show) pure resp
  let versions = map ES.hitSource $ ES.hits . ES.searchHits $ result
  case versions of
    [] ->
      pure $ MigrationVersion 0
    vs ->
      if any isNothing vs
        then throwM $ VersionSourceMissing result
        else pure $ maximum $ catMaybes vs

data MigrationException
  = CreateMigrationIndexFailed String
  | FetchMigrationVersionsFailed String
  | PersistVersionFailed MigrationVersion String
  | PutMappingFailed String
  | TargetIndexAbsent ES.IndexName
  | VersionSourceMissing (ES.SearchResult MigrationVersion)
  deriving (Show)

instance Exception MigrationException

info :: Log.MonadLogger m => String -> m ()
info = Log.info . Log.msg
