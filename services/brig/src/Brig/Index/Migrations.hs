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

module Brig.Index.Migrations
  ( migrate,
  )
where

import Brig.Index.Migrations.Types
import qualified Brig.Index.Options as Opts
import qualified Brig.User.Search.Index as Search
import qualified Cassandra as C
import qualified Cassandra.Settings as C
import Control.Lens (view, (^.))
import Control.Monad.Catch (Exception, MonadThrow, finally, throwM)
import Data.Aeson (Value, object, (.=))
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
        runMigration expectedMigrationVersion

-- | Increase this number any time you want to force reindexing.
expectedMigrationVersion :: MigrationVersion
expectedMigrationVersion = MigrationVersion 2

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
      C.init $
        C.setLogger (C.mkLogger l)
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
      unless (ES.isSuccess response) $
        throwM $
          err (show response)

failIfIndexAbsent :: (MonadThrow m, MonadIO m, ES.MonadBH m) => ES.IndexName -> m ()
failIfIndexAbsent targetIndex =
  unlessM
    (ES.indexExists targetIndex)
    (throwM $ TargetIndexAbsent targetIndex)

-- | Runs only the migrations which need to run
runMigration :: MigrationVersion -> MigrationActionT IO ()
runMigration ver = do
  vmax <- latestMigrationVersion
  if ver > vmax
    then do
      info "Migration necessary."
      Search.reindexAllIfSameOrNewer
      persistVersion ver
    else do
      info "No migration necessary."

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
