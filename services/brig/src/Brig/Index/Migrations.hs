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

module Brig.Index.Migrations
  ( migrate,
  )
where

import Brig.App (initHttpManagerWithTLSConfig)
import Brig.Index.Migrations.Types
import Brig.Index.Options qualified as Opts
import Brig.User.Search.Index qualified as Search
import Cassandra.Util (defInitCassandra)
import Control.Lens (to, view, (^.))
import Control.Monad.Catch (MonadThrow, catchAll, finally, throwM)
import Data.Aeson (Value, object, (.=))
import Data.Credentials (Credentials (..))
import Data.Text qualified as Text
import Database.Bloodhound qualified as ES
import Imports
import Network.HTTP.Client qualified as HTTP
import System.Logger.Class (Logger)
import System.Logger.Class qualified as Log
import System.Logger.Extended (runWithLogger)
import Util.Options qualified as Options

migrate :: Logger -> Opts.ElasticSettings -> Opts.CassandraSettings -> Options.Endpoint -> IO ()
migrate l es cas galleyEndpoint = do
  env <- mkEnv l es cas galleyEndpoint
  finally (go env `catchAll` logAndThrowAgain) (cleanup env)
  where
    go :: Env -> IO ()
    go env =
      runMigrationAction env $ do
        failIfIndexAbsent (es ^. Opts.esConnection . to Opts.esIndex)
        createMigrationsIndexIfNotPresent
        runMigration expectedMigrationVersion

    logAndThrowAgain :: forall a. SomeException -> IO a
    logAndThrowAgain e = do
      runWithLogger l $
        Log.err $
          Log.msg (Log.val "Migration failed with exception") . Log.field "exception" (show e)
      throwM e

-- | Increase this number any time you want to force reindexing.
expectedMigrationVersion :: MigrationVersion
expectedMigrationVersion = MigrationVersion 6

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

mkEnv :: Logger -> Opts.ElasticSettings -> Opts.CassandraSettings -> Options.Endpoint -> IO Env
mkEnv l es cas galleyEndpoint = do
  env <- do
    esMgr <- initHttpManagerWithTLSConfig (es ^. Opts.esConnection . to Opts.esInsecureSkipVerifyTls) (es ^. Opts.esConnection . to Opts.esCaCert)
    pure $ ES.mkBHEnv (Opts.toESServer (es ^. Opts.esConnection . to Opts.esServer)) esMgr
  mCreds <- for (es ^. Opts.esConnection . to Opts.esCredentials) Options.initCredentials
  let envWithAuth = maybe env (\(creds :: Credentials) -> env {ES.bhRequestHook = ES.basicAuthHook (ES.EsUsername creds.username) (ES.EsPassword creds.password)}) mCreds
  rpcMgr <- HTTP.newManager HTTP.defaultManagerSettings
  Env envWithAuth
    <$> initCassandra
    <*> initLogger
    <*> pure (view (Opts.esConnection . to Opts.esIndex) es)
    <*> pure mCreds
    <*> pure rpcMgr
    <*> pure galleyEndpoint
  where
    initCassandra = defInitCassandra (Opts.toCassandraOpts cas) l

    initLogger = pure l

createMigrationsIndexIfNotPresent :: (MonadThrow m, ES.MonadBH m, Log.MonadLogger m) => m ()
createMigrationsIndexIfNotPresent =
  do
    unlessM (ES.indexExists indexName) $ do
      Log.info $
        Log.msg (Log.val "Creating migrations index, used for tracking which migrations have run")
      ES.createIndexWith [] 1 indexName
        >>= throwIfNotCreated CreateMigrationIndexFailed
    ES.putMapping indexName indexMappingName indexMapping
      >>= throwIfNotCreated PutMappingFailed
  where
    throwIfNotCreated err response =
      unless (ES.isSuccess response) $
        throwM $
          err (show response)

failIfIndexAbsent :: (MonadThrow m, ES.MonadBH m) => ES.IndexName -> m ()
failIfIndexAbsent targetIndex =
  unlessM
    (ES.indexExists targetIndex)
    (throwM $ TargetIndexAbsent targetIndex)

-- | Runs only the migrations which need to run
runMigration :: MigrationVersion -> MigrationActionT IO ()
runMigration expectedVersion = do
  foundVersion <- latestMigrationVersion
  if expectedVersion > foundVersion
    then do
      Log.info $
        Log.msg (Log.val "Migration necessary.")
          . Log.field "expectedVersion" expectedVersion
          . Log.field "foundVersion" foundVersion
      Search.reindexAllIfSameOrNewer
      persistVersion expectedVersion
    else do
      Log.info $
        Log.msg (Log.val "No migration necessary.")
          . Log.field "expectedVersion" expectedVersion
          . Log.field "foundVersion" foundVersion

persistVersion :: (MonadThrow m, MonadIO m) => MigrationVersion -> MigrationActionT m ()
persistVersion v =
  let docId = ES.DocId . Text.pack . show $ migrationVersion v
   in do
        persistResponse <- ES.indexDocument indexName indexMappingName ES.defaultIndexDocumentSettings v docId
        if ES.isCreated persistResponse
          then do
            Log.info $
              Log.msg (Log.val "Migration success recorded")
                . Log.field "migrationVersion" v
          else throwM $ PersistVersionFailed v $ show persistResponse

-- | Which version is the table space currently running on?
latestMigrationVersion :: (MonadThrow m, MonadIO m) => MigrationActionT m MigrationVersion
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
