-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.IndexedUserStore.MigrationStore.ElasticSearch where

import Data.Aeson
import Data.Text qualified as Text
import Database.Bloodhound qualified as ES
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog
import System.Logger.Message qualified as Log
import Wire.IndexedUserStore.MigrationStore
import Wire.Sem.Logger qualified as Log
import Wire.UserSearch.Migration

runES :: (Member (Embed IO) r) => ES.BHEnv -> ES.BH IO a -> Sem r (Either ES.EsError a)
runES env act = join <$> embed (ES.runBH env (ES.tryEsError act))

interpretIndexedUserMigrationStoreES :: (Member (Embed IO) r, Member (Error MigrationException) r, Member TinyLog r) => ES.BHEnv -> ES.IndexName -> InterpreterFor IndexedUserMigrationStore r
interpretIndexedUserMigrationStoreES env migrationIndexName = interpret $ \case
  EnsureMigrationIndex -> ensureMigrationIndexImpl env migrationIndexName
  GetLatestMigrationVersion -> getLatestMigrationVersionImpl env migrationIndexName
  PersistMigrationVersion v -> persistMigrationVersionImpl env v migrationIndexName

ensureMigrationIndexImpl :: (Member TinyLog r, Member (Embed IO) r, Member (Error MigrationException) r) => ES.BHEnv -> ES.IndexName -> Sem r ()
ensureMigrationIndexImpl env migrationIndexName = do
  exists <- runES env (ES.indexExists migrationIndexName)
  case exists of
    Left e -> throw (CreateMigrationIndexFailed (show e))
    Right False -> do
      Log.info $
        Log.msg (Log.val "Creating migrations index, used for tracking which migrations have run")
      created <- runES env (ES.createIndexWith [] 1 migrationIndexName)
      case created of
        Left e -> throw (CreateMigrationIndexFailed (show e))
        Right _ -> pure ()
    Right True -> pure ()
  mapped <- runES env (ES.putMapping @ES.Acknowledged migrationIndexName migrationIndexMapping)
  case mapped of
    Left e -> throw (PutMappingFailed (show e))
    Right _ -> pure ()

getLatestMigrationVersionImpl :: (Member (Embed IO) r, Member (Error MigrationException) r) => ES.BHEnv -> ES.IndexName -> Sem r MigrationVersion
getLatestMigrationVersionImpl env migrationIndexName = do
  result <-
    runES env (ES.searchByIndex migrationIndexName (ES.mkSearch Nothing Nothing))
      >>= either (throw . FetchMigrationVersionsFailed . show) pure
  let versions = map ES.hitSource $ ES.hits . ES.searchHits $ result
  case versions of
    [] ->
      pure $ MigrationVersion 0
    vs ->
      if any isNothing vs
        then throw $ VersionSourceMissing result
        else pure $ maximum $ catMaybes vs

persistMigrationVersionImpl :: (Member (Embed IO) r, Member TinyLog r, Member (Error MigrationException) r) => ES.BHEnv -> MigrationVersion -> ES.IndexName -> Sem r ()
persistMigrationVersionImpl env v migrationIndexName = do
  let docId = ES.DocId . Text.pack . show $ migrationVersion v
  persisted <- runES env (ES.indexDocument migrationIndexName ES.defaultIndexDocumentSettings v docId)
  case persisted of
    Right _ ->
      Log.info $
        Log.msg (Log.val "Migration success recorded")
          . Log.field "migrationVersion" v
    Left e -> throw (PersistVersionFailed v (show e))

defaultMigrationIndexName :: ES.IndexName
defaultMigrationIndexName = either (error . Text.unpack) id (ES.mkIndexName "wire_brig_migrations")

migrationIndexMapping :: Value
migrationIndexMapping =
  object
    [ "properties"
        .= object
          ["migration_version" .= object ["index" .= True, "type" .= ("integer" :: Text)]]
    ]
