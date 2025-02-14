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

interpretIndexedUserMigrationStoreES :: (Member (Embed IO) r, Member (Error MigrationException) r, Member TinyLog r) => ES.BHEnv -> InterpreterFor IndexedUserMigrationStore r
interpretIndexedUserMigrationStoreES env = interpret $ \case
  EnsureMigrationIndex -> ensureMigrationIndexImpl env
  GetLatestMigrationVersion -> getLatestMigrationVersionImpl env
  PersistMigrationVersion v -> persistMigrationVersionImpl env v

ensureMigrationIndexImpl :: (Member TinyLog r, Member (Embed IO) r, Member (Error MigrationException) r) => ES.BHEnv -> Sem r ()
ensureMigrationIndexImpl env = do
  unlessM (ES.runBH env $ ES.indexExists migrationIndexName) $ do
    Log.info $
      Log.msg (Log.val "Creating migrations index, used for tracking which migrations have run")
    ES.runBH env (ES.createIndexWith [] 1 migrationIndexName)
      >>= throwIfNotCreated CreateMigrationIndexFailed
  -- TODO: We need to stay consistent about the mapping name, otherwise ES 7
  -- complains about having to deal with two: _doc and wire_brig_migrations.
  -- So, we can either try to stick with our name or the default _doc.
  ES.runBH env (ES.putNamedMapping migrationIndexName migrationMappingName migrationIndexMapping)
    >>= throwIfNotCreated PutMappingFailed
  where
    throwIfNotCreated mkErr response =
      unless (ES.isSuccess response) $
        throw $
          mkErr (show response)

getLatestMigrationVersionImpl :: (Member (Embed IO) r, Member (Error MigrationException) r) => ES.BHEnv -> Sem r MigrationVersion
getLatestMigrationVersionImpl env = do
  reply <- ES.runBH env $ ES.searchByIndex migrationIndexName (ES.mkSearch Nothing Nothing)
  resp <- liftIO $ ES.parseEsResponse reply
  result <- either (throw . FetchMigrationVersionsFailed . show) pure resp
  let versions = map ES.hitSource $ ES.hits . ES.searchHits $ result
  case versions of
    [] ->
      pure $ MigrationVersion 0
    vs ->
      if any isNothing vs
        then throw $ VersionSourceMissing result
        else pure $ maximum $ catMaybes vs

persistMigrationVersionImpl :: (Member (Embed IO) r, Member TinyLog r, Member (Error MigrationException) r) => ES.BHEnv -> MigrationVersion -> Sem r ()
persistMigrationVersionImpl env v = do
  let docId = ES.DocId . Text.pack . show $ migrationVersion v
  persistResponse <- ES.runBH env $ ES.indexDocument migrationIndexName migrationMappingName ES.defaultIndexDocumentSettings v docId
  if ES.isCreated persistResponse
    then do
      Log.info $
        Log.msg (Log.val "Migration success recorded")
          . Log.field "migrationVersion" v
    else throw $ PersistVersionFailed v $ show persistResponse

migrationIndexName :: ES.IndexName
migrationIndexName = ES.IndexName "wire_brig_migrations"

migrationMappingName :: ES.MappingName
migrationMappingName = ES.MappingName "wire_brig_migrations"

migrationIndexMapping :: Value
migrationIndexMapping =
  object
    [ "properties"
        .= object
          ["migration_version" .= object ["index" .= True, "type" .= ("integer" :: Text)]]
    ]
