-- TODO: This needs to be resolved. Warning message:
-- In the use of ‘putMapping’
-- (imported from Database.Bloodhound, but defined in Database.Bloodhound.Common.Client):
-- Deprecated: "See <https://www.elastic.co/guide/en/elasticsearch/reference/7.17/removal-of-types.html>"
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Wire.IndexedUserStore.MigrationStore.ElasticSearch where

import Data.Aeson
import Data.ByteString.Char8 qualified as BS
import Data.Either
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
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
  exists <- either (\l -> logAndThrow CreateMigrationIndexFailed l) pure <$> liftIO (ES.runBH env (ES.indexExists migrationIndexName))
  unlessM exists $ do
    Log.info $
      Log.msg (Log.val "Creating migrations index, used for tracking which migrations have run")
    liftIO (ES.runBH env (ES.createIndexWith [] 1 migrationIndexName))
      >>= throwIfNotCreated CreateMigrationIndexFailed
  liftIO (ES.runBH env (ES.putMapping @Value migrationIndexName migrationIndexMapping))
    >>= throwIfNotCreated PutMappingFailed
  where
    throwIfNotCreated :: (Member TinyLog r, Member (Error MigrationException) r) => (String -> MigrationException) -> Either ES.EsError a -> Sem r ()
    throwIfNotCreated mkErr response =
      -- TODO: Hopefully, it's good enough to look for errors on the left as we
      -- don't know the structure of the right for sure...
      case response of
        Left e -> logAndThrow mkErr e
        Right _ -> pure ()

    logAndThrow :: (Member TinyLog r, Member (Error MigrationException) r) => (String -> MigrationException) -> ES.EsError -> Sem r a
    logAndThrow mkErr response = do
      Log.warn $
        Log.msg (Log.val ("An OpenSearch/ElasticSearch error appeared: " `BS.append` (encodeUtf8 . Text.pack . show) response))
      throw $
        mkErr (show response)

getLatestMigrationVersionImpl :: (Member (Embed IO) r, Member (Error MigrationException) r) => ES.BHEnv -> Sem r MigrationVersion
getLatestMigrationVersionImpl env = do
  reply <- liftIO $ ES.runBH env $ ES.searchByIndex @MigrationVersion migrationIndexName (ES.mkSearch Nothing Nothing)
  result <- either (throw . FetchMigrationVersionsFailed . show) pure reply
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
  let docIdText = Text.pack . show $ migrationVersion v
      docId = ES.DocId docIdText
  persistResponse <- liftIO $ ES.runBH env $ ES.indexDocument migrationIndexName ES.defaultIndexDocumentSettings v docId
  case persistResponse of
    Left _ -> throw $ PersistVersionFailed v $ show persistResponse
    Right r ->
      if ES.idxDocId r == docIdText
        then do
          Log.info $
            Log.msg (Log.val "Migration success recorded")
              . Log.field "migrationVersion" v
        else throw $ PersistVersionFailed v $ show persistResponse

migrationIndexName :: ES.IndexName
migrationIndexName = [ES.qqIndexName|wire_brig_migrations|]

migrationIndexMapping :: Value
migrationIndexMapping =
  object
    [ "properties"
        .= object
          ["migration_version" .= object ["index" .= True, "type" .= ("integer" :: Text)]]
    ]
