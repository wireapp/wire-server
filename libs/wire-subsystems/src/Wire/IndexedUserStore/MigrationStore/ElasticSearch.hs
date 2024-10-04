-- 'putMapping' is incorrectly deprecated in bloodhound
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Wire.IndexedUserStore.MigrationStore.ElasticSearch where

import Data.Aeson
import Data.ByteString.Char8 qualified as BS
import Data.Either
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Database.Bloodhound qualified as ES
import Database.Bloodhound.Common.Requests qualified as ESR
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
  exists <-
    either (\l -> logAndThrow CreateMigrationIndexFailed l) pure
      <$> liftIO (ES.runBH env (ES.indexExists migrationIndexName))
  unlessM exists $ do
    Log.info $
      Log.msg (Log.val "Creating migrations index, used for tracking which migrations have run")
    liftIO (ES.runBH env . ES.performBHRequest . ES.keepBHResponse $ (ESR.createIndexWith [] 1 migrationIndexName))
      >>= throwIfNotCreated CreateMigrationIndexFailed
  liftIO (ES.runBH env . ES.performBHRequest . ES.keepBHResponse $ (ESR.putMapping @Value migrationIndexName migrationIndexMapping))
    >>= throwIfNotCreated PutMappingFailed
  where
    throwIfNotCreated :: (Member TinyLog r, Member (Error MigrationException) r) => (String -> MigrationException) -> Either ES.EsError (ES.BHResponse a b, c) -> Sem r ()
    throwIfNotCreated mkErr (Left e) = logAndThrow mkErr e
    throwIfNotCreated mkErr (Right (resp, _)) =
      if ES.isSuccess resp
        then pure ()
        else logAndThrow mkErr resp

    logAndThrow :: (Member TinyLog r, Member (Error MigrationException) r, Show e) => (String -> MigrationException) -> e -> Sem r a
    logAndThrow mkErr errMsg = do
      Log.warn $
        Log.msg (Log.val ("An OpenSearch/ElasticSearch error appeared: " `BS.append` (encodeUtf8 . Text.pack . show) errMsg))
      throw $
        mkErr (show errMsg)

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
