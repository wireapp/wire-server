module Wire.IndexedUserStore.ElasticSearch where

import Database.Bloodhound qualified as ES
import Imports
import Polysemy
import Polysemy.Error
import Wire.IndexedUserStore
import Wire.UserSearch.Types (UserDoc)

data ESConn = ESConn
  { env :: ES.BHEnv,
    indexName :: ES.IndexName
  }

data IndexedUserStoreConfig = IndexedUserStoreConfig
  { conn :: ESConn,
    additionalConn :: Maybe ESConn
  }

data IndexedUserStoreError = IndexUpdateError ES.EsError

interpretIndexedUserStoreES :: (Member (Embed IO) r, Member (Error IndexedUserStoreError) r) => IndexedUserStoreConfig -> InterpreterFor IndexedUserStore r
interpretIndexedUserStoreES cfg =
  interpret $ \case
    Upsert docId userDoc versioning -> upsertImpl cfg docId userDoc versioning

upsertImpl :: forall r. (Member (Embed IO) r, Member (Error IndexedUserStoreError) r) => IndexedUserStoreConfig -> ES.DocId -> UserDoc -> ES.VersionControl -> Sem r ()
upsertImpl cfg docId userDoc versioning = do
  runInBothES cfg indexDoc
  where
    indexDoc :: ES.IndexName -> ES.BH (Sem r) ()
    indexDoc idx = do
      r <- ES.indexDocument idx mappingName settings userDoc docId
      unless (ES.isSuccess r || ES.isVersionConflict r) $ do
        -- liftIO $ Prom.incCounter indexUpdateErrorCounter
        res <- liftIO $ ES.parseEsResponse r
        lift . throw . IndexUpdateError . either id id $ res
    -- liftIO $ Prom.incCounter indexUpdateSuccessCounter

    settings = ES.defaultIndexDocumentSettings {ES.idsVersionControl = versioning}

runInBothES :: (Monad m) => IndexedUserStoreConfig -> (ES.IndexName -> ES.BH m a) -> m a
runInBothES cfg f = do
  x <- ES.runBH cfg.conn.env $ f cfg.conn.indexName
  forM_ cfg.additionalConn $ \additional ->
    ES.runBH additional.env $ f additional.indexName
  pure x

-- withDefaultESUrl :: (Member (Embed m) r) => Sem (Embed (ES.BH m) : r) a -> Sem r a
-- withDefaultESUrl action = do
--   bhEnv <- liftIndexIO $ asks idxElastic
--   ES.runBH bhEnv action

-- -- | When the additional URL is not provided, uses the default url.
-- withAdditionalESUrl :: ES.BH m a -> m a
-- withAdditionalESUrl action = do
--   mAdditionalBHEnv <- liftIndexIO $ asks idxAdditionalElastic
--   defaultBHEnv <- liftIndexIO $ asks idxElastic
--   ES.runBH (fromMaybe defaultBHEnv mAdditionalBHEnv) action

mappingName :: ES.MappingName
mappingName = ES.MappingName "user"

-- -- This is useful and necessary due to the lack of expressiveness in the bulk API
-- indexUpdateToVersionControlText :: IndexDocUpdateType -> Text
-- indexUpdateToVersionControlText IndexUpdateIfNewerVersion = "external_gt"
-- indexUpdateToVersionControlText IndexUpdateIfSameOrNewerVersion = "external_gte"

-- indexUpdateToVersionControl :: IndexDocUpdateType -> (ES.ExternalDocVersion -> ES.VersionControl)
-- indexUpdateToVersionControl IndexUpdateIfNewerVersion = ES.ExternalGT
-- indexUpdateToVersionControl IndexUpdateIfSameOrNewerVersion = ES.ExternalGTE
