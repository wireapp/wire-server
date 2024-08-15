module Wire.IndexedUserStore.ElasticSearch where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteString.Builder
import Data.ByteString.Conversion
import Data.Credentials
import Data.Id
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Database.Bloodhound qualified as ES
import Imports
import Network.HTTP.Client
import Network.HTTP.Types
import Polysemy
import Polysemy.Error
import Wire.IndexedUserStore
import Wire.Sem.Metrics (Metrics)
import Wire.Sem.Metrics qualified as Metrics
import Wire.UserSearch.Metrics
import Wire.UserSearch.Types

data ESConn = ESConn
  { env :: ES.BHEnv,
    credentials :: Maybe Credentials,
    indexName :: ES.IndexName
  }

data IndexedUserStoreConfig = IndexedUserStoreConfig
  { conn :: ESConn,
    additionalConn :: Maybe ESConn
  }

data IndexedUserStoreError = IndexUpdateError ES.EsError

interpretIndexedUserStoreES ::
  ( Member (Embed IO) r,
    Member (Error IndexedUserStoreError) r,
    Member Metrics r
  ) =>
  IndexedUserStoreConfig ->
  InterpreterFor IndexedUserStore r
interpretIndexedUserStoreES cfg =
  interpret $ \case
    Upsert docId userDoc versioning -> upsertImpl cfg docId userDoc versioning
    UpdateTeamSearchVisibilityInbound tid vis -> updateTeamSearchVisibilityInboundImpl cfg tid vis
    BulkUpsert docs -> bulkUpsertImpl cfg docs

upsertImpl ::
  forall r.
  ( Member (Embed IO) r,
    Member (Error IndexedUserStoreError) r,
    Member Metrics r
  ) =>
  IndexedUserStoreConfig ->
  ES.DocId ->
  UserDoc ->
  ES.VersionControl ->
  Sem r ()
upsertImpl cfg docId userDoc versioning = do
  runInBothES cfg indexDoc
  where
    indexDoc :: ES.IndexName -> ES.BH (Sem r) ()
    indexDoc idx = do
      r <- ES.indexDocument idx mappingName settings userDoc docId
      unless (ES.isSuccess r || ES.isVersionConflict r) $ do
        lift $ Metrics.incCounter indexUpdateErrorCounter
        res <- liftIO $ ES.parseEsResponse r
        lift . throw . IndexUpdateError . either id id $ res
      lift $ Metrics.incCounter indexUpdateSuccessCounter

    settings = ES.defaultIndexDocumentSettings {ES.idsVersionControl = versioning}

updateTeamSearchVisibilityInboundImpl :: forall r. (Member (Embed IO) r, Member (Error IndexedUserStoreError) r) => IndexedUserStoreConfig -> TeamId -> SearchVisibilityInbound -> Sem r ()
updateTeamSearchVisibilityInboundImpl cfg tid vis =
  runInBothES cfg updateAllDocs
  where
    updateAllDocs :: ES.IndexName -> ES.BH (Sem r) ()
    updateAllDocs idx = do
      r <- ES.updateByQuery idx query (Just script)
      unless (ES.isSuccess r || ES.isVersionConflict r) $ do
        res <- liftIO $ ES.parseEsResponse r
        lift . throw . IndexUpdateError . either id id $ res

    query :: ES.Query
    query = ES.TermQuery (ES.Term "team" $ idToText tid) Nothing

    script :: ES.Script
    script = ES.Script (Just (ES.ScriptLanguage "painless")) (Just (ES.ScriptInline scriptText)) Nothing Nothing

    -- Unfortunately ES disallows updating ctx._version with a "Update By Query"
    scriptText =
      "ctx._source."
        <> Key.toText searchVisibilityInboundFieldName
        <> " = '"
        <> Text.decodeUtf8 (toByteString' vis)
        <> "';"

bulkUpsertImpl ::
  ( Member (Embed IO) r,
    Member (Error IndexedUserStoreError) r
  ) =>
  IndexedUserStoreConfig ->
  [(ES.DocId, UserDoc, ES.VersionControl)] ->
  Sem r ()
bulkUpsertImpl cfg docs = do
  let bhe = cfg.conn.env
      ES.IndexName idx = cfg.conn.indexName
      ES.MappingName mpp = mappingName
      (ES.Server base) = ES.bhServer bhe
      authHeaders = maybe [] ((: []) . mkBasicAuthHeader) cfg.conn.credentials
  req <- embed $ parseRequest (Text.unpack $ base <> "/" <> idx <> "/" <> mpp <> "/_bulk")
  res <-
    embed $
      httpLbs
        req
          { method = "POST",
            requestHeaders = [(hContentType, "application/x-ndjson")] <> authHeaders, -- sic
            requestBody = RequestBodyLBS (toLazyByteString (foldMap encodeActionAndData docs))
          }
        (ES.bhManager bhe)
  unless (ES.isSuccess res) $ do
    parsedRes <- liftIO $ ES.parseEsResponse res
    throw . IndexUpdateError . either id id $ parsedRes
  where
    encodeJSONToString :: (ToJSON a) => a -> Builder
    encodeJSONToString = fromEncoding . toEncoding

    encodeActionAndData :: (ES.DocId, UserDoc, ES.VersionControl) -> Builder
    encodeActionAndData (docId, userDoc, versionControl) =
      encodeJSONToString (bulkIndexAction docId versionControl)
        <> "\n"
        <> encodeJSONToString userDoc
        <> "\n"

    bulkIndexAction :: ES.DocId -> ES.VersionControl -> Value
    bulkIndexAction docId versionControl =
      let (versionType :: Maybe Text, version) = case versionControl of
            ES.NoVersionControl -> (Nothing, Nothing)
            ES.InternalVersion v -> (Nothing, Just v)
            ES.ExternalGT (ES.ExternalDocVersion v) -> (Just "external", Just v)
            ES.ExternalGTE (ES.ExternalDocVersion v) -> (Just "external_gte", Just v)
            ES.ForceVersion (ES.ExternalDocVersion v) -> (Just "force", Just v)
       in object
            [ "index"
                .= object
                  [ "_id" .= docId,
                    "_version_type" .= versionType,
                    "_version" .= version
                  ]
            ]

runInBothES :: (Monad m) => IndexedUserStoreConfig -> (ES.IndexName -> ES.BH m a) -> m a
runInBothES cfg f = do
  x <- ES.runBH cfg.conn.env $ f cfg.conn.indexName
  forM_ cfg.additionalConn $ \additional ->
    ES.runBH additional.env $ f additional.indexName
  pure x

mappingName :: ES.MappingName
mappingName = ES.MappingName "user"
