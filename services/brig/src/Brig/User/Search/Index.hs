{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module Brig.User.Search.Index
  ( mappingName,
    boolQuery,
    _TextId,

    -- * Monad
    IndexEnv (..),
    IndexIO,
    runIndexIO,
    MonadIndexIO (..),

    -- * Updates
    reindex,
    updateSearchVisibilityInbound,

    -- * Administrative
    createIndex,
    createIndexIfNotPresent,
    resetIndex,
    reindexAll,
    reindexAllIfSameOrNewer,
    refreshIndex,
    updateMapping,

    -- * Re-exports
    module Types,
    ES.IndexSettings (..),
    ES.IndexName (..),
  )
where

import Bilge (expect2xx, header, lbytes, paths)
import Bilge.IO (MonadHttp)
import Bilge.IO qualified as RPC
import Bilge.RPC (RPCException (RPCException))
import Bilge.Request qualified as RPC (empty, host, method, port)
import Bilge.Response (responseJsonThrow)
import Bilge.Retry (rpcHandlers)
import Brig.Index.Types (CreateIndexSettings (..))
import Brig.Types.Search (SearchVisibilityInbound, defaultSearchVisibilityInbound, searchVisibilityInboundFromFeatureStatus)
import Brig.User.Search.Index.Types as Types
import Cassandra.CQL qualified as C
import Cassandra.Exec qualified as C
import Cassandra.Util
import Control.Lens hiding ((#), (.=))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, throwM, try)
import Control.Monad.Except
import Control.Retry (RetryPolicy, exponentialBackoff, limitRetries, recovering)
import Data.Aeson as Aeson
import Data.Aeson.Encoding
import Data.Aeson.Lens
import Data.ByteString (toStrict)
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Conversion (toByteString')
import Data.ByteString.Conversion qualified as Bytes
import Data.ByteString.Lazy qualified as BL
import Data.Credentials
import Data.Handle (Handle)
import Data.Id
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Text.Lazy qualified as LT
import Data.Text.Lens hiding (text)
import Data.UUID qualified as UUID
import Database.Bloodhound qualified as ES
import Imports hiding (log, searchable)
import Network.HTTP.Client hiding (host, path, port)
import Network.HTTP.Types (StdMethod (POST), hContentType, statusCode)
import Prometheus (MonadMonitor)
import Prometheus qualified as Prom
import SAML2.WebSSO.Types qualified as SAML
import System.Logger qualified as Log
import System.Logger.Class (Logger, MonadLogger (..), field, info, msg, val, (+++), (~~))
import URI.ByteString (URI, serializeURIRef)
import Util.Options (Endpoint, host, port)
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti qualified as Multi
import Wire.API.Team.Feature (SearchVisibilityInboundConfig, featureNameBS)
import Wire.API.User
import Wire.API.User qualified as User
import Wire.API.User.Search (Sso (..))

--------------------------------------------------------------------------------
-- IndexIO Monad

data IndexEnv = IndexEnv
  { idxLogger :: Logger,
    idxElastic :: ES.BHEnv,
    idxRequest :: Maybe RequestId,
    idxName :: ES.IndexName,
    idxAdditionalName :: Maybe ES.IndexName,
    idxAdditionalElastic :: Maybe ES.BHEnv,
    idxGalley :: Endpoint,
    -- | Used to make RPC calls to other wire-server services
    idxRpcHttpManager :: Manager,
    -- credentials for reindexing have to be passed via the env because bulk API requests are not supported by bloodhound
    idxCredentials :: Maybe Credentials
  }

newtype IndexIO a = IndexIO (ReaderT IndexEnv IO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader IndexEnv,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadMonitor
    )

runIndexIO :: (MonadIO m) => IndexEnv -> IndexIO a -> m a
runIndexIO e (IndexIO m) = liftIO $ runReaderT m e

class (MonadIO m) => MonadIndexIO m where
  liftIndexIO :: IndexIO a -> m a

instance MonadIndexIO IndexIO where
  liftIndexIO = id

instance MonadLogger IndexIO where
  log l m = do
    g <- asks idxLogger
    r <- asks idxRequest
    Log.log g l $ maybe id (field "request" . unRequestId) r ~~ m

instance MonadLogger (ExceptT e IndexIO) where
  log l m = lift (log l m)

instance ES.MonadBH IndexIO where
  getBHEnv = asks idxElastic

instance MonadHttp IndexIO where
  handleRequestWithCont req handler = do
    manager <- asks idxRpcHttpManager
    liftIO $ withResponse req manager handler

withDefaultESUrl :: (MonadIndexIO m) => ES.BH m a -> m a
withDefaultESUrl action = do
  bhEnv <- liftIndexIO $ asks idxElastic
  ES.runBH bhEnv action

-- | When the additional URL is not provided, uses the default url.
withAdditionalESUrl :: (MonadIndexIO m) => ES.BH m a -> m a
withAdditionalESUrl action = do
  mAdditionalBHEnv <- liftIndexIO $ asks idxAdditionalElastic
  defaultBHEnv <- liftIndexIO $ asks idxElastic
  ES.runBH (fromMaybe defaultBHEnv mAdditionalBHEnv) action

--------------------------------------------------------------------------------
-- Updates

reindex :: (MonadLogger m, MonadIndexIO m, C.MonadClient m, Prom.MonadMonitor IndexIO) => UserId -> m ()
reindex u = do
  ixu <- lookupIndexUser u
  updateIndex (maybe (IndexDeleteUser u) (IndexUpdateUser IndexUpdateIfNewerVersion) ixu)

updateIndex :: (MonadIndexIO m, Prom.MonadMonitor IndexIO) => IndexUpdate -> m ()
updateIndex (IndexUpdateUser updateType iu) = liftIndexIO $ do
  Prom.incCounter indexUpdateCounter
  info $
    field "user" (Bytes.toByteString (view iuUserId iu))
      . msg (val "Indexing user")
  idx <- asks idxName
  withDefaultESUrl $ indexDoc idx
  withAdditionalESUrl $ traverse_ indexDoc =<< asks idxAdditionalName
  where
    indexDoc :: (MonadIndexIO m, MonadThrow m) => ES.IndexName -> ES.BH m ()
    indexDoc idx = do
      r <- ES.indexDocument idx mappingName versioning (indexToDoc iu) docId
      unless (ES.isSuccess r || ES.isVersionConflict r) $ do
        liftIO $ Prom.incCounter indexUpdateErrorCounter
        ES.parseEsResponse r >>= throwM . IndexUpdateError . either id id
      liftIO $ Prom.incCounter indexUpdateSuccessCounter
    versioning =
      ES.defaultIndexDocumentSettings
        { ES.idsVersionControl = indexUpdateToVersionControl updateType (ES.ExternalDocVersion (docVersion (_iuVersion iu)))
        }
    docId = ES.DocId (view (iuUserId . re _TextId) iu)
updateIndex (IndexUpdateUsers updateType ius) = liftIndexIO $ do
  Prom.incCounter indexBulkUpdateCounter
  info $
    field "num_users" (length ius)
      . msg (val "Bulk indexing users")
  -- Sadly, 'bloodhound' is not aware of the versioning capabilities of ES'
  -- bulk API, thus we need to stitch everything together by hand.
  bhe <- ES.getBHEnv
  ES.IndexName idx <- asks idxName
  let (ES.MappingName mpp) = mappingName
  let (ES.Server base) = ES.bhServer bhe
  req <- parseRequest (view unpacked $ base <> "/" <> idx <> "/" <> mpp <> "/_bulk")
  authHeaders <- mkAuthHeaders
  res <-
    liftIO $
      httpLbs
        req
          { method = "POST",
            requestHeaders = [(hContentType, "application/x-ndjson")] <> authHeaders, -- sic
            requestBody = RequestBodyLBS (toLazyByteString (foldMap bulkEncode ius))
          }
        (ES.bhManager bhe)
  unless (ES.isSuccess res) $ do
    Prom.incCounter indexBulkUpdateErrorCounter
    ES.parseEsResponse res >>= throwM . IndexUpdateError . either id id
  Prom.incCounter indexBulkUpdateSuccessCounter
  for_ (statuses res) $ \(s, f) ->
    Prom.withLabel indexBulkUpdateResponseCounter (Text.pack $ show s) $ (void . flip Prom.addCounter (fromIntegral f))
  where
    mkAuthHeaders = do
      creds <- asks idxCredentials
      pure $ maybe [] ((: []) . mkBasicAuthHeader) creds

    encodeJSONToString :: (ToJSON a) => a -> Builder
    encodeJSONToString = fromEncoding . toEncoding
    bulkEncode iu =
      bulkMeta (view (iuUserId . re _TextId) iu) (docVersion (_iuVersion iu))
        <> "\n"
        <> encodeJSONToString (indexToDoc iu)
        <> "\n"
    bulkMeta :: Text -> ES.DocVersion -> Builder
    bulkMeta docId v =
      fromEncoding . pairs . pair "index" . pairs $
        "_id" .= docId
          <> "_version" .= v
          -- "external_gt or external_gte"
          <> "_version_type" .= indexUpdateToVersionControlText updateType
    statuses :: ES.Reply -> [(Int, Int)] -- [(Status, Int)]
    statuses =
      Map.toList
        . Map.fromListWith (+)
        . flip zip [1, 1 ..]
        . toListOf (key "items" . values . key "index" . key "status" . _Integral)
        . responseBody
updateIndex (IndexDeleteUser u) = liftIndexIO $ do
  Prom.incCounter indexDeleteCounter
  info $
    field "user" (Bytes.toByteString u)
      . msg (val "(Soft) deleting user from index")
  idx <- asks idxName
  r <- ES.getDocument idx mappingName (ES.DocId (review _TextId u))
  case statusCode (responseStatus r) of
    200 -> case preview (key "_version" . _Integer) (responseBody r) of
      Nothing -> throwM $ ES.EsProtocolException "'version' not found" (responseBody r)
      Just v -> updateIndex . IndexUpdateUser IndexUpdateIfNewerVersion . mkIndexUser u =<< mkIndexVersion (v + 1)
    404 -> pure ()
    _ -> ES.parseEsResponse r >>= throwM . IndexUpdateError . either id id

updateSearchVisibilityInbound :: (MonadIndexIO m) => Multi.TeamStatus SearchVisibilityInboundConfig -> m ()
updateSearchVisibilityInbound status = liftIndexIO $ do
  withDefaultESUrl . updateAllDocs =<< asks idxName
  withAdditionalESUrl $ traverse_ updateAllDocs =<< asks idxAdditionalName
  where
    updateAllDocs :: (MonadIndexIO m, MonadThrow m) => ES.IndexName -> ES.BH m ()
    updateAllDocs idx = do
      r <- ES.updateByQuery idx query (Just script)
      unless (ES.isSuccess r || ES.isVersionConflict r) $ do
        ES.parseEsResponse r >>= throwM . IndexUpdateError . either id id

    query :: ES.Query
    query = ES.TermQuery (ES.Term "team" $ idToText (Multi.team status)) Nothing

    script :: ES.Script
    script = ES.Script (Just (ES.ScriptLanguage "painless")) (Just (ES.ScriptInline scriptText)) Nothing Nothing

    -- Unfortunately ES disallows updating ctx._version with a "Update By Query"
    scriptText =
      "ctx._source."
        <> searchVisibilityInboundFieldName
        <> " = '"
        <> decodeUtf8 (toByteString' (searchVisibilityInboundFromFeatureStatus (Multi.status status)))
        <> "';"

--------------------------------------------------------------------------------
-- Administrative

refreshIndex :: (MonadIndexIO m) => m ()
refreshIndex = liftIndexIO $ do
  idx <- asks idxName
  void $ ES.refreshIndex idx

createIndexIfNotPresent ::
  (MonadIndexIO m) =>
  CreateIndexSettings ->
  m ()
createIndexIfNotPresent = createIndex' False

createIndex ::
  (MonadIndexIO m) =>
  CreateIndexSettings ->
  m ()
createIndex = createIndex' True

createIndex' ::
  (MonadIndexIO m) =>
  -- | Fail if index alredy exists
  Bool ->
  CreateIndexSettings ->
  m ()
createIndex' failIfExists (CreateIndexSettings settings shardCount mbDeleteTemplate) = liftIndexIO $ do
  idx <- asks idxName
  ex <- ES.indexExists idx
  when (failIfExists && ex) $
    throwM (IndexError "Index already exists.")
  unless ex $ do
    let fullSettings = settings ++ [ES.AnalysisSetting analysisSettings]

    -- A previous release added an ES Index Template that matched all indices
    -- named 'directory*'. This template is deprecated now, but it might still
    -- be present in production instances. If present then it causes the update mapping
    -- step to fail.
    -- FUTUREWORK: remove this block and the --delete-template option,
    -- after this has been released.
    for_ mbDeleteTemplate $ \templateName@(ES.TemplateName tname) -> do
      tExists <- ES.templateExists templateName
      when tExists $ do
        dr <-
          traceES
            ( encodeUtf8
                ("Delete index template " <> "\"" <> tname <> "\"")
            )
            $ ES.deleteTemplate templateName
        unless (ES.isSuccess dr) $
          throwM (IndexError "Deleting index template failed.")

    cr <- traceES "Create index" $ ES.createIndexWith fullSettings shardCount idx
    unless (ES.isSuccess cr) $
      throwM (IndexError "Index creation failed.")
    mr <-
      traceES "Put mapping" $
        ES.putMapping idx (ES.MappingName "user") indexMapping
    unless (ES.isSuccess mr) $
      throwM (IndexError "Put Mapping failed.")

analysisSettings :: ES.Analysis
analysisSettings =
  let analyzerDef =
        Map.fromList
          [ ("prefix_index", ES.AnalyzerDefinition (Just $ ES.Tokenizer "whitespace") [ES.TokenFilter "edge_ngram_1_30"] []),
            ("prefix_search", ES.AnalyzerDefinition (Just $ ES.Tokenizer "whitespace") [ES.TokenFilter "truncate_30"] [])
          ]
      filterDef =
        Map.fromList
          [ ("edge_ngram_1_30", ES.TokenFilterDefinitionEdgeNgram (ES.NgramFilter 1 30) Nothing),
            ("truncate_30", ES.TokenFilterTruncate 30)
          ]
   in ES.Analysis analyzerDef mempty filterDef mempty

updateMapping :: (MonadIndexIO m) => m ()
updateMapping = liftIndexIO $ do
  idx <- asks idxName
  ex <- ES.indexExists idx
  unless ex $
    throwM (IndexError "Index does not exist.")
  -- FUTUREWORK: check return code (ES.isSuccess) and fail if appropriate.
  -- But to do that we have to consider the consequences of this failing in our helm chart:
  -- https://github.com/wireapp/wire-server-deploy/blob/92311d189818ffc5e26ff589f81b95c95de8722c/charts/elasticsearch-index/templates/create-index.yaml
  void $
    traceES "Put mapping" $
      ES.putMapping idx (ES.MappingName "user") indexMapping

resetIndex ::
  (MonadIndexIO m) =>
  CreateIndexSettings ->
  m ()
resetIndex ciSettings = liftIndexIO $ do
  idx <- asks idxName
  gone <-
    ES.indexExists idx >>= \case
      True -> ES.isSuccess <$> traceES "Delete Index" (ES.deleteIndex idx)
      False -> pure True
  if gone
    then createIndex ciSettings
    else throwM (IndexError "Index deletion failed.")

reindexAllIfSameOrNewer :: (MonadLogger m, MonadIndexIO m, C.MonadClient m) => m ()
reindexAllIfSameOrNewer = reindexAllWith IndexUpdateIfSameOrNewerVersion

reindexAll :: (MonadLogger m, MonadIndexIO m, C.MonadClient m) => m ()
reindexAll = reindexAllWith IndexUpdateIfNewerVersion

reindexAllWith :: (MonadLogger m, MonadIndexIO m, C.MonadClient m) => IndexDocUpdateType -> m ()
reindexAllWith updateType = do
  idx <- liftIndexIO $ asks idxName
  C.liftClient (scanForIndex 1000) >>= loop idx
  where
    loop idx page = do
      info $
        field "size" (length (C.result page))
          . msg (val "Reindex: processing C* page")
      unless (null (C.result page)) $ do
        let teamsInPage = mapMaybe teamInReindexRow (C.result page)
        lookupFn <- liftIndexIO $ getSearchVisibilityInboundMulti teamsInPage
        let reindexRow row =
              let sv = maybe defaultSearchVisibilityInbound lookupFn (teamInReindexRow row)
               in reindexRowToIndexUser row sv
        indexUsers <- mapM reindexRow (C.result page)
        updateIndex (IndexUpdateUsers updateType indexUsers)
      when (C.hasMore page) $
        C.liftClient (C.nextPage page) >>= loop idx

--------------------------------------------------------------------------------
-- Internal

-- This is useful and necessary due to the lack of expressiveness in the bulk API
indexUpdateToVersionControlText :: IndexDocUpdateType -> Text
indexUpdateToVersionControlText IndexUpdateIfNewerVersion = "external_gt"
indexUpdateToVersionControlText IndexUpdateIfSameOrNewerVersion = "external_gte"

indexUpdateToVersionControl :: IndexDocUpdateType -> (ES.ExternalDocVersion -> ES.VersionControl)
indexUpdateToVersionControl IndexUpdateIfNewerVersion = ES.ExternalGT
indexUpdateToVersionControl IndexUpdateIfSameOrNewerVersion = ES.ExternalGTE

traceES :: (MonadIndexIO m) => ByteString -> IndexIO ES.Reply -> m ES.Reply
traceES descr act = liftIndexIO $ do
  info (msg descr)
  r <- act
  info . msg $ (r & statusCode . responseStatus) +++ val " - " +++ responseBody r
  pure r

-- | This mapping defines how elasticsearch will treat each field in a document. Here
-- is how it treats each field:
-- name: Not indexed, as it is only meant to be shown to user, for querying we use
--       normalized
-- team: Used to ensure only teammates can find each other
-- accent_id: Not indexed, we cannot search by this.
-- normalized: This is transliterated version of the name to ASCII Latin characters,
--             this is used for searching by name
-- handle: Used for searching by handle
-- normalized.prefix: Used for searching by name prefix
-- handle.prefix: Used for searching by handle prefix
-- saml_idp: URL of SAML issuer, not indexed, used for sorting
-- managed_by: possible values "scim" or "wire", indexed as keyword
-- created_at: date when "activated" state last chagned in epoch-millis, not indexed, used for sorting
--
-- The prefix fields use "prefix_index" analyzer for indexing and "prefix_search"
-- analyzer for searching. The "prefix_search" analyzer uses "edge_ngram" filter, this
-- indexes the handles and normalized names by prefixes. For example: "alice" will be
-- indexed as "a", "al", "ali", "alic" and "alice". While searching for say "ali", we
-- do not want to again use the "prefix" analyzer, otherwise we would get a match for
-- "a", "al" and "ali" each, this skews the scoring in elasticsearch a lot and exact
-- matches get pushed behind prefix matches.
--
-- The "prefix_index" analyzer is defined as a combination of the "whitespace"
-- tokenizer and "edge_ngram_1_30" filter. The edge_ngram_1_30 filter generates tokens
-- of from length 1 to 30 and the whitespace tokenizer ensures words separated by
-- whitespaces are tokenized separately. So, tokens for "Alice Charlie" would be:
-- ["a", "al", "ali", "alic", "alice", "c", "ch", "cha", "char", "charl", "charlie"]
-- This makes searching for somebody by just their last or middle name possible.
-- Additionally one could look for "ali char" and still expect to find "Alice Charlie"
--
-- The "prefix_search" analyzer is defined as a combination of the "whitespace"
-- tokenizer and "truncate_30" filter. The truncate_30 filter ensures that the
-- searched tokens are not bigger than 30 characters by truncating them, this is
-- necessary as our "prefix_index" analyzer only creates edge_ngrams until 30
-- characters.
--
-- About the dynamic field: When this is not set and we add another field to our
-- user document, elasticsearch will try to guess how it is supposed to be indexed.
-- Changes to this require creating a new index and a cumbersome migration. So it is
-- important that we set this field to `false`. This will make new fields will just
-- not be indexed. After we decide what they should look like, we can just run a
-- reindex to make them usable. More info:
-- https://www.elastic.co/guide/en/elasticsearch/reference/7.7/dynamic.html
indexMapping :: Value
indexMapping =
  object
    [ "dynamic" .= False,
      "properties"
        .= object
          [ "normalized" -- normalized user name
              .= MappingProperty
                { mpType = MPText,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields =
                    Map.fromList [("prefix", MappingField MPText (Just "prefix_index") (Just "prefix_search"))]
                },
            "name"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "handle"
              .= MappingProperty
                { mpType = MPText,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields =
                    Map.fromList
                      [ ("prefix", MappingField MPText (Just "prefix_index") (Just "prefix_search")),
                        ("keyword", MappingField MPKeyword Nothing Nothing)
                      ]
                },
            "email"
              .= MappingProperty
                { mpType = MPText,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields =
                    Map.fromList
                      [ ("prefix", MappingField MPText (Just "prefix_index") (Just "prefix_search")),
                        ("keyword", MappingField MPKeyword Nothing Nothing)
                      ]
                },
            "team"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "accent_id"
              .= MappingProperty
                { mpType = MPByte,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "account_status"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "saml_idp"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "managed_by"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "created_at"
              .= MappingProperty
                { mpType = MPDate,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "role"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            (fromString . T.unpack $ searchVisibilityInboundFieldName)
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = True,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "scim_external_id"
              .= MappingProperty
                { mpType = MPKeyword,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                },
            "sso"
              .= object
                [ "type" .= Aeson.String "nested",
                  "properties"
                    .= object
                      [ "issuer"
                          .= MappingProperty
                            { mpType = MPKeyword,
                              mpStore = False,
                              mpIndex = False,
                              mpAnalyzer = Nothing,
                              mpFields = mempty
                            },
                        "nameid"
                          .= MappingProperty
                            { mpType = MPKeyword,
                              mpStore = False,
                              mpIndex = False,
                              mpAnalyzer = Nothing,
                              mpFields = mempty
                            }
                      ]
                ],
            "email_unvalidated"
              .= MappingProperty
                { mpType = MPText,
                  mpStore = False,
                  mpIndex = False,
                  mpAnalyzer = Nothing,
                  mpFields = mempty
                }
          ]
    ]

data MappingProperty = MappingProperty
  { mpType :: MappingPropertyType,
    mpStore :: Bool,
    mpIndex :: Bool,
    mpAnalyzer :: Maybe Text,
    mpFields :: Map Text MappingField
  }

data MappingField = MappingField
  { mfType :: MappingPropertyType,
    mfAnalyzer :: Maybe Text,
    mfSearchAnalyzer :: Maybe Text
  }

data MappingPropertyType = MPText | MPKeyword | MPByte | MPDate
  deriving (Eq)

instance ToJSON MappingProperty where
  toJSON mp =
    object
      ( [ "type" .= mpType mp,
          "store" .= mpStore mp,
          "index" .= mpIndex mp
        ]
          <> ["analyzer" .= mpAnalyzer mp | isJust $ mpAnalyzer mp]
          <> ["fields" .= mpFields mp | not . Map.null $ mpFields mp]
      )

instance ToJSON MappingPropertyType where
  toJSON MPText = Aeson.String "text"
  toJSON MPKeyword = Aeson.String "keyword"
  toJSON MPByte = Aeson.String "byte"
  toJSON MPDate = Aeson.String "date"

instance ToJSON MappingField where
  toJSON mf =
    object $
      ["type" .= mfType mf]
        <> ["analyzer" .= mfAnalyzer mf | isJust (mfAnalyzer mf)]
        <> ["search_analyzer" .= mfSearchAnalyzer mf | isJust (mfSearchAnalyzer mf)]

boolQuery :: ES.BoolQuery
boolQuery = ES.mkBoolQuery [] [] [] []

_TextId :: Prism' Text (Id a)
_TextId = prism' (UUID.toText . toUUID) (fmap Id . UUID.fromText)

mappingName :: ES.MappingName
mappingName = ES.MappingName "user"

lookupIndexUser ::
  (MonadIndexIO m, C.MonadClient m) =>
  UserId ->
  m (Maybe IndexUser)
lookupIndexUser = lookupForIndex

lookupForIndex :: (C.MonadClient m, MonadIndexIO m) => UserId -> m (Maybe IndexUser)
lookupForIndex u = do
  mrow <- C.retry C.x1 (C.query1 cql (C.params C.LocalQuorum (Identity u)))
  for mrow $ \row -> do
    let mteam = teamInReindexRow row
    searchVis <- liftIndexIO $ getSearchVisibilityInbound mteam
    reindexRowToIndexUser row searchVis
  where
    cql :: C.PrepQuery C.R (Identity UserId) ReindexRow
    cql =
      "SELECT \
      \id, \
      \team, \
      \name, \
      \writetime(name), \
      \status, \
      \writetime(status), \
      \handle, \
      \writetime(handle), \
      \email, \
      \writetime(email), \
      \accent_id, \
      \writetime(accent_id), \
      \activated, \
      \writetime(activated), \
      \service, \
      \writetime(service), \
      \managed_by, \
      \writetime(managed_by), \
      \sso_id, \
      \writetime(sso_id), \
      \email_unvalidated, \
      \writetime(email_unvalidated) \
      \FROM user \
      \WHERE id = ?"

getSearchVisibilityInbound ::
  Maybe TeamId ->
  IndexIO SearchVisibilityInbound
getSearchVisibilityInbound Nothing = pure defaultSearchVisibilityInbound
getSearchVisibilityInbound (Just tid) = do
  searchVisibilityInboundFromStatus <$> getTeamSearchVisibilityInbound tid

getSearchVisibilityInboundMulti :: [TeamId] -> IndexIO (TeamId -> SearchVisibilityInbound)
getSearchVisibilityInboundMulti tids = do
  Multi.TeamFeatureNoConfigMultiResponse teamsStatuses <- getTeamSearchVisibilityInboundMulti tids
  let lookupMap = Map.fromList (teamsStatuses <&> \x -> (Multi.team x, x))
  pure $ \tid ->
    searchVisibilityInboundFromStatus (tid `Map.lookup` lookupMap)

searchVisibilityInboundFromStatus :: Maybe (Multi.TeamStatus SearchVisibilityInboundConfig) -> SearchVisibilityInbound
searchVisibilityInboundFromStatus = \case
  Nothing -> defaultSearchVisibilityInbound
  Just tvi -> searchVisibilityInboundFromFeatureStatus . Multi.status $ tvi

scanForIndex :: Int32 -> C.Client (C.Page ReindexRow)
scanForIndex num = do
  C.paginate cql (C.paramsP C.One () (num + 1))
  where
    cql :: C.PrepQuery C.R () ReindexRow
    cql =
      "SELECT \
      \id, \
      \team, \
      \name, \
      \writetime(name), \
      \status, \
      \writetime(status), \
      \handle, \
      \writetime(handle), \
      \email, \
      \writetime(email), \
      \accent_id, \
      \writetime(accent_id), \
      \activated, \
      \writetime(activated), \
      \service, \
      \writetime(service), \
      \managed_by, \
      \writetime(managed_by), \
      \sso_id, \
      \writetime(sso_id), \
      \email_unvalidated, \
      \writetime(email_unvalidated) \
      \FROM user"

type Activated = Bool

type ReindexRow =
  ( UserId,
    Maybe TeamId,
    Name,
    Writetime Name,
    Maybe AccountStatus,
    Maybe (Writetime AccountStatus),
    Maybe Handle,
    Maybe (Writetime Handle),
    Maybe Email,
    Maybe (Writetime Email),
    ColourId,
    Writetime ColourId,
    Activated,
    Writetime Activated,
    Maybe ServiceId,
    Maybe (Writetime ServiceId),
    Maybe ManagedBy,
    Maybe (Writetime ManagedBy),
    Maybe UserSSOId,
    Maybe (Writetime UserSSOId),
    Maybe Email,
    Maybe (Writetime Email)
  )

-- the _2 lens does not work for a tuple this big
teamInReindexRow :: ReindexRow -> Maybe TeamId
teamInReindexRow (_f1, f2, _f3, _f4, _f5, _f6, _f7, _f8, _f9, _f10, _f11, _f12, _f13, _f14, _f15, _f16, _f17, _f18, _f19, _f20, _f21, _f22) = f2

reindexRowToIndexUser :: forall m. (MonadThrow m) => ReindexRow -> SearchVisibilityInbound -> m IndexUser
reindexRowToIndexUser
  ( u,
    mteam,
    name,
    tName,
    status,
    tStatus,
    handle,
    tHandle,
    email,
    tEmail,
    colour,
    tColour,
    activated,
    tActivated,
    service,
    tService,
    managedBy,
    tManagedBy,
    ssoId,
    tSsoId,
    emailUnvalidated,
    tEmailUnvalidated
    )
  searchVisInbound =
    do
      iu <-
        mkIndexUser u
          <$> version
            [ Just (v tName),
              v <$> tStatus,
              v <$> tHandle,
              v <$> tEmail,
              Just (v tColour),
              Just (v tActivated),
              v <$> tService,
              v <$> tManagedBy,
              v <$> tSsoId,
              v <$> tEmailUnvalidated
            ]
      pure $
        if shouldIndex
          then
            iu
              & set iuTeam mteam
                . set iuName (Just name)
                . set iuHandle handle
                . set iuEmail email
                . set iuColourId (Just colour)
                . set iuAccountStatus status
                . set iuSAMLIdP (idpUrl =<< ssoId)
                . set iuManagedBy managedBy
                . set iuCreatedAt (Just (writetimeToUTC tActivated))
                . set iuSearchVisibilityInbound (Just searchVisInbound)
                . set iuScimExternalId (join $ User.scimExternalId <$> managedBy <*> ssoId)
                . set iuSso (sso =<< ssoId)
                . set iuEmailUnvalidated emailUnvalidated
          else
            iu
              -- We insert a tombstone-style user here, as it's easier than deleting the old one.
              -- It's mostly empty, but having the status here might be useful in the future.
              & set iuAccountStatus status
    where
      v :: Writetime a -> Int64
      v = writetimeToInt64

      version :: [Maybe Int64] -> m IndexVersion
      version = mkIndexVersion . getMax . mconcat . fmap Max . catMaybes

      shouldIndex =
        ( case status of
            Nothing -> True
            Just Active -> True
            Just Suspended -> True
            Just Deleted -> False
            Just Ephemeral -> False
            Just PendingInvitation -> False
        )
          && activated -- FUTUREWORK: how is this adding to the first case?
          && isNothing service
      idpUrl :: UserSSOId -> Maybe Text
      idpUrl (UserSSOId (SAML.UserRef (SAML.Issuer uri) _subject)) =
        Just $ fromUri uri
      idpUrl (UserScimExternalId _) = Nothing

      fromUri :: URI -> Text
      fromUri =
        decodeUtf8With lenientDecode
          . toStrict
          . toLazyByteString
          . serializeURIRef

      sso :: UserSSOId -> Maybe Sso
      sso userSsoId = do
        (issuer, nameid) <- User.ssoIssuerAndNameId userSsoId
        pure $ Sso {ssoIssuer = issuer, ssoNameId = nameid}

getTeamSearchVisibilityInbound ::
  TeamId ->
  IndexIO (Maybe (Multi.TeamStatus SearchVisibilityInboundConfig))
getTeamSearchVisibilityInbound tid = do
  Multi.TeamFeatureNoConfigMultiResponse teamsStatuses <- getTeamSearchVisibilityInboundMulti [tid]
  case filter ((== tid) . Multi.team) teamsStatuses of
    [teamStatus] -> pure (Just teamStatus)
    _ -> pure Nothing

getTeamSearchVisibilityInboundMulti ::
  [TeamId] ->
  IndexIO (Multi.TeamFeatureNoConfigMultiResponse SearchVisibilityInboundConfig)
getTeamSearchVisibilityInboundMulti tids = do
  galley <- asks idxGalley
  serviceRequest' "galley" galley POST req >>= responseJsonThrow (ParseException "galley")
  where
    req =
      paths ["i", "features-multi-teams", featureNameBS @SearchVisibilityInboundConfig]
        . header "Content-Type" "application/json"
        . expect2xx
        . lbytes (encode $ Multi.TeamFeatureNoConfigMultiRequest tids)

    serviceRequest' ::
      forall m.
      (MonadIO m, MonadMask m, MonadHttp m) =>
      LT.Text ->
      Endpoint ->
      StdMethod ->
      (Request -> Request) ->
      m (Response (Maybe BL.ByteString))
    serviceRequest' nm endpoint m r = do
      let service = mkEndpoint endpoint
      recovering x3 rpcHandlers $
        const $ do
          let rq = (RPC.method m . r) service
          res <- try $ RPC.httpLbs rq id
          case res of
            Left x -> throwM $ RPCException nm rq x
            Right x -> pure x
      where
        mkEndpoint service = RPC.host (encodeUtf8 (service ^. host)) . RPC.port (service ^. port) $ RPC.empty

        x3 :: RetryPolicy
        x3 = limitRetries 3 <> exponentialBackoff 100000

data ParseException = ParseException
  { _parseExceptionRemote :: !Text,
    _parseExceptionMsg :: String
  }

instance Show ParseException where
  show (ParseException r m) =
    "Failed to parse response from remote "
      ++ Text.unpack r
      ++ " with message: "
      ++ m

instance Exception ParseException

---------------------------------------------------------------------------------
-- Metrics

{-# NOINLINE indexUpdateCounter #-}
indexUpdateCounter :: Prom.Counter
indexUpdateCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "user.index.update.count",
          Prom.metricHelp = "Number of updates on user index"
        }

{-# NOINLINE indexUpdateErrorCounter #-}
indexUpdateErrorCounter :: Prom.Counter
indexUpdateErrorCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "user.index.update.err",
          Prom.metricHelp = "Number of errors during user index update"
        }

{-# NOINLINE indexUpdateSuccessCounter #-}
indexUpdateSuccessCounter :: Prom.Counter
indexUpdateSuccessCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "user.index.update.ok",
          Prom.metricHelp = "Number of successful user index updates"
        }

{-# NOINLINE indexBulkUpdateCounter #-}
indexBulkUpdateCounter :: Prom.Counter
indexBulkUpdateCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "user.index.update.bulk.count",
          Prom.metricHelp = "Number of bulk updates on user index"
        }

{-# NOINLINE indexBulkUpdateErrorCounter #-}
indexBulkUpdateErrorCounter :: Prom.Counter
indexBulkUpdateErrorCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "user.index.update.bulk.err",
          Prom.metricHelp = "Number of errors during bulk updates on user index"
        }

{-# NOINLINE indexBulkUpdateSuccessCounter #-}
indexBulkUpdateSuccessCounter :: Prom.Counter
indexBulkUpdateSuccessCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "user.index.update.bulk.ok",
          Prom.metricHelp = "Number of successful bulk updates on user index"
        }

{-# NOINLINE indexBulkUpdateResponseCounter #-}
indexBulkUpdateResponseCounter :: Prom.Vector Prom.Label1 Prom.Counter
indexBulkUpdateResponseCounter =
  Prom.unsafeRegister $
    Prom.vector ("status") $
      Prom.counter
        Prom.Info
          { Prom.metricName = "user.index.update.bulk.response",
            Prom.metricHelp = "Number of successful bulk updates on user index"
          }

{-# NOINLINE indexDeleteCounter #-}
indexDeleteCounter :: Prom.Counter
indexDeleteCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "user.index.delete.count",
          Prom.metricHelp = "Number of deletes on user index"
        }
