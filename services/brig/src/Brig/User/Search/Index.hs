{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module Brig.User.Search.Index
  ( -- * Monad
    IndexEnv (..),
    IndexIO,
    runIndexIO,
    MonadIndexIO (..),

    -- * Queries
    searchIndex,
    teamSize,

    -- * Updates
    reindex,

    -- * Administrative
    createIndex,
    createIndexIfNotPresent,
    resetIndex,
    reindexAll,
    reindexAllIfSameOrNewer,
    refreshIndex,
    updateMapping,

    -- * exported for testing only
    userDoc,

    -- * Re-exports
    module Types,
    ES.IndexSettings (..),
    ES.IndexName (..),
  )
where

import Brig.Data.Instances ()
import Brig.Types.Intra
import Brig.Types.Search
import Brig.Types.Team (TeamSize (..))
import Brig.Types.User
import Brig.User.Search.Index.Types as Types
import qualified Cassandra as C
import Control.Lens hiding ((#), (.=))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, throwM)
import Control.Monad.Except
import Data.Aeson as Aeson
import Data.Aeson.Encoding
import Data.Aeson.Lens
import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Conversion as Bytes
import Data.Handle (Handle)
import Data.Id
import qualified Data.Map as Map
import Data.Metrics
import Data.Range
import Data.Semigroup (Max (..))
import Data.Text.ICU.Translit (trans, transliterate)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lens hiding (text)
import qualified Data.UUID as UUID
import qualified Database.Bloodhound as ES
import Imports hiding (log, searchable)
import Network.HTTP.Client hiding (path)
import Network.HTTP.Types (hContentType, statusCode)
import qualified System.Logger as Log
import System.Logger.Class
  ( Logger,
    MonadLogger (..),
    field,
    info,
    msg,
    val,
    (+++),
    (~~),
  )

--------------------------------------------------------------------------------
-- IndexIO Monad

data IndexEnv = IndexEnv
  { idxMetrics :: Metrics,
    idxLogger :: Logger,
    idxElastic :: ES.BHEnv,
    idxRequest :: Maybe RequestId,
    idxName :: ES.IndexName,
    idxAdditional :: Maybe ES.IndexName
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
      MonadMask
    )

runIndexIO :: MonadIO m => IndexEnv -> IndexIO a -> m a
runIndexIO e (IndexIO m) = liftIO $ runReaderT m e

class MonadIO m => MonadIndexIO m where
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

--------------------------------------------------------------------------------
-- Queries

teamSize :: MonadIndexIO m => TeamId -> m TeamSize
teamSize t = liftIndexIO $ do
  indexName <- asks idxName
  countResEither <- ES.countByIndex indexName (ES.CountQuery query)
  countRes <- either (throwM . IndexLookupError) pure countResEither
  pure . TeamSize $ ES.crCount countRes
  where
    query =
      ES.TermQuery
        ES.Term
          { ES.termField = "team",
            ES.termValue = idToText t
          }
        Nothing

searchIndex ::
  MonadIndexIO m =>
  -- | The user performing the search.
  UserId ->
  TeamSearchInfo ->
  -- | The search query
  Text ->
  -- | The maximum number of results.
  Range 1 100 Int32 ->
  m (SearchResult Contact)
searchIndex u teamSearchInfo q = queryIndex (defaultUserQuery u teamSearchInfo q)

queryIndex ::
  (MonadIndexIO m, FromJSON r) =>
  IndexQuery r ->
  Range 1 100 Int32 ->
  m (SearchResult r)
queryIndex (IndexQuery q f) (fromRange -> s) = liftIndexIO $ do
  idx <- asks idxName
  let search = (ES.mkSearch (Just q) (Just f)) {ES.size = ES.Size (fromIntegral s)}
  r <-
    ES.searchByType idx mappingName search
      >>= ES.parseEsResponse
  either (throwM . IndexLookupError) (pure . mkResult) r
  where
    mkResult es =
      let results = mapMaybe ES.hitSource . ES.hits . ES.searchHits $ es
       in SearchResult
            { searchFound = ES.hitsTotal . ES.searchHits $ es,
              searchReturned = length results,
              searchTook = ES.took es,
              searchResults = results
            }

-- | The default or canonical 'IndexQuery'.
--
-- The intention behind parameterising 'queryIndex' over the 'IndexQuery' is that
-- it allows to experiment with different queries (perhaps in an A/B context).
-- FUTUREWORK: Drop legacyPrefixMatch
defaultUserQuery :: UserId -> TeamSearchInfo -> Text -> IndexQuery Contact
defaultUserQuery u teamSearchInfo (normalized -> term') =
  let matchPhraseOrPrefix =
        ES.QueryMultiMatchQuery $
          ( ES.mkMultiMatchQuery
              [ ES.FieldName "handle.prefix^2",
                ES.FieldName "normalized.prefix",
                ES.FieldName "handle^4",
                ES.FieldName "normalized^3"
              ]
              (ES.QueryString term')
          )
            { ES.multiMatchQueryType = Just ES.MultiMatchMostFields,
              ES.multiMatchQueryOperator = ES.And
            }
      -- This is required, so we can support prefix match until migration is done
      legacyPrefixMatch =
        ES.QueryMultiMatchQuery $
          ( ES.mkMultiMatchQuery
              [ ES.FieldName "handle",
                ES.FieldName "normalized"
              ]
              (ES.QueryString term')
          )
            { ES.multiMatchQueryType = Just ES.MultiMatchPhrasePrefix,
              ES.multiMatchQueryOperator = ES.And
            }
      query =
        ES.QueryBoolQuery
          boolQuery
            { ES.boolQueryMustMatch =
                [ ES.QueryBoolQuery
                    boolQuery {ES.boolQueryShouldMatch = [matchPhraseOrPrefix, legacyPrefixMatch]}
                ],
              ES.boolQueryShouldMatch = [ES.QueryExistsQuery (ES.FieldName "handle")]
            }
      -- This reduces relevance on non-team users by 90%, there was no science
      -- put behind the negative boost value.
      -- It is applied regardless of a teamId being present as users without a
      -- team anyways don't see any users with team and hence it won't affect
      -- results if a non team user does the search.
      queryWithBoost =
        ES.QueryBoostingQuery
          ES.BoostingQuery
            { ES.positiveQuery = query,
              ES.negativeQuery = matchNonTeamMemberUsers,
              ES.negativeBoost = ES.Boost 0.1
            }
   in mkUserQuery u teamSearchInfo queryWithBoost

mkUserQuery :: UserId -> TeamSearchInfo -> ES.Query -> IndexQuery Contact
mkUserQuery (review _TextId -> self) teamSearchInfo q =
  IndexQuery q $
    ES.Filter . ES.QueryBoolQuery $
      boolQuery
        { ES.boolQueryMustNotMatch = [termQ "_id" self],
          ES.boolQueryMustMatch =
            [ optionallySearchWithinTeam teamSearchInfo,
              ES.QueryBoolQuery
                boolQuery
                  { ES.boolQueryShouldMatch =
                      [ termQ "account_status" "active",
                        -- Also match entries where the account_status field is not present.
                        -- These must have been inserted before we added the account_status
                        -- and at that time we only inserted active users in the first place.
                        -- This should be unnecessary after re-indexing, but let's be lenient
                        -- here for a while.
                        ES.QueryBoolQuery
                          boolQuery
                            { ES.boolQueryMustNotMatch =
                                [ES.QueryExistsQuery (ES.FieldName "account_status")]
                            }
                      ]
                  }
            ]
        }
  where
    termQ f v =
      ES.TermQuery
        ES.Term
          { ES.termField = f,
            ES.termValue = v
          }
        Nothing

-- | This query will make sure that: if teamId is absent, only users without a teamId are
-- returned.  if teamId is present, only users with the *same* teamId or users without a
-- teamId are returned.
optionallySearchWithinTeam :: TeamSearchInfo -> ES.Query
optionallySearchWithinTeam =
  \case
    NoTeam ->
      matchNonTeamMemberUsers
    TeamOnly teamId ->
      matchTeamMembersOf teamId
    TeamAndNonMembers teamId ->
      ES.QueryBoolQuery
        boolQuery
          { ES.boolQueryShouldMatch =
              [ matchTeamMembersOf teamId,
                matchNonTeamMemberUsers
              ]
          }
  where
    matchTeamMembersOf team = ES.TermQuery (ES.Term "team" $ idToText team) Nothing

matchNonTeamMemberUsers :: ES.Query
matchNonTeamMemberUsers =
  ES.QueryBoolQuery
    boolQuery
      { ES.boolQueryMustNotMatch = [ES.QueryExistsQuery $ ES.FieldName "team"]
      }

--------------------------------------------------------------------------------
-- Updates

reindex :: (MonadLogger m, MonadIndexIO m, C.MonadClient m) => UserId -> m ()
reindex u = do
  ixu <- C.liftClient (lookupForIndex u)
  updateIndex (maybe (IndexDeleteUser u) (IndexUpdateUser IndexUpdateIfNewerVersion) ixu)

updateIndex :: MonadIndexIO m => IndexUpdate -> m ()
updateIndex (IndexUpdateUser updateType iu) = liftIndexIO $ do
  m <- asks idxMetrics
  counterIncr (path "user.index.update.count") m
  info $
    field "user" (Bytes.toByteString (view iuUserId iu))
      . msg (val "Indexing user")
  idx <- asks idxName
  indexDoc idx
  traverse_ indexDoc =<< asks idxAdditional
  where
    indexDoc :: MonadIndexIO m => ES.IndexName -> m ()
    indexDoc idx = liftIndexIO $ do
      m <- asks idxMetrics
      r <- ES.indexDocument idx mappingName versioning (userDoc iu) docId
      unless (ES.isSuccess r || ES.isVersionConflict r) $ do
        counterIncr (path "user.index.update.err") m
        ES.parseEsResponse r >>= throwM . IndexUpdateError . either id id
      counterIncr (path "user.index.update.ok") m
    versioning =
      ES.defaultIndexDocumentSettings
        { ES.idsVersionControl = indexUpdateToVersionControl updateType (ES.ExternalDocVersion (docVersion (_iuVersion iu)))
        }
    docId = ES.DocId (view (iuUserId . re _TextId) iu)
updateIndex (IndexUpdateUsers updateType ius) = liftIndexIO $ do
  m <- asks idxMetrics
  counterIncr (path "user.index.update.bulk.count") m
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
  res <-
    liftIO $
      httpLbs
        req
          { method = "POST",
            requestHeaders = [(hContentType, "application/x-ndjson")], -- sic
            requestBody = RequestBodyLBS (toLazyByteString (foldMap bulkEncode ius))
          }
        (ES.bhManager bhe)
  unless (ES.isSuccess res) $ do
    counterIncr (path "user.index.update.bulk.err") m
    ES.parseEsResponse res >>= throwM . IndexUpdateError . either id id
  counterIncr (path "user.index.update.bulk.ok") m
  for_ (statuses res) $ \(s, f) ->
    counterAdd
      (fromIntegral f)
      (path ("user.index.update.bulk.status." <> review builder (decimal s)))
      m
  where
    encodeJSONToString :: ToJSON a => a -> Builder
    encodeJSONToString = fromEncoding . toEncoding
    bulkEncode iu =
      bulkMeta (view (iuUserId . re _TextId) iu) (docVersion (_iuVersion iu))
        <> "\n"
        <> encodeJSONToString (userDoc iu)
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
  counterIncr (path "user.index.delete.count") =<< asks idxMetrics
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

--------------------------------------------------------------------------------
-- Administrative

refreshIndex :: MonadIndexIO m => m ()
refreshIndex = liftIndexIO $ do
  idx <- asks idxName
  void $ ES.refreshIndex idx

createIndexIfNotPresent ::
  MonadIndexIO m =>
  [ES.UpdatableIndexSetting] ->
  -- | Number of shards
  Int ->
  m ()
createIndexIfNotPresent = createIndex' False

createIndex ::
  MonadIndexIO m =>
  [ES.UpdatableIndexSetting] ->
  -- | Number of shards
  Int ->
  m ()
createIndex = createIndex' True

createIndex' ::
  MonadIndexIO m =>
  -- | Fail if index alredy exists
  Bool ->
  [ES.UpdatableIndexSetting] ->
  -- | Number of shards
  Int ->
  m ()
createIndex' failIfExists settings shardCount = liftIndexIO $ do
  idx <- asks idxName
  ex <- ES.indexExists idx
  when (failIfExists && ex) $
    throwM (IndexError "Index already exists.")
  unless ex $ do
    let fullSettings = settings ++ [ES.AnalysisSetting analysisSettings]
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

updateMapping :: MonadIndexIO m => m ()
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
  MonadIndexIO m =>
  [ES.UpdatableIndexSetting] ->
  -- | Number of shards
  Int ->
  m ()
resetIndex settings shardCount = liftIndexIO $ do
  idx <- asks idxName
  gone <-
    ES.indexExists idx >>= \case
      True -> ES.isSuccess <$> traceES "Delete Index" (ES.deleteIndex idx)
      False -> return True
  if gone
    then createIndex settings shardCount
    else throwM (IndexError "Index deletion failed.")

reindexAllIfSameOrNewer :: (MonadLogger m, MonadIndexIO m, C.MonadClient m) => m ()
reindexAllIfSameOrNewer = reindexAllWith IndexUpdateIfSameOrNewerVersion

reindexAll :: (MonadLogger m, MonadIndexIO m, C.MonadClient m) => m ()
reindexAll = reindexAllWith IndexUpdateIfNewerVersion

reindexAllWith :: (MonadLogger m, MonadIndexIO m, C.MonadClient m) => IndexDocUpdateType -> m ()
reindexAllWith updateType = do
  idx <- liftIndexIO $ asks idxName
  C.liftClient (scanForIndex 100) >>= loop idx
  where
    loop idx page = do
      info $
        field "size" (length (C.result page))
          . msg (val "Reindex: processing C* page")
      unless (null (C.result page)) $
        updateIndex (IndexUpdateUsers updateType (C.result page))
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

traceES :: MonadIndexIO m => ByteString -> IndexIO ES.Reply -> m ES.Reply
traceES descr act = liftIndexIO $ do
  info (msg descr)
  r <- act
  info . msg $ (r & statusCode . responseStatus) +++ val " - " +++ responseBody r
  return r

userDoc :: IndexUser -> UserDoc
userDoc iu =
  UserDoc
    { udId = _iuUserId iu,
      udTeam = _iuTeam iu,
      udName = _iuName iu,
      udAccountStatus = _iuAccountStatus iu,
      udNormalized = normalized . fromName <$> _iuName iu,
      udHandle = _iuHandle iu,
      udColourId = _iuColourId iu
    }

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
                    Map.fromList [("prefix", MappingField MPText "prefix_index" "prefix_search")]
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
                    Map.fromList [("prefix", MappingField MPText "prefix_index" "prefix_search")]
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
    mfAnalyzer :: Text,
    mfSearchAnalyzer :: Text
  }

data MappingPropertyType = MPText | MPKeyword | MPByte

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

instance ToJSON MappingField where
  toJSON mf =
    object
      [ "type" .= mfType mf,
        "analyzer" .= mfAnalyzer mf,
        "search_analyzer" .= mfSearchAnalyzer mf
      ]

-- TODO: Transliteration should be left to ElasticSearch (ICU plugin),
--       yet this will require a data migration.
normalized :: Text -> Text
normalized = transliterate (trans "Any-Latin; Latin-ASCII; Lower")

boolQuery :: ES.BoolQuery
boolQuery = ES.mkBoolQuery [] [] [] []

_TextId :: Prism' Text (Id a)
_TextId = prism' (UUID.toText . toUUID) (fmap Id . UUID.fromText)

mappingName :: ES.MappingName
mappingName = ES.MappingName "user"

lookupForIndex :: (MonadThrow m, C.MonadClient m) => UserId -> m (Maybe IndexUser)
lookupForIndex u = do
  result <- C.retry C.x1 (C.query1 cql (C.params C.Quorum (Identity u)))
  sequence $ reindexRowToIndexUser <$> result
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
      \accent_id, \
      \writetime(accent_id), \
      \activated, \
      \writetime(activated), \
      \service, \
      \writetime(service) \
      \FROM user \
      \WHERE id = ?"

-- | FUTUREWORK: make a PR to cql-io with a 'Traversable' instance.
traversePage :: forall a. C.Page (C.Client a) -> C.Client (C.Page a)
traversePage (C.Page hasmore result nextpage) =
  C.Page hasmore <$> sequence result <*> (traversePage <$> nextpage)

scanForIndex :: Int32 -> C.Client (C.Page IndexUser)
scanForIndex num = do
  result :: C.Page ReindexRow <- C.paginate cql (C.paramsP C.One () (num + 1))
  traversePage $ reindexRowToIndexUser <$> result
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
      \accent_id, \
      \writetime(accent_id), \
      \activated, \
      \writetime(activated), \
      \service, \
      \writetime(service) \
      \FROM user"

type Activated = Bool

type Writetime a = Int64

type ReindexRow =
  ( UserId,
    Maybe TeamId,
    Name,
    Writetime Name,
    Maybe AccountStatus,
    Maybe (Writetime AccountStatus),
    Maybe Handle,
    Maybe (Writetime Handle),
    ColourId,
    Writetime ColourId,
    Activated,
    Writetime Activated,
    Maybe ServiceId,
    Maybe (Writetime ServiceId)
  )

reindexRowToIndexUser :: forall m. MonadThrow m => ReindexRow -> m IndexUser
reindexRowToIndexUser (u, mteam, name, t0, status, t1, handle, t2, colour, t4, activated, t5, service, t6) =
  do
    iu <- mkIndexUser u <$> version [Just t0, t1, t2, Just t4, Just t5, t6]
    pure $
      if shouldIndex
        then
          iu
            & set iuTeam mteam
              . set iuName (Just name)
              . set iuHandle handle
              . set iuColourId (Just colour)
              . set iuAccountStatus status
        else
          iu
            -- We insert a tombstone-style user here, as it's easier than deleting the old one.
            -- It's mostly empty, but having the status here might be useful in the future.
            & set iuAccountStatus status
  where
    version :: [Maybe (Writetime Name)] -> m IndexVersion
    version = mkIndexVersion . getMax . mconcat . fmap Max . catMaybes
    shouldIndex =
      and
        [ case status of
            Nothing -> True
            Just Active -> True
            Just Suspended -> True
            Just Deleted -> False
            Just Ephemeral -> False,
          activated, -- FUTUREWORK: how is this adding to the first case?
          isNothing service
        ]
