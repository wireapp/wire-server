{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Brig.User.Search.Index
  ( -- * Monad
    IndexEnv (..),
    IndexIO,
    runIndexIO,
    MonadIndexIO (..),

    -- * Queries
    searchIndex,
    checkIndex,

    -- * Updates
    reindex,

    -- * Administrative
    createIndex,
    resetIndex,
    reindexAll,
    refreshIndex,

    -- * Re-exports
    module Types,
    ES.IndexSettings (..),
    ES.IndexName (..),
  )
where

import Brig.Data.Instances ()
import Brig.Types.Intra
import Brig.Types.Search
import Brig.Types.User
import Brig.User.Search.Index.Types as Types
import qualified Cassandra as C
import Control.Lens hiding ((#), (.=))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, throwM)
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.Lens
import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Conversion as Bytes
import Data.Id
import qualified Data.Map as Map
import Data.Metrics
import Data.Range
import Data.Semigroup (Max (..))
import Data.Text.ICU.Translit (trans, transliterate)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lens hiding (text)
import qualified Data.UUID as UUID
import qualified Database.V5.Bloodhound as ES
import Imports hiding (log, searchable)
import Network.HTTP.Client hiding (path)
import Network.HTTP.Types (hContentType, statusCode)
import qualified System.Logger as Log
import System.Logger.Class
  ( (+++),
    Logger,
    MonadLogger (..),
    field,
    info,
    msg,
    val,
    (~~),
  )

--------------------------------------------------------------------------------
-- IndexIO Monad

data IndexEnv
  = IndexEnv
      { idxMetrics :: Metrics,
        idxLogger :: Logger,
        idxElastic :: ES.BHEnv,
        idxRequest :: Maybe RequestId,
        idxName :: ES.IndexName
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

searchIndex ::
  MonadIndexIO m =>
  -- | The user performing the search.
  UserId ->
  -- | The search query
  Text ->
  -- | The maximum number of results.
  Range 1 100 Int32 ->
  m (SearchResult Contact)
searchIndex u q = queryIndex (defaultUserQuery u q)

checkIndex :: MonadIndexIO m => UserId -> m SearchableStatus
checkIndex u = liftIndexIO $ fmap SearchableStatus $ do
  idx <- asks idxName
  r <- ES.getDocument idx mappingName (ES.DocId (review _TextId u))
  case statusCode (responseStatus r) of
    404 -> pure False
    _ ->
      ES.parseEsResponse r
        >>= either
          (throwM . IndexLookupError)
          (pure . maybe False searchable . fmap ES._source . ES.foundResult)
  where
    -- NOTE: This only holds when using 'defaultUserQuery'.
    searchable (UserDoc _ _ Nothing Nothing _) = False
    searchable _ = True

-- [Note: suspended]
-- ~~~~~~~~~~~~~~~~~
--
-- The suspended field in the ES index is an artifact of the legacy system,
-- which would not remove data from the index upon user suspension. The filter
-- clause can be removed after the first full re-index, as this will have
-- cleared the data of suspended users.
--

queryIndex ::
  (MonadIndexIO m, FromJSON r) =>
  IndexQuery r ->
  Range 1 100 Int32 ->
  m (SearchResult r)
queryIndex (IndexQuery q f) (fromRange -> s) = liftIndexIO $ do
  idx <- asks idxName
  r <-
    ES.searchByType
      idx
      mappingName
      ((ES.mkSearch (Just q) (Just f)) {ES.size = ES.Size (fromIntegral s)})
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
-- Note, however, that the result of 'isSearchableUser' depends on which fields
-- are queried, and thus can only be correct for the canonical query.
defaultUserQuery :: UserId -> Text -> IndexQuery Contact
defaultUserQuery u (normalized -> term') =
  mkUserQuery u $
    ES.QueryBoolQuery
      boolQuery
        { ES.boolQueryMustMatch =
            [ ES.QueryBoolQuery
                boolQuery
                  { ES.boolQueryShouldMatch =
                      [ ES.QueryMultiMatchQuery $
                          ( ES.mkMultiMatchQuery
                              [ ES.FieldName "handle^2",
                                ES.FieldName "normalized"
                              ]
                              (ES.QueryString term')
                          )
                            { ES.multiMatchQueryType = Just ES.MultiMatchPhrasePrefix
                            },
                        ES.QueryMultiMatchQuery $
                          ( ES.mkMultiMatchQuery
                              [ ES.FieldName "handle^3",
                                ES.FieldName "normalized^2"
                              ]
                              (ES.QueryString term')
                          )
                            { ES.multiMatchQueryType = Just ES.MultiMatchPhrase
                            }
                      ]
                  }
            ],
          ES.boolQueryShouldMatch = [ES.QueryExistsQuery (ES.FieldName "handle")]
        }

mkUserQuery :: UserId -> ES.Query -> IndexQuery Contact
mkUserQuery (review _TextId -> self) q =
  IndexQuery q
    $ ES.Filter . ES.QueryBoolQuery
    $ boolQuery
      { ES.boolQueryMustNotMatch =
          [ termQ "suspended" "1", -- [Note: suspended]
            termQ "_id" self
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

--------------------------------------------------------------------------------
-- Updates

reindex :: (MonadLogger m, MonadIndexIO m, C.MonadClient m) => UserId -> m ()
reindex u = do
  ixu <- C.liftClient (lookupForIndex u)
  updateIndex (maybe (IndexDeleteUser u) IndexUpdateUser ixu)

updateIndex :: MonadIndexIO m => IndexUpdate -> m ()
updateIndex (IndexUpdateUser iu) = liftIndexIO $ do
  m <- asks idxMetrics
  counterIncr (path "user.index.update.count") m
  info $
    field "user" (Bytes.toByteString (view iuUserId iu))
      . msg (val "Indexing user")
  idx <- asks idxName
  r <- ES.indexDocument idx mappingName versioning (userDoc iu) docId
  unless (ES.isSuccess r || ES.isVersionConflict r) $ do
    counterIncr (path "user.index.update.err") m
    ES.parseEsResponse r >>= throwM . IndexUpdateError . either id id
  counterIncr (path "user.index.update.ok") m
  where
    versioning =
      ES.defaultIndexDocumentSettings
        { ES.idsVersionControl = ES.ExternalGT (ES.ExternalDocVersion (docVersion (_iuVersion iu)))
        }
    docId = ES.DocId (view (iuUserId . re _TextId) iu)
updateIndex (IndexUpdateUsers ius) = liftIndexIO $ do
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
    bulkEncode iu =
      bulkMeta (view (iuUserId . re _TextId) iu) (docVersion (_iuVersion iu))
        <> "\n"
        <> fromEncoding (toEncoding (userDoc iu))
        <> "\n"
    bulkMeta :: Text -> ES.DocVersion -> Builder
    bulkMeta docId v =
      fromEncoding . pairs . pair "index" . pairs $
        "_id" .= docId
          <> "_version" .= v
          <> "_version_type" .= ("external" :: Text)
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
      Just v ->
        updateIndex . IndexUpdateUser $
          mkIndexUser u (mkIndexVersion (v + 1))
    404 -> pure ()
    _ -> ES.parseEsResponse r >>= throwM . IndexUpdateError . either id id

--------------------------------------------------------------------------------
-- Administrative

refreshIndex :: MonadIndexIO m => m ()
refreshIndex = liftIndexIO $ do
  idx <- asks idxName
  void $ ES.refreshIndex idx

createIndex :: MonadIndexIO m => ES.IndexSettings -> m ()
createIndex s = liftIndexIO $ do
  idx <- asks idxName
  ex <- ES.indexExists idx
  when ex $
    throwM (IndexError "Index already exists.")
  cr <- traceES "Create index" $ ES.createIndex s idx
  unless (ES.isSuccess cr) $
    throwM (IndexError "Index creation failed.")
  mr <-
    traceES "Put mapping" $
      ES.putMapping idx (ES.MappingName "user") indexMapping
  unless (ES.isSuccess mr) $
    throwM (IndexError "Put Mapping failed.")

resetIndex :: MonadIndexIO m => ES.IndexSettings -> m ()
resetIndex s = liftIndexIO $ do
  idx <- asks idxName
  gone <- ES.indexExists idx >>= \case
    True -> ES.isSuccess <$> traceES "Delete Index" (ES.deleteIndex idx)
    False -> return True
  if gone
    then createIndex s
    else throwM (IndexError "Index deletion failed.")

reindexAll :: (MonadLogger m, MonadIndexIO m, C.MonadClient m) => m ()
reindexAll = do
  idx <- liftIndexIO $ asks idxName
  C.liftClient (scanForIndex 100) >>= loop idx
  where
    loop idx page = do
      info $
        field "size" (length (C.result page))
          . msg (val "Reindex: processing C* page")
      unless (null (C.result page)) $
        updateIndex (IndexUpdateUsers (C.result page))
      when (C.hasMore page) $
        C.liftClient (C.nextPage page) >>= loop idx

--------------------------------------------------------------------------------
-- Internal

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
      udName = fromName <$> _iuName iu,
      udNormalized = normalized . fromName <$> _iuName iu,
      udHandle = fromHandle <$> _iuHandle iu,
      udColourId = _iuColourId iu
    }

indexMapping :: Value
indexMapping =
  object
    [ "properties"
        .= object
          [ "normalized" .= object ["type" .= ("text" :: Text), "store" .= False],
            "name" .= object ["type" .= ("keyword" :: Text), "index" .= False],
            "handle" .= object ["type" .= ("text" :: Text)],
            "accent_id" .= object ["type" .= ("byte" :: Text), "index" .= False]
          ]
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

lookupForIndex :: C.MonadClient m => UserId -> m (Maybe IndexUser)
lookupForIndex u =
  fmap reindexRowToIndexUser
    <$> C.retry C.x1 (C.query1 cql (C.params C.Quorum (Identity u)))
  where
    cql :: C.PrepQuery C.R (Identity UserId) ReindexRow
    cql =
      "SELECT \
      \id, \
      \name, \
      \writetime(name), \
      \status, \
      \writetime(status), \
      \handle, \
      \writetime(handle), \
      \searchable, \
      \writetime(searchable), \
      \accent_id, \
      \writetime(accent_id), \
      \activated, \
      \writetime(activated), \
      \service, \
      \writetime(service) \
      \FROM user \
      \WHERE id = ?"

scanForIndex :: C.MonadClient m => Int32 -> m (C.Page IndexUser)
scanForIndex num =
  fmap reindexRowToIndexUser
    <$> C.paginate cql (C.paramsP C.One () (num + 1))
  where
    cql :: C.PrepQuery C.R () ReindexRow
    cql =
      "SELECT \
      \id, \
      \name, \
      \writetime(name), \
      \status, \
      \writetime(status), \
      \handle, \
      \writetime(handle), \
      \searchable, \
      \writetime(searchable), \
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
    Name,
    Writetime Name,
    Maybe AccountStatus,
    Maybe (Writetime AccountStatus),
    Maybe Handle,
    Maybe (Writetime Handle),
    Maybe SearchableStatus,
    Maybe (Writetime SearchableStatus),
    ColourId,
    Writetime ColourId,
    Activated,
    Writetime Activated,
    Maybe ServiceId,
    Maybe (Writetime ServiceId)
  )

reindexRowToIndexUser :: ReindexRow -> IndexUser
reindexRowToIndexUser (u, name, t0, status, t1, handle, t2, searchable, t3, colour, t4, activated, t5, service, t6) =
  let iu = mkIndexUser u (version [Just t0, t1, t2, t3, Just t4, Just t5, t6])
   in if shouldIndex
        then
          iu & set iuName (Just name)
            . set iuHandle handle
            . set iuColourId (Just colour)
        else iu
  where
    version = mkIndexVersion . getMax . mconcat . fmap Max . catMaybes
    shouldIndex =
      and
        [ fromMaybe True (isSearchable <$> searchable), -- implicit opt-in
          not suspended,
          activated,
          isNothing service
        ]
    suspended = case status of
      Just Active -> False
      Nothing -> False
      _ -> True
