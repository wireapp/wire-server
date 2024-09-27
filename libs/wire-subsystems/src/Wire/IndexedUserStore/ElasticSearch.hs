{-# LANGUAGE RecordWildCards #-}

module Wire.IndexedUserStore.ElasticSearch where

import Control.Error (ExceptT (..), lastMay, runExceptT)
import Control.Exception (throwIO)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as LBS
import Data.ByteString.Builder
import Data.ByteString.Conversion
import Data.Id
import Data.Text qualified as Text
import Data.Text.Ascii
import Data.Text.Encoding qualified as Text
import Database.Bloodhound (BHResponse (BHResponse))
import Database.Bloodhound qualified as ES
import Database.Bloodhound.Common.Requests qualified as ESR
import Imports
import Network.HTTP.Client
import Network.HTTP.Types
import Polysemy
import Wire.API.Team.Size (TeamSize (TeamSize))
import Wire.API.User.Search
import Wire.IndexedUserStore
import Wire.Sem.Metrics (Metrics)
import Wire.Sem.Metrics qualified as Metrics
import Wire.UserSearch.Metrics
import Wire.UserSearch.Types
import Wire.UserStore.IndexUser

data ESConn = ESConn
  { env :: ES.BHEnv,
    indexName :: ES.IndexName
  }

data IndexedUserStoreConfig = IndexedUserStoreConfig
  { conn :: ESConn,
    additionalConn :: Maybe ESConn
  }

interpretIndexedUserStoreES ::
  ( Member (Embed IO) r,
    Member Metrics r
  ) =>
  IndexedUserStoreConfig ->
  InterpreterFor IndexedUserStore r
interpretIndexedUserStoreES cfg =
  interpret $ \case
    Upsert docId userDoc versioning -> upsertImpl cfg docId userDoc versioning
    UpdateTeamSearchVisibilityInbound tid vis ->
      updateTeamSearchVisibilityInboundImpl cfg tid vis
    BulkUpsert docs -> bulkUpsertImpl cfg docs
    DoesIndexExist -> doesIndexExistImpl cfg
    SearchUsers searcherId mSearcherTeam teamSearchInfo term maxResults ->
      searchUsersImpl cfg searcherId mSearcherTeam teamSearchInfo term maxResults
    PaginateTeamMembers filters maxResults mPagingState ->
      paginateTeamMembersImpl cfg filters maxResults mPagingState
    GetTeamSize tid -> getTeamSizeImpl cfg tid

getTeamSizeImpl ::
  ( Member (Embed IO) r
  ) =>
  IndexedUserStoreConfig ->
  TeamId ->
  Sem r TeamSize
getTeamSizeImpl cfg tid = do
  let indexName = cfg.conn.indexName
  countResEither <- embed $ ES.runBH cfg.conn.env $ ES.countByIndex indexName (ES.CountQuery query)
  countRes <- either (liftIO . throwIO . IndexLookupError) pure countResEither
  pure . TeamSize $ ES.crCount countRes
  where
    query =
      ES.TermQuery
        ES.Term
          { ES.termField = "team",
            ES.termValue = idToText tid
          }
        Nothing

upsertImpl ::
  forall r.
  ( Member (Embed IO) r,
    Member Metrics r
  ) =>
  IndexedUserStoreConfig ->
  ES.DocId ->
  UserDoc ->
  ES.VersionControl ->
  Sem r ()
upsertImpl cfg docId userDoc versioning = do
  void $ runInBothES cfg indexDoc
  where
    indexDoc :: ES.IndexName -> ES.BH (Sem r) ()
    indexDoc idx = do
      r <- hoistBH (embed @IO) $ ES.performBHRequest . fmap fst . ES.keepBHResponse $ ESR.indexDocument idx settings userDoc docId
      unless (ES.isSuccess r || ES.isVersionConflict r) $ do
        lift $ Metrics.incCounter indexUpdateErrorCounter
        liftIO . throwIO . IndexUpdateError $ parseESError r
      lift $ Metrics.incCounter indexUpdateSuccessCounter

    settings = ES.defaultIndexDocumentSettings {ES.idsVersionControl = versioning}

hoistBH :: (forall x. m x -> n x) -> ES.BH m a -> ES.BH n a
hoistBH nat (ES.BH action) = ES.BH $ hoistReaderT (hoistExceptT nat) action

hoistReaderT :: (forall x. m x -> n x) -> ReaderT r m a -> ReaderT r n a
hoistReaderT nat (ReaderT f) = ReaderT $ \r -> nat (f r)

-- Hoist a natural transformation from m to n through ExceptT
hoistExceptT :: (forall x. m x -> n x) -> ExceptT e m a -> ExceptT e n a
hoistExceptT nat (ExceptT ema) = ExceptT (nat ema)

-- TODO: Extract into helper Module / Or the bottom of the file
castResponse :: forall context1 val1 context2 val2. BHResponse context1 val1 -> BHResponse context2 val2
castResponse BHResponse {..} = BHResponse {..}

updateTeamSearchVisibilityInboundImpl :: forall r. (Member (Embed IO) r) => IndexedUserStoreConfig -> TeamId -> SearchVisibilityInbound -> Sem r ()
updateTeamSearchVisibilityInboundImpl cfg tid vis =
  void $ runInBothES cfg updateAllDocs
  where
    updateAllDocs :: ES.IndexName -> ES.BH (Sem r) ()
    updateAllDocs idx = do
      r <- hoistBH (embed @IO) $ ES.performBHRequest . fmap fst . ES.keepBHResponse $ ESR.updateByQuery @Value idx query (Just script)
      unless (ES.isSuccess r || ES.isVersionConflict r) $ do
        liftIO . throwIO . IndexUpdateError $ parseESError r

    query :: ES.Query
    query = ES.TermQuery (ES.Term "team" $ idToText tid) Nothing

    script :: ES.Script
    script = ES.Script (Just (ES.ScriptLanguage "painless")) (ES.ScriptInline scriptText) Nothing

    -- Unfortunately ES disallows updating ctx._version with a "Update By Query"
    scriptText =
      "ctx._source."
        <> Key.toText searchVisibilityInboundFieldName
        <> " = '"
        <> Text.decodeUtf8 (toByteString' vis)
        <> "';"

bulkUpsertImpl :: (Member (Embed IO) r) => IndexedUserStoreConfig -> [(ES.DocId, UserDoc, ES.VersionControl)] -> Sem r ()
bulkUpsertImpl cfg docs = do
  let bhe = cfg.conn.env
      idx = ES.unIndexName cfg.conn.indexName
      (ES.Server base) = ES.bhServer bhe
  baseReq <- embed $ parseRequest (Text.unpack $ base <> "/" <> idx <> "/_bulk")
  let reqWithoutCreds =
        baseReq
          { method = "POST",
            requestHeaders = [(hContentType, "application/x-ndjson")],
            requestBody = RequestBodyLBS (toLazyByteString (foldMap encodeActionAndData docs))
          }
  req <- embed $ bhe.bhRequestHook reqWithoutCreds
  res <- fmap (BHResponse @ES.StatusDependant @ES.BulkResponse) . embed $ httpLbs req (ES.bhManager bhe)
  unless (ES.isSuccess res) $ do
    liftIO . throwIO . IndexUpdateError $ parseESError res
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

parseESError :: BHResponse context a -> Either ES.EsProtocolException ES.EsError
parseESError res = either id id <$> ES.parseEsResponse (castResponse @_ @_ @_ @ES.EsError res)

doesIndexExistImpl :: (Member (Embed IO) r) => IndexedUserStoreConfig -> Sem r Bool
doesIndexExistImpl cfg = embed $ do
  (mainExists, fromMaybe True -> additionalExists) <- runInBothES cfg ES.indexExists
  pure $ mainExists && additionalExists

searchUsersImpl ::
  (Member (Embed IO) r) =>
  IndexedUserStoreConfig ->
  UserId ->
  Maybe TeamId ->
  TeamSearchInfo ->
  Text ->
  Int ->
  Sem r (SearchResult UserDoc)
searchUsersImpl cfg searcherId mSearcherTeam teamSearchInfo term maxResults =
  queryIndex cfg maxResults $
    defaultUserQuery searcherId mSearcherTeam teamSearchInfo term

-- | The default or canonical 'IndexQuery'.
--
-- The intention behind parameterising 'queryIndex' over the 'IndexQuery' is that
-- it allows to experiment with different queries (perhaps in an A/B context).
--
-- FUTUREWORK: Drop legacyPrefixMatch
defaultUserQuery :: UserId -> Maybe TeamId -> TeamSearchInfo -> Text -> IndexQuery Contact
defaultUserQuery searcher mSearcherTeamId teamSearchInfo (normalized -> term') =
  let matchPhraseOrPrefix =
        ES.QueryMultiMatchQuery $
          ( ES.mkMultiMatchQuery
              [ ES.FieldName "handle.prefix^2",
                ES.FieldName "normalized.prefix",
                ES.FieldName "normalized^3"
              ]
              (ES.QueryString term')
          )
            { ES.multiMatchQueryType = Just ES.MultiMatchMostFields,
              ES.multiMatchQueryOperator = ES.And
            }
      query =
        ES.QueryBoolQuery
          boolQuery
            { ES.boolQueryMustMatch =
                [ ES.QueryBoolQuery
                    boolQuery
                      { ES.boolQueryShouldMatch = [matchPhraseOrPrefix],
                        -- This removes exact handle matches, as they are fetched from cassandra
                        ES.boolQueryMustNotMatch = [termQ "handle" term']
                      }
                ],
              ES.boolQueryShouldMatch = [ES.QueryExistsQuery (ES.FieldName "handle")]
            }
      -- This reduces relevance on users not in team of search by 90% (no
      -- science behind that number). If the searcher is not part of a team the
      -- relevance is not reduced for any users.
      queryWithBoost =
        ES.QueryBoostingQuery
          ES.BoostingQuery
            { ES.positiveQuery = query,
              ES.negativeQuery = maybe ES.QueryMatchNoneQuery matchUsersNotInTeam mSearcherTeamId,
              ES.negativeBoost = ES.Boost 0.1
            }
   in mkUserQuery searcher mSearcherTeamId teamSearchInfo queryWithBoost

paginateTeamMembersImpl ::
  (Member (Embed IO) r) =>
  IndexedUserStoreConfig ->
  BrowseTeamFilters ->
  Int ->
  Maybe PagingState ->
  Sem r (SearchResult UserDoc)
paginateTeamMembersImpl cfg BrowseTeamFilters {..} maxResults mPagingState = do
  let (IndexQuery q f sortSpecs) =
        teamUserSearchQuery teamId mQuery mRoleFilter mSortBy mSortOrder
  let search =
        (ES.mkSearch (Just q) (Just f))
          { -- we are requesting one more result than the page size to determine if there is a next page
            ES.size = ES.Size (fromIntegral maxResults + 1),
            ES.sortBody = Just (fmap ES.DefaultSortSpec sortSpecs),
            ES.searchAfterKey = toSearchAfterKey =<< mPagingState
          }
  mkResult <$> searchInMainIndex cfg search
  where
    toSearchAfterKey ps = decode' . LBS.fromStrict =<< (decodeBase64Url . unPagingState) ps

    fromSearchAfterKey :: ES.SearchAfterKey -> PagingState
    fromSearchAfterKey = PagingState . encodeBase64Url . LBS.toStrict . encode

    mkResult es =
      let hitsPlusOne = ES.hits . ES.searchHits $ es
          hits = take (fromIntegral maxResults) hitsPlusOne
          mps = fromSearchAfterKey <$> lastMay (mapMaybe ES.hitSort hits)
          results = mapMaybe ES.hitSource hits
       in SearchResult
            { searchFound = es.searchHits.hitsTotal.value,
              searchReturned = length results,
              searchTook = ES.took es,
              searchResults = results,
              searchPolicy = FullSearch,
              searchPagingState = mps,
              searchHasMore = Just $ length hitsPlusOne > length hits
            }

searchInMainIndex :: forall r. (Member (Embed IO) r) => IndexedUserStoreConfig -> ES.Search -> Sem r (ES.SearchResult UserDoc)
searchInMainIndex cfg search = embed $ do
  r <- ES.runBH cfg.conn.env $ ES.searchByIndex @UserDoc cfg.conn.indexName search
  either (throwIO . IndexLookupError . Right) pure r

queryIndex ::
  (Member (Embed IO) r) =>
  IndexedUserStoreConfig ->
  Int ->
  IndexQuery x ->
  Sem r (SearchResult UserDoc)
queryIndex cfg s (IndexQuery q f _) = do
  let search = (ES.mkSearch (Just q) (Just f)) {ES.size = ES.Size (fromIntegral s)}
  mkResult <$> searchInMainIndex cfg search
  where
    mkResult es =
      let results = mapMaybe ES.hitSource . ES.hits . ES.searchHits $ es
       in SearchResult
            { searchFound = es.searchHits.hitsTotal.value,
              searchReturned = length results,
              searchTook = ES.took es,
              searchResults = results,
              searchPolicy = FullSearch,
              searchPagingState = Nothing,
              searchHasMore = Nothing
            }

teamUserSearchQuery ::
  TeamId ->
  Maybe Text ->
  Maybe RoleFilter ->
  Maybe TeamUserSearchSortBy ->
  Maybe TeamUserSearchSortOrder ->
  IndexQuery TeamContact
teamUserSearchQuery tid mbSearchText _mRoleFilter mSortBy mSortOrder =
  IndexQuery
    ( maybe
        (ES.MatchAllQuery Nothing)
        matchPhraseOrPrefix
        mbQStr
    )
    teamFilter
    -- in combination with pagination a non-unique search specification can lead to missing results
    -- therefore we use the unique `_doc` value as a tie breaker
    -- - see https://www.elastic.co/guide/en/elasticsearch/reference/6.8/search-request-sort.html for details on `_doc`
    -- - see https://www.elastic.co/guide/en/elasticsearch/reference/6.8/search-request-search-after.html for details on pagination and tie breaker
    -- in the latter article it "is advised to duplicate (client side or [...]) the content of the _id field
    -- in another field that has doc value enabled and to use this new field as the tiebreaker for the sort"
    -- so alternatively we could use the user ID as a tie breaker, but this would require a change in the index mapping
    (sorting ++ sortingTieBreaker)
  where
    sorting :: [ES.DefaultSort]
    sorting =
      maybe
        [defaultSort SortByCreatedAt SortOrderDesc | isNothing mbQStr]
        (\tuSortBy -> [defaultSort tuSortBy (fromMaybe SortOrderAsc mSortOrder)])
        mSortBy
    sortingTieBreaker :: [ES.DefaultSort]
    sortingTieBreaker = [ES.DefaultSort (ES.FieldName "_doc") ES.Ascending Nothing Nothing Nothing Nothing]

    mbQStr :: Maybe Text
    mbQStr =
      case mbSearchText of
        Nothing -> Nothing
        Just q ->
          case normalized q of
            "" -> Nothing
            term' -> Just term'

    matchPhraseOrPrefix term' =
      ES.QueryMultiMatchQuery $
        ( ES.mkMultiMatchQuery
            [ ES.FieldName "email^4",
              ES.FieldName "handle^4",
              ES.FieldName "normalized^3",
              ES.FieldName "email.prefix^3",
              ES.FieldName "handle.prefix^2",
              ES.FieldName "normalized.prefix"
            ]
            (ES.QueryString term')
        )
          { ES.multiMatchQueryType = Just ES.MultiMatchMostFields,
            ES.multiMatchQueryOperator = ES.And
          }

    teamFilter =
      ES.Filter $
        ES.QueryBoolQuery
          boolQuery
            { ES.boolQueryMustMatch = [ES.TermQuery (ES.Term "team" $ idToText tid) Nothing]
            }

    defaultSort :: TeamUserSearchSortBy -> TeamUserSearchSortOrder -> ES.DefaultSort
    defaultSort tuSortBy sortOrder =
      ES.DefaultSort
        ( case tuSortBy of
            SortByName -> ES.FieldName "name"
            SortByHandle -> ES.FieldName "handle.keyword"
            SortByEmail -> ES.FieldName "email.keyword"
            SortBySAMLIdp -> ES.FieldName "saml_idp"
            SortByManagedBy -> ES.FieldName "managed_by"
            SortByRole -> ES.FieldName "role"
            SortByCreatedAt -> ES.FieldName "created_at"
        )
        ( case sortOrder of
            SortOrderAsc -> ES.Ascending
            SortOrderDesc -> ES.Descending
        )
        Nothing
        Nothing
        Nothing
        Nothing

mkUserQuery :: UserId -> Maybe TeamId -> TeamSearchInfo -> ES.Query -> IndexQuery Contact
mkUserQuery searcher mSearcherTeamId teamSearchInfo q =
  IndexQuery
    q
    ( ES.Filter
        . ES.QueryBoolQuery
        $ boolQuery
          { ES.boolQueryMustNotMatch = maybeToList $ matchSelf searcher,
            ES.boolQueryMustMatch =
              [ restrictSearchSpace mSearcherTeamId teamSearchInfo,
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
    )
    []

termQ :: Text -> Text -> ES.Query
termQ f v =
  ES.TermQuery
    ES.Term
      { ES.termField = Key.fromText f,
        ES.termValue = v
      }
    Nothing

matchSelf :: UserId -> Maybe ES.Query
matchSelf searcher = Just (termQ "_id" (idToText searcher))

-- | See 'TeamSearchInfo'
restrictSearchSpace :: Maybe TeamId -> TeamSearchInfo -> ES.Query
-- restrictSearchSpace (FederatedSearch Nothing) =
--   ES.QueryBoolQuery
--     boolQuery
--       { ES.boolQueryShouldMatch =
--           [ matchNonTeamMemberUsers,
--             matchTeamMembersSearchableByAllTeams
--           ]
--       }
-- restrictSearchSpace (FederatedSearch (Just [])) =
--   ES.QueryBoolQuery
--     boolQuery
--       { ES.boolQueryMustMatch =
--           [ -- if the list of allowed teams is empty, this is impossible to fulfill, and no results will be returned
--             -- this case should be handled earlier, so this is just a safety net
--             ES.TermQuery (ES.Term "team" "must not match any team") Nothing
--           ]
--       }
-- restrictSearchSpace (FederatedSearch (Just teams)) =
--   ES.QueryBoolQuery
--     boolQuery
--       { ES.boolQueryMustMatch =
--           [ matchTeamMembersSearchableByAllTeams,
--             onlyInTeams
--           ]
--       }
--   where
--     onlyInTeams = ES.QueryBoolQuery boolQuery {ES.boolQueryShouldMatch = map matchTeamMembersOf teams}
restrictSearchSpace mteam searchInfo =
  case (mteam, searchInfo) of
    (Nothing, _) -> matchNonTeamMemberUsers
    (Just _, NoTeam) -> matchNonTeamMemberUsers
    (Just searcherTeam, TeamOnly team) ->
      if searcherTeam == team
        then matchTeamMembersOf team
        else ES.QueryMatchNoneQuery
    (Just searcherTeam, AllUsers) ->
      ES.QueryBoolQuery
        boolQuery
          { ES.boolQueryShouldMatch =
              [ matchNonTeamMemberUsers,
                matchTeamMembersSearchableByAllTeams,
                matchTeamMembersOf searcherTeam
              ]
          }

matchTeamMembersOf :: TeamId -> ES.Query
matchTeamMembersOf team = ES.TermQuery (ES.Term "team" $ idToText team) Nothing

matchTeamMembersSearchableByAllTeams :: ES.Query
matchTeamMembersSearchableByAllTeams =
  ES.QueryBoolQuery
    boolQuery
      { ES.boolQueryMustMatch =
          [ ES.QueryExistsQuery $ ES.FieldName "team",
            ES.TermQuery (ES.Term searchVisibilityInboundFieldName "searchable-by-all-teams") Nothing
          ]
      }

matchNonTeamMemberUsers :: ES.Query
matchNonTeamMemberUsers =
  ES.QueryBoolQuery
    boolQuery
      { ES.boolQueryMustNotMatch = [ES.QueryExistsQuery $ ES.FieldName "team"]
      }

matchUsersNotInTeam :: TeamId -> ES.Query
matchUsersNotInTeam tid =
  ES.QueryBoolQuery
    boolQuery
      { ES.boolQueryMustNotMatch = [ES.TermQuery (ES.Term "team" $ idToText tid) Nothing]
      }

--------------------------------------------
-- Utils

runInBothES :: forall m a. (MonadIO m) => IndexedUserStoreConfig -> (ES.IndexName -> ES.BH m a) -> m (a, Maybe a)
runInBothES cfg f =
  either (liftIO . throwIO) pure =<< runExceptT do
    x <- ExceptT $ ES.runBH cfg.conn.env $ f cfg.conn.indexName
    y <- forM @Maybe @(ExceptT ES.EsError m) cfg.additionalConn $ \additional ->
      ExceptT $ ES.runBH additional.env $ f additional.indexName
    pure (x, y)

boolQuery :: ES.BoolQuery
boolQuery = ES.mkBoolQuery [] [] [] []
