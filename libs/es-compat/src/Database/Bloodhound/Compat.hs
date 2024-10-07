-- for putMapping
{-# OPTIONS_GHC -Wno-deprecations #-}

module Database.Bloodhound.Compat
  ( putMapping,
    indexDocument,
    ElasticSearchVersion (..),
    IndexedDocument' (..),
    searchByIndex,
  )
where

import Data.Aeson
import Data.Aeson.Key
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
import Data.Text
import Data.Text qualified as T
import Database.Bloodhound (HitsTotal (HitsTotal))
import Database.Bloodhound qualified as ES
import Database.Bloodhound.Common.Requests qualified as ESR
import GHC.Base
import Imports
import Network.HTTP.Types qualified as HTTP

-- | Version to use for requests
data ElasticSearchVersion = ES6 | ES8

putMapping :: (FromJSON r, ToJSON a) => ElasticSearchVersion -> ES.IndexName -> Text -> a -> ES.BHRequest ES.StatusDependant r
putMapping ES6 indexName mappingName mapping =
  -- "_mapping" above is originally transposed
  -- erroneously. The correct API call is: "/INDEX/_mapping"
  put (ES.mkEndpoint [ES.unIndexName indexName, "_mapping", mappingName]) (encode mapping)
putMapping ES8 indexName _mappingName mapping = ESR.putMapping indexName mapping

put ::
  (ES.ParseBHResponse contextualized, FromJSON body) =>
  ES.Endpoint ->
  LBS.ByteString ->
  ES.BHRequest contextualized body
put = ES.mkFullRequest HTTP.methodPut

post ::
  (ES.ParseBHResponse contextualized, FromJSON body) =>
  ES.Endpoint ->
  LBS.ByteString ->
  ES.BHRequest contextualized body
post = ES.mkFullRequest HTTP.methodPost

indexDocument ::
  (ToJSON doc) =>
  ElasticSearchVersion ->
  ES.IndexName ->
  Text ->
  ES.IndexDocumentSettings ->
  doc ->
  ES.DocId ->
  ES.BHRequest ES.StatusDependant IndexedDocument'
indexDocument ES6 indexName mappingName cfg document (ES.DocId docId) =
  put endpoint (encode body)
  where
    endpoint = ES.mkEndpoint [ES.unIndexName indexName, mappingName, docId] `ES.withQueries` indexQueryString cfg (ES.DocId docId)
    body = encodeDocument cfg document
indexDocument ES8 indexName _mappingName cfg document docId = castToMigrationType <$> ESR.indexDocument indexName cfg document docId
  where
    castToMigrationType :: ESR.IndexedDocument -> IndexedDocument'
    castToMigrationType d =
      IndexedDocument'
        { idxDocIndex = d.idxDocIndex,
          idxDocType = d.idxDocType,
          idxDocId = d.idxDocId,
          idxDocVersion = d.idxDocVersion,
          idxDocResult = d.idxDocResult,
          idxDocShards = d.idxDocShards
        }

encodeDocument :: (ToJSON doc) => ES.IndexDocumentSettings -> doc -> Value
encodeDocument cfg document =
  case ES.idsJoinRelation cfg of
    Nothing -> toJSON document
    Just (ES.ParentDocument (ES.FieldName field) name) ->
      mergeObjects (toJSON document) (object [fromText field .= name])
    Just (ES.ChildDocument (ES.FieldName field) name parent) ->
      mergeObjects (toJSON document) (object [fromText field .= object ["name" .= name, "parent" .= parent]])
  where
    mergeObjects (Object a) (Object b) = Object (a <> b)
    mergeObjects _ _ = error "Impossible happened: both document body and join parameters must be objects"

indexQueryString :: ES.IndexDocumentSettings -> ES.DocId -> [(Text, Maybe Text)]
indexQueryString cfg (ES.DocId docId) =
  versionCtlParams cfg <> routeParams
  where
    routeParams = case ES.idsJoinRelation cfg of
      Nothing -> []
      Just (ES.ParentDocument _ _) -> [("routing", Just docId)]
      Just (ES.ChildDocument _ _ (ES.DocId pid)) -> [("routing", Just pid)]

versionCtlParams :: ES.IndexDocumentSettings -> [(Text, Maybe Text)]
versionCtlParams cfg =
  case ES.idsVersionControl cfg of
    ES.NoVersionControl -> []
    ES.InternalVersion v -> versionParams v "internal"
    ES.ExternalGT (ES.ExternalDocVersion v) -> versionParams v "external_gt"
    ES.ExternalGTE (ES.ExternalDocVersion v) -> versionParams v "external_gte"
    ES.ForceVersion (ES.ExternalDocVersion v) -> versionParams v "force"
  where
    vt = showText . ES.docVersionNumber
    versionParams :: ES.DocVersion -> Text -> [(Text, Maybe Text)]
    versionParams v t =
      [ ("version", Just $ vt v),
        ("version_type", Just t)
      ]

showText :: (Show a) => a -> Text
showText = T.pack . show

data IndexedDocument' = IndexedDocument'
  { idxDocIndex :: Text,
    idxDocType :: Maybe Text,
    idxDocId :: Text,
    idxDocVersion :: Int,
    idxDocResult :: Text,
    idxDocShards :: ES.ShardResult
  }
  deriving stock (Eq, Show)

instance FromJSON IndexedDocument' where
  parseJSON =
    withObject "IndexedDocument" $ \v ->
      IndexedDocument'
        <$> v
          .: "_index"
        <*> v
          .:? "_type"
        <*> v
          .: "_id"
        <*> v
          .: "_version"
        <*> v
          .: "result"
        <*> v
          .: "_shards"

data SearchHits' a = SearchHits'
  { hitsTotal :: Int,
    maxScore :: ES.Score,
    hits :: [ES.Hit a]
  }
  deriving stock (Eq, Show)

instance (FromJSON a) => FromJSON (SearchHits' a) where
  parseJSON (Object v) =
    SearchHits'
      <$> v .: "total"
      <*> v .: "max_score"
      <*> v .: "hits"
  parseJSON _ = GHC.Base.empty

instance Semigroup (SearchHits' a) where
  (SearchHits' ta ma ha) <> (SearchHits' tb mb hb) =
    SearchHits' (ta + tb) (max ma mb) (ha <> hb)

instance Monoid (SearchHits' a) where
  mempty = SearchHits' 0 Nothing mempty
  mappend = (<>)

data SearchResult' a = SearchResult'
  { took :: Int,
    timedOut :: Bool,
    shards :: ES.ShardResult,
    searchHits :: SearchHits' a,
    aggregations :: Maybe ES.AggregationResults,
    -- | Only one Suggestion request / response per
    --   Search is supported.
    scrollId :: Maybe ES.ScrollId,
    suggest :: Maybe ES.NamedSuggestionResponse,
    pitId :: Maybe Text
  }
  deriving stock (Eq, Show)

instance (FromJSON a) => FromJSON (SearchResult' a) where
  parseJSON (Object v) =
    SearchResult'
      <$> v
        .: "took"
      <*> v
        .: "timed_out"
      <*> v
        .: "_shards"
      <*> v
        .: "hits"
      <*> v
        .:? "aggregations"
      <*> v
        .:? "_scroll_id"
      <*> v
        .:? "suggest"
      <*> v
        .:? "pit_id"
  parseJSON _ = GHC.Base.empty

searchByIndex :: (FromJSON a) => ElasticSearchVersion -> ES.IndexName -> ES.Search -> ES.BHRequest ES.StatusDependant (ES.SearchResult a)
searchByIndex ES6 indexName search =
  let endpoint = ES.mkEndpoint [ES.unIndexName indexName, "_search"]
   in castToMigrationType <$> dispatchSearch endpoint search
  where
    castToMigrationType :: SearchResult' a -> ES.SearchResult a
    castToMigrationType r =
      ES.SearchResult
        { took = r.took,
          timedOut = r.timedOut,
          shards = r.shards,
          searchHits = (castSearchHits r.searchHits),
          aggregations = r.aggregations,
          scrollId = r.scrollId,
          suggest = r.suggest,
          pitId = r.pitId
        }

    castSearchHits :: SearchHits' a -> ES.SearchHits a
    castSearchHits h =
      ES.SearchHits
        { hitsTotal = HitsTotal h.hitsTotal ES.HTR_GTE,
          maxScore = h.maxScore,
          hits = h.hits
        }
searchByIndex ES8 indexName search = ESR.searchByIndex indexName search

dispatchSearch :: (FromJSON a) => ES.Endpoint -> ES.Search -> ES.BHRequest ES.StatusDependant (SearchResult' a)
dispatchSearch endpoint search =
  post url' (encode search)
  where
    url' = appendSearchTypeParam endpoint (ES.searchType search)
    appendSearchTypeParam :: ES.Endpoint -> ES.SearchType -> ES.Endpoint
    appendSearchTypeParam originalUrl st = originalUrl `ES.withQueries` params
      where
        stText = "search_type"
        params
          | st == ES.SearchTypeDfsQueryThenFetch = [(stText, Just "dfs_query_then_fetch")]
          -- used to catch 'SearchTypeQueryThenFetch', which is also the default
          | otherwise = []
