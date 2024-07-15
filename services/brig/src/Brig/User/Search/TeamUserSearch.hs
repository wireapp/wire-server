{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

module Brig.User.Search.TeamUserSearch
  ( teamUserSearch,
    teamUserSearchQuery,
    TeamUserSearchSortBy (..),
    TeamUserSearchSortOrder (..),
    RoleFilter (..),
  )
where

import Brig.User.Search.Index
import Control.Error (lastMay)
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Aeson (decode', encode)
import Data.ByteString (fromStrict, toStrict)
import Data.Id (TeamId, idToText)
import Data.Range (Range (..))
import Data.Text.Ascii (decodeBase64Url, encodeBase64Url)
import Database.Bloodhound qualified as ES
import Imports hiding (log, searchable)
import Wire.API.User.Search

teamUserSearch ::
  (HasCallStack, MonadIndexIO m) =>
  TeamId ->
  Maybe Text ->
  Maybe RoleFilter ->
  Maybe TeamUserSearchSortBy ->
  Maybe TeamUserSearchSortOrder ->
  Range 1 500 Int32 ->
  Maybe PagingState ->
  m (SearchResult TeamContact)
teamUserSearch tid mbSearchText mRoleFilter mSortBy mSortOrder (fromRange -> size) mPagingState = liftIndexIO $ do
  let (IndexQuery q f sortSpecs) = teamUserSearchQuery tid mbSearchText mRoleFilter mSortBy mSortOrder
  idx <- asks idxName
  let search =
        (ES.mkSearch (Just q) (Just f))
          { -- we are requesting one more result than the page size to determine if there is a next page
            ES.size = ES.Size (fromIntegral size + 1),
            ES.sortBody = Just (fmap ES.DefaultSortSpec sortSpecs),
            ES.searchAfterKey = toSearchAfterKey =<< mPagingState
          }
  r :: Either ES.EsError (ES.SearchResult TeamContact) <-
    ES.searchByType idx mappingName search
      >>= ES.parseEsResponse
  -- TODO: revert this file
  _ <- either (throwM . IndexLookupError) (pure . mkResult) r
  throwM $ IndexError "oops"
  where
    toSearchAfterKey :: PagingState -> Maybe ES.SearchAfterKey
    toSearchAfterKey ps = decode' . fromStrict =<< (decodeBase64Url . unPagingState $ ps)

    fromSearchAfterKey :: ES.SearchAfterKey -> PagingState
    fromSearchAfterKey = PagingState . encodeBase64Url . toStrict . encode

    mkResult es =
      let hitsPlusOne = ES.hits . ES.searchHits $ es
          hits = take (fromIntegral size) hitsPlusOne
          mps = fromSearchAfterKey <$> lastMay (mapMaybe ES.hitSort hits)
          results = mapMaybe ES.hitSource hits
       in SearchResult
            { searchFound = ES.hitsTotal . ES.searchHits $ es,
              searchReturned = length results,
              searchTook = ES.took es,
              searchResults = results,
              searchPolicy = FullSearch,
              searchPagingState = mps,
              searchHasMore = Just $ length hitsPlusOne > length hits
            }

-- FUTURWORK: Implement role filter (needs galley data)
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
