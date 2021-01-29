{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Brig.User.Search.TeamUserSearch
  ( teamUserSearch,
    teamUserSearchQuery,
    TeamUserSearchSortBy (..),
    TeamUserSearchSortOrder (..),
    RoleFilter (..),
  )
where

import Brig.Data.Instances ()
import Brig.Types.Search
import Brig.User.Search.Index
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Id (TeamId, idToText)
import Data.Range (Range (..))
import qualified Database.Bloodhound as ES
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
  m (SearchResult TeamContact)
teamUserSearch tid mbSearchText mRoleFilter mSortBy mSortOrder (fromRange -> s) = liftIndexIO $ do
  let (IndexQuery q f sortSpecs) = teamUserSearchQuery tid mbSearchText mRoleFilter mSortBy mSortOrder
  idx <- asks idxName
  let search =
        (ES.mkSearch (Just q) (Just f))
          { ES.size = ES.Size (fromIntegral s),
            ES.sortBody = Just (fmap ES.DefaultSortSpec sortSpecs)
          }
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
    ( maybe
        [defaultSort SortByCreatedAt SortOrderDesc | isNothing mbQStr]
        (\tuSortBy -> [defaultSort tuSortBy (fromMaybe SortOrderAsc mSortOrder)])
        mSortBy
    )
  where
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
