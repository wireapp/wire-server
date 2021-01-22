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

-- TODO: rename module
module Brig.User.Search.TeamUserSearch
  ( teamUserSearch,
    teamUserSearchQuery,
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
import Wire.API.Team.Role
import Wire.API.User.Search

teamUserSearch ::
  (HasCallStack, MonadIndexIO m) =>
  TeamId ->
  Maybe Text ->
  Maybe [Role] ->
  Maybe Text ->
  Maybe Text ->
  Range 1 500 Int32 ->
  m (SearchResult TeamContact)
teamUserSearch tid mbSearchText _mRoleFilter _mSortBy _mSortOrder (fromRange -> s) = liftIndexIO $ do
  let (IndexQuery q f sortSpecs) = teamUserSearchQuery tid mbSearchText
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

-- TODO: Maybe (sortby=<name|handle|email|saml_idp|managed_by|role|created_at>, Maybe ES.SortOrder)
-- TODO: Maybe [Role]
-- analogous to SearchIndex.hs
teamUserSearchQuery ::
  TeamId ->
  Maybe Text ->
  IndexQuery TeamContact
teamUserSearchQuery tid mbSearchText =
  case mbQStr of
    Nothing ->
      IndexQuery
        (ES.MatchAllQuery Nothing)
        teamFilter
        [sortByCreatedAt]
    Just qStr ->
      IndexQuery
        (matchPhraseOrPrefix qStr)
        teamFilter
        []
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

    sortByCreatedAt =
      ES.DefaultSort (ES.FieldName "created_at") ES.Descending Nothing Nothing Nothing Nothing
