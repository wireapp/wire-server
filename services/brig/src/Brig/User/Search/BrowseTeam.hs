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

module Brig.User.Search.BrowseTeam
  ( browseTeam,
    browseTeamQuery,
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

browseTeam ::
  (HasCallStack, MonadIndexIO m) =>
  TeamId ->
  Maybe Text ->
  Maybe [Role] ->
  Maybe Text ->
  Maybe Text ->
  Range 1 500 Int32 ->
  m (SearchResult TeamContact)
browseTeam tid mbSearchText _mRoleFilter _mSortBy _mSortOrder (fromRange -> s) = liftIndexIO $ do
  let (IndexQuery q f) = browseTeamQuery tid mbSearchText
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

-- TODO: Maybe (sortby=<name|handle|email|saml_idp|managed_by|role|created_at>, Maybe ES.SortOrder)
-- TODO: Maybe [Role]
-- analogous to SearchIndex.hs
browseTeamQuery ::
  TeamId ->
  Maybe Text ->
  IndexQuery TeamContact
browseTeamQuery tid mbSearchText =
  IndexQuery query filterQuery
  where
    query = case mbSearchText of
      Nothing -> ES.MatchAllQuery Nothing
      Just (normalized -> term') -> matchPhraseOrPrefix term'

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

    filterQuery =
      ES.Filter $
        ES.QueryBoolQuery boolQuery {ES.boolQueryMustMatch = [matchTeamMembersOf]}

    matchTeamMembersOf = ES.TermQuery (ES.Term "team" $ idToText tid) Nothing
