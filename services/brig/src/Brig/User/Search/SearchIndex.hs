{-# LANGUAGE RecordWildCards #-}
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

module Brig.User.Search.SearchIndex
  ( searchIndex,
    SearchSetting (..),
  )
where

import Brig.App (Env, viewFederationDomain)
import Brig.User.Search.Index
import Control.Lens hiding (setting, (#), (.=))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Aeson.Key qualified as Key
import Data.Domain (Domain)
import Data.Handle (Handle (fromHandle))
import Data.Id
import Data.Qualified (Qualified (Qualified))
import Database.Bloodhound qualified as ES
import Imports hiding (log, searchable)
import Wire.API.User (ColourId (..), Name (fromName))
import Wire.API.User.Search
-- TODO: importing interpreters here is not ideal, perhaps much of this code
-- will go into the interpreter in following commits.
import Wire.IndexedUserStore.ElasticSearch (IndexedUserStoreError (..), mappingName)
import Wire.UserSearch.Types
import Wire.UserStore.IndexUser (normalized)

-- | User that is performing the search
-- Team of user that is performing the search
-- Outgoing search restrictions
data SearchSetting
  = FederatedSearch (Maybe [TeamId])
  | LocalSearch
      UserId
      (Maybe TeamId)
      TeamSearchInfo

searchSettingTeam :: SearchSetting -> Maybe TeamId
searchSettingTeam (FederatedSearch _) = Nothing
searchSettingTeam (LocalSearch _ mbTeam _) = mbTeam

searchIndex ::
  (MonadIndexIO m, MonadReader Env m) =>
  -- | The user performing the search.
  SearchSetting ->
  -- | The search query
  Text ->
  -- | The maximum number of results.
  Int ->
  m (SearchResult Contact)
searchIndex setting q = queryIndex (defaultUserQuery setting q)

queryIndex ::
  (MonadIndexIO m, MonadReader Env m) =>
  IndexQuery r ->
  Int ->
  m (SearchResult Contact)
queryIndex (IndexQuery q f _) s = do
  localDomain <- viewFederationDomain
  liftIndexIO $ do
    idx <- asks idxName
    let search = (ES.mkSearch (Just q) (Just f)) {ES.size = ES.Size (fromIntegral s)}
    r <-
      ES.searchByType idx mappingName search
        >>= ES.parseEsResponse @_ @(ES.SearchResult UserDoc)
    either (throwM . IndexLookupError) (traverse (userDocToContact localDomain) . mkResult) r
  where
    mkResult es =
      let results = mapMaybe ES.hitSource . ES.hits . ES.searchHits $ es
       in SearchResult
            { searchFound = ES.hitsTotal . ES.searchHits $ es,
              searchReturned = length results,
              searchTook = ES.took es,
              searchResults = results,
              searchPolicy = FullSearch,
              searchPagingState = Nothing,
              searchHasMore = Nothing
            }

userDocToContact :: (MonadThrow m) => Domain -> UserDoc -> m Contact
userDocToContact localDomain UserDoc {..} = do
  let contactQualifiedId = Qualified udId localDomain
  contactName <- maybe (throwM $ IndexError "Name not found") (pure . fromName) udName
  let contactColorId = fromIntegral . fromColourId <$> udColourId
      contactHandle = fromHandle <$> udHandle
      contactTeam = udTeam
  pure $ Contact {..}

-- | The default or canonical 'IndexQuery'.
--
-- The intention behind parameterising 'queryIndex' over the 'IndexQuery' is that
-- it allows to experiment with different queries (perhaps in an A/B context).
--
-- FUTUREWORK: Drop legacyPrefixMatch
defaultUserQuery :: SearchSetting -> Text -> IndexQuery Contact
defaultUserQuery setting (normalized -> term') =
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
      queryWithBoost setting' =
        ES.QueryBoostingQuery
          ES.BoostingQuery
            { ES.positiveQuery = query,
              ES.negativeQuery = maybe ES.QueryMatchNoneQuery matchUsersNotInTeam (searchSettingTeam setting'),
              ES.negativeBoost = ES.Boost 0.1
            }
   in mkUserQuery setting (queryWithBoost setting)

mkUserQuery :: SearchSetting -> ES.Query -> IndexQuery Contact
mkUserQuery setting q =
  IndexQuery
    q
    ( ES.Filter
        . ES.QueryBoolQuery
        $ boolQuery
          { ES.boolQueryMustNotMatch = maybeToList $ matchSelf setting,
            ES.boolQueryMustMatch =
              [ restrictSearchSpace setting,
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
      { ES.termField = f,
        ES.termValue = v
      }
    Nothing

matchSelf :: SearchSetting -> Maybe ES.Query
matchSelf (FederatedSearch _) = Nothing
matchSelf (LocalSearch searcher _tid _searchInfo) = Just (termQ "_id" (idToText searcher))

-- | See 'TeamSearchInfo'
restrictSearchSpace :: SearchSetting -> ES.Query
restrictSearchSpace (FederatedSearch Nothing) =
  ES.QueryBoolQuery
    boolQuery
      { ES.boolQueryShouldMatch =
          [ matchNonTeamMemberUsers,
            matchTeamMembersSearchableByAllTeams
          ]
      }
restrictSearchSpace (FederatedSearch (Just [])) =
  ES.QueryBoolQuery
    boolQuery
      { ES.boolQueryMustMatch =
          [ -- if the list of allowed teams is empty, this is impossible to fulfill, and no results will be returned
            -- this case should be handled earlier, so this is just a safety net
            ES.TermQuery (ES.Term "team" "must not match any team") Nothing
          ]
      }
restrictSearchSpace (FederatedSearch (Just teams)) =
  ES.QueryBoolQuery
    boolQuery
      { ES.boolQueryMustMatch =
          [ matchTeamMembersSearchableByAllTeams,
            onlyInTeams
          ]
      }
  where
    onlyInTeams = ES.QueryBoolQuery boolQuery {ES.boolQueryShouldMatch = map matchTeamMembersOf teams}
restrictSearchSpace (LocalSearch _uid mteam searchInfo) =
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
            ES.TermQuery (ES.Term (Key.toText searchVisibilityInboundFieldName) "searchable-by-all-teams") Nothing
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
