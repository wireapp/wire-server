{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
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

module Brig.User.Search.SearchIndex
  ( searchIndex,
  )
where

import Brig.App (Env, viewFederationDomain)
import Brig.Data.Instances ()
import Brig.Types.Search
import Brig.User.Search.Index
import Control.Lens hiding ((#), (.=))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Except
import Data.Domain (Domain)
import Data.Handle (Handle (fromHandle))
import Data.Id
import Data.Qualified (Qualified (Qualified))
import qualified Database.Bloodhound as ES
import Imports hiding (log, searchable)
import Wire.API.User (ColourId (..), Name (fromName))

searchIndex ::
  (MonadIndexIO m, MonadReader Env m) =>
  -- | The user performing the search.
  UserId ->
  TeamSearchInfo ->
  -- | The search query
  Text ->
  -- | The maximum number of results.
  Int ->
  m (SearchResult Contact)
searchIndex u teamSearchInfo q = queryIndex (defaultUserQuery u teamSearchInfo q)

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
              searchResults = results
            }

userDocToContact :: MonadThrow m => Domain -> UserDoc -> m Contact
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
defaultUserQuery :: UserId -> TeamSearchInfo -> Text -> IndexQuery Contact
defaultUserQuery u teamSearchInfo (normalized -> term') =
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
                    boolQuery
                      { ES.boolQueryShouldMatch = [matchPhraseOrPrefix, legacyPrefixMatch],
                        ES.boolQueryMustNotMatch = [termQ "handle" term']
                      }
                ],
              -- This removes exact handle matches, as they are fetched from cassandra
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
  IndexQuery
    q
    ( ES.Filter . ES.QueryBoolQuery $
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
