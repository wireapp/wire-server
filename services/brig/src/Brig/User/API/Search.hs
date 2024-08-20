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

module Brig.User.API.Search
  ( search,
    teamUserSearch,
    refreshIndex,
  )
where

import Brig.API.Error (fedError)
import Brig.API.Handler
import Brig.App
import Brig.Data.User qualified as DB
import Brig.Effects.FederationConfigStore
import Brig.Effects.FederationConfigStore qualified as E
import Brig.Federation.Client qualified as Federation
import Brig.Options qualified as Opts
import Brig.Team.Util (ensurePermissions, ensurePermissionsOrPersonalUser)
import Brig.User.API.Handle qualified as HandleAPI
import Brig.User.Search.Index
import Brig.User.Search.SearchIndex qualified as Q
import Brig.User.Search.TeamUserSearch qualified as Q
import Control.Lens (view)
import Data.Domain (Domain)
import Data.Handle qualified as Handle
import Data.Id
import Data.Range
import Imports
import Network.Wai.Utilities ((!>>))
import Polysemy
import System.Logger (field, msg)
import System.Logger.Class (val, (~~))
import System.Logger.Class qualified as Log
import Wire.API.Federation.API.Brig qualified as FedBrig
import Wire.API.Federation.API.Brig qualified as S
import Wire.API.Routes.FederationDomainConfig
import Wire.API.Team.Member (HiddenPerm (SearchContacts))
import Wire.API.Team.Permission qualified as Public
import Wire.API.Team.SearchVisibility (TeamSearchVisibility (..))
import Wire.API.User.Search
import Wire.API.User.Search qualified as Public
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.UserSearch.Types qualified as Search
import Wire.UserStore (UserStore)
import Wire.UserSubsystem

-- FUTUREWORK: Consider augmenting 'SearchResult' with full user profiles
-- for all results. This is tracked in https://wearezeta.atlassian.net/browse/SQCORE-599
search ::
  ( Member GalleyAPIAccess r,
    Member FederationConfigStore r,
    Member UserStore r,
    Member UserSubsystem r
  ) =>
  UserId ->
  Text ->
  Maybe Domain ->
  Maybe (Range 1 500 Int32) ->
  (Handler r) (Public.SearchResult Public.Contact)
search searcherId searchTerm maybeDomain maybeMaxResults = do
  -- FUTUREWORK(fisx): to reduce cassandra traffic, 'ensurePermissionsOrPersonalUser' could be
  -- run from `searchLocally` and `searchRemotely`, resp., where the team id is already
  -- available (at least in the local case) and can be passed as an argument rather than
  -- looked up again.
  ensurePermissionsOrPersonalUser searcherId [SearchContacts]
  federationDomain <- viewFederationDomain
  mSearcherTeamId <- lift $ wrapClient $ DB.lookupUserTeam searcherId
  let queryDomain = fromMaybe federationDomain maybeDomain
  if queryDomain == federationDomain
    then searchLocally searcherId searchTerm maybeMaxResults
    else searchRemotely queryDomain mSearcherTeamId searchTerm

searchRemotely :: (Member FederationConfigStore r) => Domain -> Maybe TeamId -> Text -> (Handler r) (Public.SearchResult Public.Contact)
searchRemotely domain mTid searchTerm = do
  lift . Log.info $
    msg (val "searchRemotely")
      ~~ field "domain" (show domain)
      ~~ field "searchTerm" searchTerm
  mFedCnf <- lift $ liftSem $ E.getFederationConfig domain
  let onlyInTeams = case restriction <$> mFedCnf of
        Just FederationRestrictionAllowAll -> Nothing
        Just (FederationRestrictionByTeam teams) -> Just teams
        -- if we are not federating at all, we also do not allow to search any remote teams
        Nothing -> Just []

  searchResponse <- Federation.searchUsers domain (FedBrig.SearchRequest searchTerm mTid onlyInTeams) !>> fedError
  let contacts = S.contacts searchResponse
  let count = length contacts
  pure
    SearchResult
      { searchResults = contacts,
        searchFound = count,
        searchReturned = count,
        searchTook = 0,
        searchPolicy = S.searchPolicy searchResponse,
        searchPagingState = Nothing,
        searchHasMore = Nothing
      }

searchLocally ::
  forall r.
  ( Member GalleyAPIAccess r,
    Member UserSubsystem r,
    Member UserStore r
  ) =>
  UserId ->
  Text ->
  Maybe (Range 1 500 Int32) ->
  (Handler r) (Public.SearchResult Public.Contact)
searchLocally searcherId searchTerm maybeMaxResults = do
  let maxResults = maybe 15 (fromIntegral . fromRange) maybeMaxResults
  searcherTeamId <- lift $ wrapClient $ DB.lookupUserTeam searcherId
  teamSearchInfo <- mkTeamSearchInfo searcherTeamId

  maybeExactHandleMatch <- exactHandleSearch

  let exactHandleMatchCount = length maybeExactHandleMatch
      esMaxResults = maxResults - exactHandleMatchCount

  esResult <-
    if esMaxResults > 0
      then Q.searchIndex (Q.LocalSearch searcherId searcherTeamId teamSearchInfo) searchTerm esMaxResults
      else pure $ SearchResult 0 0 0 [] FullSearch Nothing Nothing

  -- Prepend results matching exact handle and results from ES.
  pure $
    esResult
      { searchResults = maybeToList maybeExactHandleMatch <> searchResults esResult,
        searchFound = exactHandleMatchCount + searchFound esResult,
        searchReturned = exactHandleMatchCount + searchReturned esResult
      }
  where
    handleTeamVisibility :: TeamId -> TeamSearchVisibility -> Search.TeamSearchInfo
    handleTeamVisibility _ SearchVisibilityStandard = Search.AllUsers
    handleTeamVisibility t SearchVisibilityNoNameOutsideTeam = Search.TeamOnly t

    mkTeamSearchInfo :: Maybe TeamId -> (Handler r) Search.TeamSearchInfo
    mkTeamSearchInfo searcherTeamId = lift $ do
      sameTeamSearchOnly <- fromMaybe False <$> view (settings . Opts.searchSameTeamOnly)
      case searcherTeamId of
        Nothing -> pure Search.NoTeam
        Just t ->
          -- This flag in brig overrules any flag on galley - it is system wide
          if sameTeamSearchOnly
            then pure (Search.TeamOnly t)
            else do
              -- For team users, we need to check the visibility flag
              handleTeamVisibility t <$> liftSem (GalleyAPIAccess.getTeamSearchVisibility t)

    exactHandleSearch :: (Handler r) (Maybe Contact)
    exactHandleSearch = do
      lsearcherId <- qualifyLocal searcherId
      case Handle.parseHandle searchTerm of
        Nothing -> pure Nothing
        Just handle -> do
          HandleAPI.contactFromProfile
            <$$> HandleAPI.getLocalHandleInfo lsearcherId handle

teamUserSearch ::
  (Member GalleyAPIAccess r) =>
  UserId ->
  TeamId ->
  Maybe Text ->
  Maybe RoleFilter ->
  Maybe TeamUserSearchSortBy ->
  Maybe TeamUserSearchSortOrder ->
  Maybe (Range 1 500 Int32) ->
  Maybe PagingState ->
  (Handler r) (Public.SearchResult Public.TeamContact)
teamUserSearch uid tid mQuery mRoleFilter mSortBy mSortOrder size mPagingState = do
  ensurePermissions uid tid [Public.AddTeamMember] -- limit this to team admins to reduce risk of involuntary DOS attacks.  (also, this way we don't need to worry about revealing confidential user data to other team members.)
  Q.teamUserSearch tid mQuery mRoleFilter mSortBy mSortOrder (fromMaybe (unsafeRange 15) size) mPagingState
