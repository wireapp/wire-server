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
  ( routesPublic,
    routesInternal,
    search,
  )
where

import Brig.API.Error (fedError)
import Brig.API.Handler
import Brig.App
import qualified Brig.Data.User as DB
import qualified Brig.Federation.Client as Federation
import qualified Brig.IO.Intra as Intra
import qualified Brig.Options as Opts
import Brig.Team.Util (ensurePermissions)
import Brig.Types.Search as Search
import Brig.User.API.Handle (contactFromProfile)
import qualified Brig.User.API.Handle as HandleAPI
import Brig.User.Search.Index
import qualified Brig.User.Search.SearchIndex as Q
import Brig.User.Search.TeamUserSearch (RoleFilter (..), TeamUserSearchSortBy (..), TeamUserSearchSortOrder (..))
import qualified Brig.User.Search.TeamUserSearch as Q
import Control.Lens (view)
import Data.Domain (Domain)
import Data.Handle (parseHandle)
import Data.Id
import Data.Predicate
import Data.Range
import qualified Data.Swagger.Build.Api as Doc
import qualified Galley.Types.Teams.SearchVisibility as Team
import Imports
import Network.Wai (Response)
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Routing
import Network.Wai.Utilities ((!>>))
import Network.Wai.Utilities.Response (empty, json)
import Network.Wai.Utilities.Swagger (document)
import System.Logger (field, msg)
import System.Logger.Class (val, (~~))
import qualified System.Logger.Class as Log
import qualified Wire.API.Federation.API.Brig as FedBrig
import qualified Wire.API.Federation.API.Brig as S
import qualified Wire.API.Team.Permission as Public
import Wire.API.Team.SearchVisibility (TeamSearchVisibility)
import Wire.API.User.Search (FederatedUserSearchPolicy (FullSearch))
import qualified Wire.API.User.Search as Public

routesPublic :: Routes Doc.ApiBuilder (Handler r) ()
routesPublic = do
  get "/teams/:tid/search" (continue teamUserSearchH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. capture "tid"
      .&. opt (query "q")
      .&. opt (query "frole")
      .&. opt (query "sortby")
      .&. opt (query "sortorder")
      .&. def (unsafeRange 15) (query "size")

  document "GET" "browse team" $ do
    Doc.summary "Browse team for members (requires add-user permission)"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      Doc.description "ID of the team to be browsed"
    Doc.parameter Doc.Query "q" Doc.string' $ do
      Doc.description "Search expression"
      Doc.optional
    Doc.parameter Doc.Query "frole" Doc.string' $ do
      Doc.description "Role filter, eg. `member,external-partner`.  Empty list means do not filter."
      Doc.optional
    Doc.parameter Doc.Query "sortby" Doc.string' $ do
      Doc.description "Can be one of name, handle, email, saml_idp, managed_by, role, created_at."
      Doc.optional
    Doc.parameter Doc.Query "sortorder" Doc.string' $ do
      Doc.description "Can be one of asc, desc."
      Doc.optional
    Doc.parameter Doc.Query "size" Doc.int32' $ do
      Doc.description "Number of results to return (min: 1, max: 500, default: 15)"
      Doc.optional
    Doc.returns (Doc.ref $ Public.modelSearchResult Public.modelTeamContact)
    Doc.response 200 "The list of hits." Doc.end

routesInternal :: Routes a (Handler r) ()
routesInternal = do
  -- make index updates visible (e.g. for integration testing)
  post
    "/i/index/refresh"
    (continue (const $ lift refreshIndex $> empty))
    true

  -- reindex from Cassandra (e.g. integration testing -- prefer the
  -- `brig-index` executable for actual operations!)
  post
    "/i/index/reindex"
    (continue . const $ lift (wrapClient reindexAll) $> empty)
    true

  -- forcefully reindex from Cassandra, even if nothing has changed
  -- (e.g. integration testing -- prefer the `brig-index` executable
  -- for actual operations!)
  post
    "/i/index/reindex-if-same-or-newer"
    (continue . const $ lift (wrapClient reindexAllIfSameOrNewer) $> empty)
    true

-- Handlers

-- FUTUREWORK: Consider augmenting 'SearchResult' with full user profiles
-- for all results. This is tracked in https://wearezeta.atlassian.net/browse/SQCORE-599
search :: UserId -> Text -> Maybe Domain -> Maybe (Range 1 500 Int32) -> (Handler r) (Public.SearchResult Public.Contact)
search searcherId searchTerm maybeDomain maybeMaxResults = do
  federationDomain <- viewFederationDomain
  let queryDomain = fromMaybe federationDomain maybeDomain
  if queryDomain == federationDomain
    then searchLocally searcherId searchTerm maybeMaxResults
    else searchRemotely queryDomain searchTerm

searchRemotely :: Domain -> Text -> (Handler r) (Public.SearchResult Public.Contact)
searchRemotely domain searchTerm = do
  Log.info $
    msg (val "searchRemotely")
      ~~ field "domain" (show domain)
      ~~ field "searchTerm" searchTerm
  searchResponse <- Federation.searchUsers domain (FedBrig.SearchRequest searchTerm) !>> fedError
  let contacts = S.contacts searchResponse
  let count = length contacts
  pure
    SearchResult
      { searchResults = contacts,
        searchFound = count,
        searchReturned = count,
        searchTook = 0,
        searchPolicy = S.searchPolicy searchResponse
      }

searchLocally :: UserId -> Text -> Maybe (Range 1 500 Int32) -> (Handler r) (Public.SearchResult Public.Contact)
searchLocally searcherId searchTerm maybeMaxResults = do
  let maxResults = maybe 15 (fromIntegral . fromRange) maybeMaxResults
  teamSearchInfo <- mkTeamSearchInfo

  maybeExactHandleMatch <- exactHandleSearch teamSearchInfo

  let exactHandleMatchCount = length maybeExactHandleMatch
      esMaxResults = maxResults - exactHandleMatchCount

  esResult <-
    if esMaxResults > 0
      then Q.searchIndex (Just searcherId) (Just teamSearchInfo) searchTerm esMaxResults
      else pure $ SearchResult 0 0 0 [] FullSearch

  -- Prepend results matching exact handle and results from ES.
  pure $
    esResult
      { searchResults = maybeToList maybeExactHandleMatch <> searchResults esResult,
        searchFound = exactHandleMatchCount + searchFound esResult,
        searchReturned = exactHandleMatchCount + searchReturned esResult
      }
  where
    handleTeamVisibility :: TeamId -> TeamSearchVisibility -> Search.TeamSearchInfo
    handleTeamVisibility t Team.SearchVisibilityStandard = Search.TeamAndNonMembers t
    handleTeamVisibility t Team.SearchVisibilityNoNameOutsideTeam = Search.TeamOnly t

    mkTeamSearchInfo :: (Handler r) TeamSearchInfo
    mkTeamSearchInfo = lift $ do
      searcherTeamId <- wrapClient $ DB.lookupUserTeam searcherId
      sameTeamSearchOnly <- fromMaybe False <$> view (settings . Opts.searchSameTeamOnly)
      case searcherTeamId of
        Nothing -> return Search.NoTeam
        Just t ->
          -- This flag in brig overrules any flag on galley - it is system wide
          if sameTeamSearchOnly
            then return (Search.TeamOnly t)
            else do
              -- For team users, we need to check the visibility flag
              handleTeamVisibility t <$> Intra.getTeamSearchVisibility t

    exactHandleSearch :: TeamSearchInfo -> (Handler r) (Maybe Contact)
    exactHandleSearch teamSearchInfo = do
      lsearcherId <- qualifyLocal searcherId
      let searchedHandleMaybe = parseHandle searchTerm
      exactHandleResult <-
        case searchedHandleMaybe of
          Nothing -> pure Nothing
          Just searchedHandle ->
            contactFromProfile
              <$$> HandleAPI.getLocalHandleInfo lsearcherId searchedHandle
      pure $ case teamSearchInfo of
        Search.TeamOnly t ->
          if Just t == (contactTeam =<< exactHandleResult)
            then exactHandleResult
            else Nothing
        _ -> exactHandleResult

teamUserSearchH ::
  ( JSON
      ::: UserId
      ::: TeamId
      ::: Maybe Text
      ::: Maybe RoleFilter
      ::: Maybe TeamUserSearchSortBy
      ::: Maybe TeamUserSearchSortOrder
      ::: Range 1 500 Int32
  ) ->
  (Handler r) Response
teamUserSearchH (_ ::: uid ::: tid ::: mQuery ::: mRoleFilter ::: mSortBy ::: mSortOrder ::: size) = do
  json <$> teamUserSearch uid tid mQuery mRoleFilter mSortBy mSortOrder size

teamUserSearch ::
  UserId ->
  TeamId ->
  Maybe Text ->
  Maybe RoleFilter ->
  Maybe TeamUserSearchSortBy ->
  Maybe TeamUserSearchSortOrder ->
  Range 1 500 Int32 ->
  (Handler r) (Public.SearchResult Public.TeamContact)
teamUserSearch uid tid mQuery mRoleFilter mSortBy mSortOrder size = do
  ensurePermissions uid tid [Public.AddTeamMember] -- limit this to team admins to reduce risk of involuntary DOS attacks.  (also, this way we don't need to worry about revealing confidential user data to other team members.)
  Q.teamUserSearch tid mQuery mRoleFilter mSortBy mSortOrder size
