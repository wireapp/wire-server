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

module Brig.User.API.Search
  ( routesPublic,
    routesInternal,
    API,
    servantSitemap,
  )
where

import Brig.API.Handler
import Brig.API.Util (ZAuthServant)
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
import Data.Handle (Handle (Handle))
import Data.Id
import Data.Predicate
import Data.Range
import qualified Data.Swagger.Build.Api as Doc
import qualified Galley.Types.Teams.SearchVisibility as Team
import Imports
import Network.Wai (Response)
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Routing
import Network.Wai.Utilities.Response (empty, json)
import Network.Wai.Utilities.Swagger (document)
import Servant hiding (Handler, JSON)
import qualified Servant
import System.Logger (field, msg)
import System.Logger.Class (val, (~~))
import qualified System.Logger.Class as Log
import qualified Wire.API.Team.Permission as Public
import Wire.API.Team.SearchVisibility (TeamSearchVisibility)
import qualified Wire.API.User.Search as Public

type SearchContacts =
  Summary "Search for users"
    :> ZAuthServant
    :> "search"
    :> "contacts"
    :> QueryParam' '[Required, Strict, Description "Search query"] "q" Text
    :> QueryParam' '[Optional, Strict, Description "Searched domain. Note: This is optional only for backwards compatibility, future versions will mandate this."] "domain" Domain
    :> QueryParam' '[Optional, Strict, Description "Number of results to return (min: 1, max: 500, default 15)"] "size" (Range 1 500 Int32)
    :> Get '[Servant.JSON] (Public.SearchResult Public.Contact)

type API = SearchContacts

servantSitemap :: ServerT API Handler
servantSitemap =
  search

routesPublic :: Routes Doc.ApiBuilder Handler ()
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

routesInternal :: Routes a Handler ()
routesInternal = do
  -- make index updates visible (e.g. for integration testing)
  post
    "/i/index/refresh"
    (continue (const $ lift refreshIndex *> pure empty))
    true

  -- reindex from Cassandra (e.g. integration testing -- prefer the
  -- `brig-index` executable for actual operations!)
  post
    "/i/index/reindex"
    (continue . const $ lift reindexAll *> pure empty)
    true

  -- forcefully reindex from Cassandra, even if nothing has changed
  -- (e.g. integration testing -- prefer the `brig-index` executable
  -- for actual operations!)
  post
    "/i/index/reindex-if-same-or-newer"
    (continue . const $ lift reindexAllIfSameOrNewer *> pure empty)
    true

-- Handlers

-- TODO question: would it make sense to create a second endpoint with a mandatory domain?
-- That would make it easier in the future to see from logs whether the old endpoint is still in use or can be removed.
--
-- FUTUREWORK: Consider augmenting 'SearchResult' with full user profiles either
-- for only the exact handle match or for all results or for the first X results.
-- See also https://wearezeta.atlassian.net/browse/SQCORE-599
search :: UserId -> Text -> Maybe Domain -> Maybe (Range 1 500 Int32) -> Handler (Public.SearchResult Public.Contact)
search searcherId searchTerm maybeDomain maybeMaxResults = do
  federationDomain <- viewFederationDomain
  let queryDomain = fromMaybe federationDomain maybeDomain
  if queryDomain == federationDomain
    then searchLocally searcherId searchTerm maybeMaxResults
    else searchRemotely queryDomain searchTerm

searchRemotely :: Domain -> Text -> Handler (Public.SearchResult Public.Contact)
searchRemotely domain searchTerm = do
  Log.info $
    msg (val "searchRemotely")
      ~~ field "domain" (show domain)
      ~~ field "searchTerm" searchTerm
  Federation.searchUsers domain searchTerm

searchLocally :: UserId -> Text -> Maybe (Range 1 500 Int32) -> Handler (Public.SearchResult Public.Contact)
searchLocally searcherId searchTerm maybeMaxResults = do
  let maxResults = maybe 15 (fromIntegral . fromRange) maybeMaxResults
  teamSearchInfo <- mkTeamSearchInfo

  maybeExactHandleMatch <- exactHandleSearch teamSearchInfo

  let exactHandleMatchCount = length maybeExactHandleMatch
      esMaxResults = maxResults - exactHandleMatchCount

  esResult <-
    if esMaxResults > 0
      then Q.searchIndex searcherId teamSearchInfo searchTerm esMaxResults
      else pure $ SearchResult 0 0 0 []

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

    mkTeamSearchInfo :: Handler TeamSearchInfo
    mkTeamSearchInfo = lift $ do
      searcherTeamId <- DB.lookupUserTeam searcherId
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

    exactHandleSearch :: TeamSearchInfo -> Handler (Maybe Contact)
    exactHandleSearch teamSearchInfo = do
      exactHandleResult <-
        contactFromProfile
          <$$> HandleAPI.getLocalHandleInfo searcherId (Handle searchTerm)
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
  Handler Response
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
  Handler (Public.SearchResult Public.TeamContact)
teamUserSearch uid tid mQuery mRoleFilter mSortBy mSortOrder size = do
  ensurePermissions uid tid [Public.AddTeamMember] -- limit this to team admins to reduce risk of involuntary DOS attacks.  (also, this way we don't need to worry about revealing confidential user data to other team members.)
  Q.teamUserSearch tid mQuery mRoleFilter mSortBy mSortOrder size
