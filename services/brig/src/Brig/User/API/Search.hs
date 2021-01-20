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
  )
where

import Brig.API.Handler
import Brig.App
import qualified Brig.Data.User as DB
import qualified Brig.IO.Intra as Intra
import qualified Brig.Options as Opts
import Brig.Team.Util (ensurePermissions)
import Brig.Types.Search as Search
import qualified Brig.User.Search.BrowseTeam as Q
import Brig.User.Search.Index
import qualified Brig.User.Search.SearchIndex as Q
import Control.Lens (view)
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
import qualified Wire.API.Team.Permission as Public
import qualified Wire.API.Team.Role as Public
import qualified Wire.API.User.Search as Public

routesPublic :: Routes Doc.ApiBuilder Handler ()
routesPublic = do
  get "/search/contacts" (continue searchH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. query "q"
      .&. def (unsafeRange 15) (query "size")

  document "GET" "search" $ do
    Doc.summary "Search for users"
    Doc.parameter Doc.Query "q" Doc.string' $
      Doc.description "Search query"
    Doc.parameter Doc.Query "size" Doc.int32' $ do
      Doc.description "Number of results to return (min: 1, max: 500, default: 15)"
      Doc.optional
    Doc.returns (Doc.ref $ Public.modelSearchResult Public.modelSearchContact)
    Doc.response 200 "The search result." Doc.end

  get "/teams/:tid/search" (continue browseTeamH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. capture "tid"
      .&. opt (query "q")
      .&. opt (query "frole")
      .&. opt (query "sortby")
      .&. opt (query "sortoder")
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

searchH :: JSON ::: UserId ::: Text ::: Range 1 500 Int32 -> Handler Response
searchH (_ ::: u ::: q ::: s) = json <$> lift (search u q s)

search :: UserId -> Text -> Range 1 500 Int32 -> AppIO (Public.SearchResult Public.Contact)
search searcherId searchTerm maxResults = do
  -- FUTUREWORK(federation, #1269):
  -- If the query contains a qualified handle, forward the search to the remote
  -- backend.
  searcherTeamId <- DB.lookupUserTeam searcherId
  sameTeamSearchOnly <- fromMaybe False <$> view (settings . Opts.searchSameTeamOnly)
  teamSearchInfo <-
    case searcherTeamId of
      Nothing -> return Search.NoTeam
      Just t ->
        -- This flag in brig overrules any flag on galley - it is system wide
        if sameTeamSearchOnly
          then return (Search.TeamOnly t)
          else do
            -- For team users, we need to check the visibility flag
            Intra.getTeamSearchVisibility t >>= return . handleTeamVisibility t
  Q.searchIndex searcherId teamSearchInfo searchTerm maxResults
  where
    handleTeamVisibility t Team.SearchVisibilityStandard = Search.TeamAndNonMembers t
    handleTeamVisibility t Team.SearchVisibilityNoNameOutsideTeam = Search.TeamOnly t

browseTeamH ::
  ( JSON
      ::: UserId
      ::: TeamId
      ::: Maybe Text
      ::: Maybe Text
      ::: Maybe Text
      ::: Maybe Text
      ::: Range 1 500 Int32
  ) ->
  Handler Response
browseTeamH (_ ::: uid ::: tid ::: mQuery ::: mRoleFilter ::: mSortBy ::: mSortOrder ::: size) = do
  json <$> browseTeam uid tid mQuery (undefined mRoleFilter) mSortBy mSortOrder size

browseTeam ::
  UserId ->
  TeamId ->
  Maybe Text ->
  Maybe [Public.Role] ->
  Maybe Text ->
  Maybe Text ->
  Range 1 500 Int32 ->
  Handler (Public.SearchResult Public.TeamContact)
browseTeam uid tid mQuery mRoleFilter mSortBy mSortOrder size = do
  ensurePermissions uid tid [Public.AddTeamMember] -- limit this to team admins to reduce risk of involuntary DOS attacks
  Q.browseTeam tid mQuery mRoleFilter mSortBy mSortOrder size
