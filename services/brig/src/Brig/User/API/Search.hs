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
  ( routes,
  )
where

import Brig.API.Handler
import Brig.App
import qualified Brig.Data.User as DB
import qualified Brig.IO.Intra as Intra
import qualified Brig.Options as Opts
import Brig.Types.Search as Search
import qualified Brig.Types.Swagger as Doc
import Brig.User.Search.Index
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

routes :: Routes Doc.ApiBuilder Handler ()
routes = do
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
      Doc.description "Number of results to return"
      Doc.optional
    Doc.returns (Doc.ref Doc.searchResult)
    Doc.response 200 "The search result." Doc.end
  --

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

searchH :: JSON ::: UserId ::: Text ::: Range 1 100 Int32 -> Handler Response
searchH (_ ::: u ::: q ::: s) = json <$> lift (search u q s)

search :: UserId -> Text -> Range 1 100 Int32 -> AppIO (SearchResult Contact)
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
              -- For team users, we need to check the visibility flag
              else Intra.getTeamSearchVisibility t >>= handleTeamVisibility t . Team.searchVisibility
  searchIndex searcherId teamSearchInfo searchTerm maxResults
  where
    handleTeamVisibility t Team.SearchVisibilityStandard = return $ Search.TeamAndNonMembers t
    handleTeamVisibility _t Team.SearchVisibilityOutsideTeamOutboundOnly = error "handleHandleVisibility: Not implemented yet"

