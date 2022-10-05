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
  ( routesInternal,
    search,
    teamUserSearch,
  )
where

import Brig.API.Error (fedError)
import Brig.API.Handler
import Brig.App
import qualified Brig.Data.User as DB
import Brig.Effects.UserHandleStore
import Brig.Effects.UserQuery
import qualified Brig.Federation.Client as Federation
import qualified Brig.IO.Intra as Intra
import qualified Brig.Options as Opts
import Brig.Team.Util (ensurePermissions)
import Brig.Types.Search as Search
import qualified Brig.User.API.Handle as HandleAPI
import Brig.User.Search.Index
import qualified Brig.User.Search.SearchIndex as Q
import qualified Brig.User.Search.TeamUserSearch as Q
import Control.Lens (view)
import Data.Domain (Domain)
import Data.Handle (parseHandle)
import Data.Id
import Data.Predicate
import Data.Qualified
import Data.Range
import Imports
import Network.Wai.Routing
import Network.Wai.Utilities ((!>>))
import Network.Wai.Utilities.Response (empty)
import Polysemy
import Polysemy.Input
import System.Logger (field, msg)
import System.Logger.Class (val, (~~))
import qualified System.Logger.Class as Log
import qualified Wire.API.Federation.API.Brig as FedBrig
import qualified Wire.API.Federation.API.Brig as S
import qualified Wire.API.Team.Permission as Public
import Wire.API.Team.SearchVisibility (TeamSearchVisibility (..))
import Wire.API.User.Search
import qualified Wire.API.User.Search as Public

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
search ::
  Members
    '[ Input (Local ()),
       UserHandleStore,
       UserQuery p
     ]
    r =>
  UserId ->
  Text ->
  Maybe Domain ->
  Maybe (Range 1 500 Int32) ->
  Handler r (Public.SearchResult Public.Contact)
search searcherId searchTerm maybeDomain maybeMaxResults = do
  federationDomain <- viewFederationDomain
  let queryDomain = fromMaybe federationDomain maybeDomain
  if queryDomain == federationDomain
    then searchLocally searcherId searchTerm maybeMaxResults
    else searchRemotely queryDomain searchTerm

searchRemotely :: Domain -> Text -> (Handler r) (Public.SearchResult Public.Contact)
searchRemotely domain searchTerm = do
  lift . Log.info $
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

searchLocally ::
  forall r p.
  Members
    '[ Input (Local ()),
       UserHandleStore,
       UserQuery p
     ]
    r =>
  UserId ->
  Text ->
  Maybe (Range 1 500 Int32) ->
  Handler r (Public.SearchResult Public.Contact)
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
    handleTeamVisibility _ SearchVisibilityStandard = Search.AllUsers
    handleTeamVisibility t SearchVisibilityNoNameOutsideTeam = Search.TeamOnly t

    mkTeamSearchInfo :: Maybe TeamId -> (Handler r) TeamSearchInfo
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
              handleTeamVisibility t <$> wrapHttp (Intra.getTeamSearchVisibility t)

    exactHandleSearch :: Handler r (Maybe Contact)
    exactHandleSearch = do
      lsearcherId <- qualifyLocal searcherId
      case parseHandle searchTerm of
        Nothing -> pure Nothing
        Just handle -> do
          HandleAPI.contactFromProfile
            <$$> HandleAPI.getLocalHandleInfo lsearcherId handle

teamUserSearch ::
  UserId ->
  TeamId ->
  Maybe Text ->
  Maybe RoleFilter ->
  Maybe TeamUserSearchSortBy ->
  Maybe TeamUserSearchSortOrder ->
  Maybe (Range 1 500 Int32) ->
  (Handler r) (Public.SearchResult Public.TeamContact)
teamUserSearch uid tid mQuery mRoleFilter mSortBy mSortOrder size = do
  ensurePermissions uid tid [Public.AddTeamMember] -- limit this to team admins to reduce risk of involuntary DOS attacks.  (also, this way we don't need to worry about revealing confidential user data to other team members.)
  Q.teamUserSearch tid mQuery mRoleFilter mSortBy mSortOrder $ fromMaybe (unsafeRange 15) size
