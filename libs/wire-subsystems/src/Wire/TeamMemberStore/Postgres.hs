-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program.  If not, see <https://www.gnu.org/licenses/>.

-- | Paginated team member listing from the Postgres @team_member@ table.
--
-- Pagination is keyset-based on @user_id@: the paging state is the id of the
-- last member of the previous page.  The legal hold whitelist (implicit
-- consent) is still read from Cassandra, like in
-- "Wire.TeamMemberStore.Cassandra".
module Wire.TeamMemberStore.Postgres
  ( interpretTeamMemberStoreToPostgres,
    interpretTeamMemberStoreToPostgresWithPaging,
  )
where

import Cassandra (Client, ClientState, Page (..), PageWithState (..), PagingState (..), emptyPage)
import Cassandra.Exec (GeneralPaginationState (..))
import Control.Exception (ErrorCall (..))
import Control.Monad.Catch (throwM)
import Data.ByteString.Lazy qualified as LBS
import Data.Id
import Data.Range
import Data.UUID qualified as UUID
import Hasql.Pool.Extended qualified as HasqlPoolExt
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Team.Feature
import Wire.API.Team.FeatureFlags (FeatureDefaults (..))
import Wire.API.Team.Member
import Wire.Postgres
import Wire.Sem.Paging.Cassandra
import Wire.TeamMemberStore
import Wire.TeamMemberStore.Cassandra (maybeGrantImplicitConsent)
import Wire.TeamStore.Postgres (teamMemberKeysetPage)
import Wire.Util (embedClientInput, logEffect)

interpretTeamMemberStoreToPostgres ::
  ( PGConstraints r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  FeatureDefaults LegalholdConfig ->
  Sem (TeamMemberStore InternalPaging ': r) a ->
  Sem r a
interpretTeamMemberStoreToPostgres lh = interpret $ \case
  ListTeamMembers tid mps lim -> do
    logEffect "TeamMemberStore.ListTeamMembers"
    pool <- input
    embedClientInput $ case mps of
      Nothing -> do
        page <- teamMembersPage pool tid Nothing (fromRange lim)
        mkInternalPage page (maybeGrantImplicitConsent lh tid)
      Just ps -> ipNext ps

-- | A Cassandra-style 'Page' backed by keyset pagination: 'nextPage' fetches
-- the members following the last member of this page.
teamMembersPage :: HasqlPoolExt.Pool -> TeamId -> Maybe UserId -> Int32 -> Client (Page TeamMember)
teamMembersPage pool tid start size = do
  (members, next) <-
    liftIO (useWithResetAndRetry pool (teamMemberKeysetPage tid start size))
      >>= either (const (throwM (ErrorCall "team member page query failed"))) pure
  pure
    Page
      { hasMore = isJust next,
        result = members,
        nextPage = maybe (pure emptyPage) (\u -> teamMembersPage pool tid (Just u) size) next
      }

interpretTeamMemberStoreToPostgresWithPaging ::
  ( PGConstraints r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  FeatureDefaults LegalholdConfig ->
  Sem (TeamMemberStore CassandraPaging ': r) a ->
  Sem r a
interpretTeamMemberStoreToPostgresWithPaging lh = interpret $ \case
  ListTeamMembers tid mps lim -> do
    logEffect "TeamMemberStore.ListTeamMembers"
    start <- traverse decodePagingState mps
    (members, next) <- runSessionWithRetry (teamMemberKeysetPage tid start (fromRange lim))
    members' <- embedClientInput $ mapM (maybeGrantImplicitConsent lh tid) members
    pure $ PageWithState members' (PaginationStateCassandra . encodePagingState <$> next)

-- | The paging state is the ASCII representation of the last returned user
-- id.  It is opaque to (and provided by) API clients, so it is strictly
-- validated before use.
encodePagingState :: UserId -> PagingState
encodePagingState = PagingState . LBS.fromStrict . UUID.toASCIIBytes . toUUID

decodePagingState :: (Member (Embed IO) r) => PagingState -> Sem r UserId
decodePagingState (PagingState bs) =
  maybe
    (embed @IO (throwM (ErrorCall "invalid team member paging state")))
    (pure . Id)
    (UUID.fromASCIIBytes (LBS.toStrict bs))
