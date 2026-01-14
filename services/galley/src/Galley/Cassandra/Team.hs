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

module Galley.Cassandra.Team
  ( interpretTeamMemberStoreToCassandra,
    interpretTeamListToCassandra,
    interpretInternalTeamListToCassandra,
    interpretTeamMemberStoreToCassandraWithPaging,
  )
where

import Cassandra
import Control.Exception (ErrorCall (ErrorCall))
import Control.Lens hiding ((<|))
import Control.Monad.Catch (throwM)
import Control.Monad.Extra (ifM)
import Data.Id
import Data.Json.Util (UTCTimeMillis (..))
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.Range
import Galley.Cassandra.Store
import Galley.Cassandra.Util
import Galley.Effects.TeamMemberStore
import Galley.Types.Teams (FeatureDefaults (..))
import Imports hiding (Set, max)
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.Team.Permission (Permissions)
import Wire.ListItems
import Wire.Sem.Paging.Cassandra
import Wire.TeamStore.Cassandra.Queries qualified as Cql

interpretTeamListToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  Sem (ListItems LegacyPaging TeamId ': r) a ->
  Sem r a
interpretTeamListToCassandra = interpret $ \case
  ListItems uid ps lim -> do
    logEffect "TeamList.ListItems"
    embedClient $ teamIdsFrom uid ps lim

interpretInternalTeamListToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  Sem (ListItems InternalPaging TeamId ': r) a ->
  Sem r a
interpretInternalTeamListToCassandra = interpret $ \case
  ListItems uid mps lim -> do
    logEffect "InternalTeamList.ListItems"
    embedClient $ case mps of
      Nothing -> do
        page <- teamIdsForPagination uid Nothing lim
        mkInternalPage page pure
      Just ps -> ipNext ps

interpretTeamMemberStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  FeatureDefaults LegalholdConfig ->
  Sem (TeamMemberStore InternalPaging ': r) a ->
  Sem r a
interpretTeamMemberStoreToCassandra lh = interpret $ \case
  ListTeamMembers tid mps lim -> do
    logEffect "TeamMemberStore.ListTeamMembers"
    embedClient $ case mps of
      Nothing -> do
        page <- teamMembersForPagination tid Nothing lim
        mkInternalPage page (newTeamMember' lh tid)
      Just ps -> ipNext ps

interpretTeamMemberStoreToCassandraWithPaging ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  FeatureDefaults LegalholdConfig ->
  Sem (TeamMemberStore CassandraPaging ': r) a ->
  Sem r a
interpretTeamMemberStoreToCassandraWithPaging lh = interpret $ \case
  ListTeamMembers tid mps lim -> do
    logEffect "TeamMemberStore.ListTeamMembers"
    embedClient $ teamMembersPageFrom lh tid mps lim

teamIdsFrom :: UserId -> Maybe TeamId -> Range 1 100 Int32 -> Client (ResultSet TeamId)
teamIdsFrom usr range (fromRange -> max) =
  mkResultSet . fmap runIdentity . strip <$> case range of
    Just c -> paginate Cql.selectUserTeamsFrom (paramsP LocalQuorum (usr, c) (max + 1))
    Nothing -> paginate Cql.selectUserTeams (paramsP LocalQuorum (Identity usr) (max + 1))
  where
    strip p = p {result = take (fromIntegral max) (result p)}

teamIdsForPagination :: UserId -> Maybe TeamId -> Range 1 100 Int32 -> Client (Page TeamId)
teamIdsForPagination usr range (fromRange -> max) =
  fmap runIdentity <$> case range of
    Just c -> paginate Cql.selectUserTeamsFrom (paramsP LocalQuorum (usr, c) max)
    Nothing -> paginate Cql.selectUserTeams (paramsP LocalQuorum (Identity usr) max)

-- | Construct 'TeamMember' from database tuple.
-- If FeatureLegalHoldWhitelistTeamsAndImplicitConsent is enabled set UserLegalHoldDisabled
-- if team is whitelisted.
--
-- Throw an exception if one of invitation timestamp and inviter is 'Nothing' and the
-- other is 'Just', which can only be caused by inconsistent database content.
newTeamMember' ::
  FeatureDefaults LegalholdConfig ->
  TeamId ->
  (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus) ->
  Client TeamMember
newTeamMember' lh tid (uid, perms, minvu, minvt, fromMaybe defUserLegalHoldStatus -> lhStatus) = do
  mk minvu minvt >>= maybeGrant
  where
    maybeGrant :: TeamMember -> Client TeamMember
    maybeGrant m =
      ifM
        (isTeamLegalholdWhitelisted lh tid)
        (pure (grantImplicitConsent m))
        (pure m)

    grantImplicitConsent :: TeamMember -> TeamMember
    grantImplicitConsent =
      legalHoldStatus %~ \case
        UserLegalHoldNoConsent -> UserLegalHoldDisabled
        -- the other cases don't change; we just enumerate them to catch future changes in
        -- 'UserLegalHoldStatus' better.
        UserLegalHoldDisabled -> UserLegalHoldDisabled
        UserLegalHoldPending -> UserLegalHoldPending
        UserLegalHoldEnabled -> UserLegalHoldEnabled

    mk (Just invu) (Just invt) = pure $ mkTeamMember uid perms (Just (invu, invt)) lhStatus
    mk Nothing Nothing = pure $ mkTeamMember uid perms Nothing lhStatus
    mk _ _ = throwM $ ErrorCall "TeamMember with incomplete metadata."

isTeamLegalholdWhitelisted :: FeatureDefaults LegalholdConfig -> TeamId -> Client Bool
isTeamLegalholdWhitelisted FeatureLegalHoldDisabledPermanently _ = pure False
isTeamLegalholdWhitelisted FeatureLegalHoldDisabledByDefault _ = pure False
isTeamLegalholdWhitelisted FeatureLegalHoldWhitelistTeamsAndImplicitConsent tid =
  isJust <$> (runIdentity <$$> retry x5 (query1 Cql.selectLegalHoldWhitelistedTeam (params LocalQuorum (Identity tid))))

type RawTeamMember = (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus)

-- This function has a bit of a difficult type to work with because we don't
-- have a pure function of type RawTeamMember -> TeamMember so we cannot fmap
-- over the ResultSet. We don't want to mess around with the Result size
-- nextPage either otherwise
teamMembersForPagination :: TeamId -> Maybe UserId -> Range 1 HardTruncationLimit Int32 -> Client (Page RawTeamMember)
teamMembersForPagination tid start (fromRange -> max) =
  case start of
    Just u -> paginate Cql.selectTeamMembersFrom (paramsP LocalQuorum (tid, u) max)
    Nothing -> paginate Cql.selectTeamMembers (paramsP LocalQuorum (Identity tid) max)

teamMembersPageFrom ::
  FeatureDefaults LegalholdConfig ->
  TeamId ->
  Maybe PagingState ->
  Range 1 HardTruncationLimit Int32 ->
  Client (PageWithState TeamMember)
teamMembersPageFrom lh tid pagingState (fromRange -> max) = do
  page <- paginateWithState Cql.selectTeamMembers (paramsPagingState LocalQuorum (Identity tid) max pagingState)
  members <- mapM (newTeamMember' lh tid) (pwsResults page)
  pure $ PageWithState members (pwsState page)
