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
  ( interpretTeamStoreToCassandra,
    interpretTeamMemberStoreToCassandra,
    interpretTeamListToCassandra,
    interpretInternalTeamListToCassandra,
    interpretTeamMemberStoreToCassandraWithPaging,
  )
where

import Cassandra
import Cassandra.Util
import Control.Exception (ErrorCall (ErrorCall))
import Control.Lens hiding ((<|))
import Control.Monad.Catch (throwM)
import Control.Monad.Extra (ifM)
import Data.ByteString.Conversion (toByteString')
import Data.Id as Id
import Data.Json.Util (UTCTimeMillis (..))
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.Map.Strict qualified as Map
import Data.Range
import Data.Set qualified as Set
import Data.Text.Encoding
import Data.UUID.V4 (nextRandom)
import Galley.Aws qualified as Aws
import Galley.Cassandra.Conversation qualified as C
import Galley.Cassandra.LegalHold (isTeamLegalholdWhitelisted)
import Galley.Cassandra.Queries qualified as Cql
import Galley.Cassandra.Store
import Galley.Cassandra.Util
import Galley.Effects.ListItems
import Galley.Effects.TeamMemberStore
import Galley.Effects.TeamStore (TeamStore (..))
import Galley.Env
import Galley.Monad
import Galley.Options
import Galley.Types.Teams
import Imports hiding (Set, max)
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import UnliftIO qualified
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.Team.Permission (Perm (SetBilling), Permissions, self)
import Wire.Sem.Paging.Cassandra

interpretTeamStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input Env) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  FeatureDefaults LegalholdConfig ->
  Sem (TeamStore ': r) a ->
  Sem r a
interpretTeamStoreToCassandra lh = interpret $ \case
  CreateTeamMember tid mem -> do
    logEffect "TeamStore.CreateTeamMember"
    embedClient (addTeamMember tid mem)
  SetTeamMemberPermissions perm0 tid uid perm1 -> do
    logEffect "TeamStore.SetTeamMemberPermissions"
    embedClient (updateTeamMember perm0 tid uid perm1)
  CreateTeam t uid n i k b -> do
    logEffect "TeamStore.CreateTeam"
    embedClient (createTeam t uid n i k b)
  DeleteTeamMember tid uid -> do
    logEffect "TeamStore.DeleteTeamMember"
    embedClient (removeTeamMember tid uid)
  GetBillingTeamMembers tid -> do
    logEffect "TeamStore.GetBillingTeamMembers"
    embedClient (listBillingTeamMembers tid)
  GetTeamAdmins tid -> do
    logEffect "TeamStore.GetTeamAdmins"
    embedClient (listTeamAdmins tid)
  GetTeam tid -> do
    logEffect "TeamStore.GetTeam"
    embedClient (team tid)
  GetTeamName tid -> do
    logEffect "TeamStore.GetTeamName"
    embedClient (getTeamName tid)
  GetTeamConversation tid cid -> do
    logEffect "TeamStore.GetTeamConversation"
    embedClient (teamConversation tid cid)
  GetTeamConversations tid -> do
    logEffect "TeamStore.GetTeamConversations"
    embedClient (getTeamConversations tid)
  SelectTeams uid tids -> do
    logEffect "TeamStore.SelectTeams"
    embedClient (teamIdsOf uid tids)
  GetTeamMember tid uid -> do
    logEffect "TeamStore.GetTeamMember"
    embedClient (teamMember lh tid uid)
  GetTeamMembersWithLimit tid n -> do
    logEffect "TeamStore.GetTeamMembersWithLimit"
    embedClient (teamMembersWithLimit lh tid n)
  GetTeamMembers tid -> do
    logEffect "TeamStore.GetTeamMembers"
    embedClient (teamMembersCollectedWithPagination lh tid)
  SelectTeamMembers tid uids -> do
    logEffect "TeamStore.SelectTeamMembers"
    embedClient (teamMembersLimited lh tid uids)
  GetUserTeams uid -> do
    logEffect "TeamStore.GetUserTeams"
    embedClient (userTeams uid)
  GetUsersTeams uids -> do
    logEffect "TeamStore.GetUsersTeams"
    embedClient (usersTeams uids)
  GetOneUserTeam uid -> do
    logEffect "TeamStore.GetOneUserTeam"
    embedClient (oneUserTeam uid)
  GetTeamsBindings tid -> do
    logEffect "TeamStore.GetTeamsBindings"
    embedClient (getTeamsBindings tid)
  GetTeamBinding tid -> do
    logEffect "TeamStore.GetTeamBinding"
    embedClient (getTeamBinding tid)
  GetTeamCreationTime tid -> do
    logEffect "TeamStore.GetTeamCreationTime"
    embedClient (teamCreationTime tid)
  DeleteTeam tid -> do
    logEffect "TeamStore.DeleteTeam"
    embedClient (deleteTeam tid)
  DeleteTeamConversation tid cid -> do
    logEffect "TeamStore.DeleteTeamConversation"
    embedClient (removeTeamConv tid cid)
  SetTeamData tid upd -> do
    logEffect "TeamStore.SetTeamData"
    embedClient (updateTeam tid upd)
  SetTeamStatus tid st -> do
    logEffect "TeamStore.SetTeamStatus"
    embedClient (updateTeamStatus tid st)
  FanoutLimit -> do
    logEffect "TeamStore.FanoutLimit"
    embedApp (currentFanoutLimit <$> view options)
  GetLegalHoldFlag -> do
    logEffect "TeamStore.GetLegalHoldFlag"
    view (options . settings . featureFlags . to npProject) <$> input
  EnqueueTeamEvent e -> do
    logEffect "TeamStore.EnqueueTeamEvent"
    menv <- inputs (view aEnv)
    for_ menv $ \env ->
      embed @IO (Aws.execute env (Aws.enqueue e))
  SelectTeamMembersPaginated tid uids mps lim -> do
    logEffect "TeamStore.SelectTeamMembersPaginated"
    embedClient (selectSomeTeamMembersPaginated lh tid uids mps lim)

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

createTeam ::
  Maybe TeamId ->
  UserId ->
  Range 1 256 Text ->
  Icon ->
  Maybe (Range 1 256 Text) ->
  TeamBinding ->
  Client Team
createTeam t uid (fromRange -> n) i k b = do
  tid <- maybe (Id <$> liftIO nextRandom) pure t
  retry x5 $ write Cql.insertTeam (params LocalQuorum (tid, uid, n, i, fromRange <$> k, initialStatus b, b))
  pure (newTeam tid uid n i b & teamIconKey .~ (fromRange <$> k))
  where
    initialStatus Binding = PendingActive -- Team becomes Active after User account activation
    initialStatus NonBinding = Active

listBillingTeamMembers :: TeamId -> Client [UserId]
listBillingTeamMembers tid =
  fmap runIdentity
    <$> retry x1 (query Cql.listBillingTeamMembers (params LocalQuorum (Identity tid)))

listTeamAdmins :: TeamId -> Client [UserId]
listTeamAdmins tid =
  fmap runIdentity
    <$> retry x1 (query Cql.listTeamAdmins (params LocalQuorum (Identity tid)))

getTeamName :: TeamId -> Client (Maybe Text)
getTeamName tid =
  fmap runIdentity
    <$> retry x1 (query1 Cql.selectTeamName (params LocalQuorum (Identity tid)))

teamConversation :: TeamId -> ConvId -> Client (Maybe TeamConversation)
teamConversation t c =
  fmap (newTeamConversation . runIdentity)
    <$> retry x1 (query1 Cql.selectTeamConv (params LocalQuorum (t, c)))

getTeamConversations :: TeamId -> Client [TeamConversation]
getTeamConversations t =
  map (newTeamConversation . runIdentity)
    <$> retry x1 (query Cql.selectTeamConvs (params LocalQuorum (Identity t)))

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

teamMember :: FeatureDefaults LegalholdConfig -> TeamId -> UserId -> Client (Maybe TeamMember)
teamMember lh t u =
  newTeamMember'' u =<< retry x1 (query1 Cql.selectTeamMember (params LocalQuorum (t, u)))
  where
    newTeamMember'' ::
      UserId ->
      Maybe (Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus) ->
      Client (Maybe TeamMember)
    newTeamMember'' _ Nothing = pure Nothing
    newTeamMember'' uid (Just (perms, minvu, minvt, mulhStatus)) =
      Just <$> newTeamMember' lh t (uid, perms, minvu, minvt, mulhStatus)

addTeamMember :: TeamId -> TeamMember -> Client ()
addTeamMember t m =
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery
      Cql.insertTeamMember
      ( t,
        m ^. userId,
        m ^. permissions,
        m ^? invitation . _Just . _1,
        m ^? invitation . _Just . _2
      )
    addPrepQuery Cql.insertUserTeam (m ^. userId, t)

    when (m `hasPermission` SetBilling) $
      addPrepQuery Cql.insertBillingTeamMember (t, m ^. userId)

    when (isAdminOrOwner (m ^. permissions)) $
      addPrepQuery Cql.insertTeamAdmin (t, m ^. userId)

updateTeamMember ::
  -- | Old permissions, used for maintaining 'billing_team_member' and 'team_admin' tables
  Permissions ->
  TeamId ->
  UserId ->
  -- | New permissions
  Permissions ->
  Client ()
updateTeamMember oldPerms tid uid newPerms = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery Cql.updatePermissions (newPerms, tid, uid)

    -- update billing_team_member table
    let permDiff = Set.difference `on` view self
        acquiredPerms = newPerms `permDiff` oldPerms
        lostPerms = oldPerms `permDiff` newPerms

    when (SetBilling `Set.member` acquiredPerms) $
      addPrepQuery Cql.insertBillingTeamMember (tid, uid)
    when (SetBilling `Set.member` lostPerms) $
      addPrepQuery Cql.deleteBillingTeamMember (tid, uid)

    -- update team_admin table
    let wasAdmin = isAdminOrOwner oldPerms
        isAdmin = isAdminOrOwner newPerms

    when (isAdmin && not wasAdmin) $
      addPrepQuery Cql.insertTeamAdmin (tid, uid)

    when (not isAdmin && wasAdmin) $
      addPrepQuery Cql.deleteTeamAdmin (tid, uid)

removeTeamMember :: TeamId -> UserId -> Client ()
removeTeamMember t m =
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery Cql.deleteTeamMember (t, m)
    addPrepQuery Cql.deleteUserTeam (m, t)
    addPrepQuery Cql.deleteBillingTeamMember (t, m)
    addPrepQuery Cql.deleteTeamAdmin (t, m)

team :: TeamId -> Client (Maybe TeamData)
team tid =
  fmap toTeam <$> retry x1 (query1 Cql.selectTeam (params LocalQuorum (Identity tid)))
  where
    toTeam (u, n, i, k, d, s, st, b, ss) =
      let t = newTeam tid u n i (fromMaybe NonBinding b) & teamIconKey .~ k & teamSplashScreen .~ fromMaybe DefaultIcon ss
          status = if d then PendingDelete else fromMaybe Active s
       in TeamData t status (writetimeToUTC <$> st)

teamIdsOf :: UserId -> [TeamId] -> Client [TeamId]
teamIdsOf usr tids =
  map runIdentity <$> retry x1 (query Cql.selectUserTeamsIn (params LocalQuorum (usr, toList tids)))

teamMembersWithLimit ::
  FeatureDefaults LegalholdConfig ->
  TeamId ->
  Range 1 HardTruncationLimit Int32 ->
  Client TeamMemberList
teamMembersWithLimit lh t (fromRange -> limit) = do
  -- NOTE: We use +1 as size and then trim it due to the semantics of C* when getting a page with the exact same size
  pageTuple <- retry x1 (paginate Cql.selectTeamMembers (paramsP LocalQuorum (Identity t) (limit + 1)))
  ms <- mapM (newTeamMember' lh t) . take (fromIntegral limit) $ result pageTuple
  pure $
    if hasMore pageTuple
      then newTeamMemberList ms ListTruncated
      else newTeamMemberList ms ListComplete

-- NOTE: Use this function with care... should only be required when deleting a team!
--       Maybe should be left explicitly for the caller?
teamMembersCollectedWithPagination :: FeatureDefaults LegalholdConfig -> TeamId -> Client [TeamMember]
teamMembersCollectedWithPagination lh tid = do
  mems <- teamMembersForPagination tid Nothing (unsafeRange 2000)
  collectTeamMembersPaginated [] mems
  where
    collectTeamMembersPaginated acc mems = do
      tMembers <- mapM (newTeamMember' lh tid) (result mems)
      if hasMore mems
        then collectTeamMembersPaginated (tMembers ++ acc) =<< nextPage mems
        else pure (tMembers ++ acc)

-- Lookup only specific team members: this is particularly useful for large teams when
-- needed to look up only a small subset of members (typically 2, user to perform the action
-- and the target user)
teamMembersLimited :: FeatureDefaults LegalholdConfig -> TeamId -> [UserId] -> Client [TeamMember]
teamMembersLimited lh t u =
  mapM (newTeamMember' lh t)
    =<< retry x1 (query Cql.selectTeamMembers' (params LocalQuorum (t, u)))

userTeams :: UserId -> Client [TeamId]
userTeams u =
  map runIdentity
    <$> retry x1 (query Cql.selectUserTeams (params LocalQuorum (Identity u)))

usersTeams :: [UserId] -> Client (Map UserId TeamId)
usersTeams uids = do
  pairs :: [(UserId, TeamId)] <-
    catMaybes
      <$> UnliftIO.pooledMapConcurrentlyN 8 (\uid -> (uid,) <$$> oneUserTeam uid) uids
  pure $ foldl' (\m (k, v) -> Map.insert k v m) Map.empty pairs

oneUserTeam :: UserId -> Client (Maybe TeamId)
oneUserTeam u =
  fmap runIdentity
    <$> retry x1 (query1 Cql.selectOneUserTeam (params LocalQuorum (Identity u)))

teamCreationTime :: TeamId -> Client (Maybe TeamCreationTime)
teamCreationTime t =
  checkCreation . fmap runIdentity
    <$> retry x1 (query1 Cql.selectTeamBindingWritetime (params LocalQuorum (Identity t)))
  where
    checkCreation (Just (Just ts)) = Just $ TeamCreationTime ts
    checkCreation _ = Nothing

getTeamBinding :: TeamId -> Client (Maybe TeamBinding)
getTeamBinding t =
  fmap (fromMaybe NonBinding . runIdentity)
    <$> retry x1 (query1 Cql.selectTeamBinding (params LocalQuorum (Identity t)))

getTeamsBindings :: [TeamId] -> Client [TeamBinding]
getTeamsBindings =
  fmap catMaybes
    . UnliftIO.pooledMapConcurrentlyN 8 getTeamBinding

deleteTeam :: TeamId -> Client ()
deleteTeam tid = do
  -- TODO: delete service_whitelist records that mention this team
  retry x5 $ write Cql.markTeamDeleted (params LocalQuorum (PendingDelete, tid))
  mems <- teamMembersForPagination tid Nothing (unsafeRange 2000)
  removeTeamMembers mems
  cnvs <- teamConversationsForPagination tid Nothing (unsafeRange 2000)
  removeConvs cnvs
  retry x5 $ write Cql.deleteTeam (params LocalQuorum (Deleted, tid))
  where
    removeConvs :: Page TeamConversation -> Client ()
    removeConvs cnvs = do
      for_ (result cnvs) $ removeTeamConv tid . view conversationId
      unless (null $ result cnvs) $
        removeConvs =<< liftClient (nextPage cnvs)

    removeTeamMembers ::
      Page
        ( UserId,
          Permissions,
          Maybe UserId,
          Maybe UTCTimeMillis,
          Maybe UserLegalHoldStatus
        ) ->
      Client ()
    removeTeamMembers mems = do
      mapM_ (removeTeamMember tid . view _1) (result mems)
      unless (null $ result mems) $
        removeTeamMembers =<< liftClient (nextPage mems)

removeTeamConv :: TeamId -> ConvId -> Client ()
removeTeamConv tid cid = liftClient $ do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery Cql.markConvDeleted (Identity cid)
    addPrepQuery Cql.deleteTeamConv (tid, cid)
  C.deleteConversation cid

updateTeamStatus :: TeamId -> TeamStatus -> Client ()
updateTeamStatus t s = retry x5 $ write Cql.updateTeamStatus (params LocalQuorum (s, t))

updateTeam :: TeamId -> TeamUpdateData -> Client ()
updateTeam tid u = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  for_ (u ^. nameUpdate) $ \n ->
    addPrepQuery Cql.updateTeamName (fromRange n, tid)
  for_ (u ^. iconUpdate) $ \i ->
    addPrepQuery Cql.updateTeamIcon (decodeUtf8 . toByteString' $ i, tid)
  for_ (u ^. iconKeyUpdate) $ \k ->
    addPrepQuery Cql.updateTeamIconKey (fromRange k, tid)
  for_ (u ^. splashScreenUpdate) $ \ss ->
    addPrepQuery Cql.updateTeamSplashScreen (decodeUtf8 . toByteString' $ ss, tid)

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

teamConversationsForPagination ::
  TeamId ->
  Maybe ConvId ->
  Range 1 HardTruncationLimit Int32 ->
  Client (Page TeamConversation)
teamConversationsForPagination tid start (fromRange -> max) =
  fmap (newTeamConversation . runIdentity) <$> case start of
    Just c -> paginate Cql.selectTeamConvsFrom (paramsP LocalQuorum (tid, c) max)
    Nothing -> paginate Cql.selectTeamConvs (paramsP LocalQuorum (Identity tid) max)

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

selectSomeTeamMembersPaginated ::
  FeatureDefaults LegalholdConfig ->
  TeamId ->
  [UserId] ->
  Maybe PagingState ->
  Range 1 HardTruncationLimit Int32 ->
  Client (PageWithState TeamMember)
selectSomeTeamMembersPaginated lh tid uids pagingState (fromRange -> max) = do
  page <- paginateWithState Cql.selectTeamMembers' (paramsPagingState LocalQuorum (tid, uids) max pagingState)
  members <- mapM (newTeamMember' lh tid) (pwsResults page)
  pure $ PageWithState members (pwsState page)
