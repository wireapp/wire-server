-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.TeamStore.Cassandra
  ( interpretTeamStoreToCassandra,
  )
where

import Cassandra
import Cassandra.Util
import Control.Lens hiding ((<|))
import Control.Monad.Catch ()
import Control.Monad.Extra (ifM)
import Data.ByteString.Conversion (toByteString')
import Data.Id as Id
import Data.Json.Util (UTCTimeMillis (..), toUTCTimeMillis)
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.Map.Strict qualified as Map
import Data.Range
import Data.Set qualified as Set
import Data.Text.Encoding
import Data.UUID.V4 (nextRandom)
import Imports hiding (Set, max)
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import UnliftIO qualified
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Team
import Wire.API.Team.Member
import Wire.API.Team.Member.Info (TeamMemberInfo (TeamMemberInfo))
import Wire.API.Team.Member.Info qualified as Info
import Wire.API.Team.Permission (Perm (SetBilling), Permissions, self)
import Wire.ConversationStore (ConversationStore)
import Wire.ConversationStore qualified as E
import Wire.ConversationStore.Cassandra.Instances ()
import Wire.LegalHoldStore (LegalHoldStore)
import Wire.LegalHoldStore qualified as LH
import Wire.TeamEventQueueAccess qualified as TEQ
import Wire.TeamStore (TeamStore (..))
import Wire.TeamStore.Cassandra.Queries qualified as Cql
import Wire.TeamStore.Env (TeamStoreEnv (..))
import Wire.Util (embedClientInput, logEffect)

interpretTeamStoreToCassandra ::
  ( Member (Embed IO) r,
    Member (Input TeamStoreEnv) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member ConversationStore r,
    Member LegalHoldStore r,
    Member TEQ.TeamEventQueueAccess r
  ) =>
  Sem (TeamStore ': r) a ->
  Sem r a
interpretTeamStoreToCassandra = interpret $ \case
  CreateTeamMember tid mem -> do
    logEffect "TeamStore.CreateTeamMember"
    embedClientInput (addTeamMember tid mem)
  SetTeamMemberPermissions perm0 tid uid perm1 -> do
    logEffect "TeamStore.SetTeamMemberPermissions"
    embedClientInput (updateTeamMember perm0 tid uid perm1)
  CreateTeam t uid n i k b -> do
    logEffect "TeamStore.CreateTeam"
    createTeam t uid n i k b
  DeleteTeamMember tid uid -> do
    logEffect "TeamStore.DeleteTeamMember"
    embedClientInput (removeTeamMember tid uid)
  GetBillingTeamMembers tid -> do
    logEffect "TeamStore.GetBillingTeamMembers"
    embedClientInput (listBillingTeamMembers tid)
  GetTeamAdmins tid -> do
    logEffect "TeamStore.GetTeamAdmins"
    embedClientInput (listTeamAdmins tid)
  GetTeam tid -> do
    logEffect "TeamStore.GetTeam"
    embedClientInput (team tid)
  GetTeamName tid -> do
    logEffect "TeamStore.GetTeamName"
    embedClientInput (getTeamName tid)
  SelectTeams uid tids -> do
    logEffect "TeamStore.SelectTeams"
    embedClientInput (teamIdsOf uid tids)
  GetTeamMember tid uid -> do
    logEffect "TeamStore.GetTeamMember"
    teamMember tid uid
  GetTeamMembersWithLimit tid n -> do
    logEffect "TeamStore.GetTeamMembersWithLimit"
    teamMembersWithLimit tid n
  GetTeamMembers tid -> do
    logEffect "TeamStore.GetTeamMembers"
    teamMembersCollectedWithPagination tid
  SelectTeamMembers tid uids -> do
    logEffect "TeamStore.SelectTeamMembers"
    teamMembersLimited tid uids
  SelectTeamMemberInfos tid uids -> do
    logEffect "TeamStore.SelectTeamMemberInfos"
    embedClientInput (teamMemberInfos tid uids)
  GetUserTeams uid -> do
    logEffect "TeamStore.GetUserTeams"
    embedClientInput (userTeams uid)
  GetUsersTeams uids -> do
    logEffect "TeamStore.GetUsersTeams"
    embedClientInput (usersTeams uids)
  GetOneUserTeam uid -> do
    logEffect "TeamStore.GetOneUserTeam"
    embedClientInput (oneUserTeam uid)
  GetTeamsBindings tid -> do
    logEffect "TeamStore.GetTeamsBindings"
    embedClientInput (getTeamsBindings tid)
  GetTeamBinding tid -> do
    logEffect "TeamStore.GetTeamBinding"
    embedClientInput (getTeamBinding tid)
  GetTeamCreationTime tid -> do
    logEffect "TeamStore.GetTeamCreationTime"
    embedClientInput (teamCreationTime tid)
  DeleteTeam tid -> do
    logEffect "TeamStore.DeleteTeam"
    deleteTeam tid
  SetTeamData tid upd -> do
    logEffect "TeamStore.SetTeamData"
    embedClientInput (updateTeam tid upd)
  SetTeamStatus tid st -> do
    logEffect "TeamStore.SetTeamStatus"
    embedClientInput (updateTeamStatus tid st)
  -- TODO(leif): remove and use input directly
  GetLegalHoldFlag -> do
    logEffect "TeamStore.GetLegalHoldFlag"
    legalholdDefaults <$> input
  -- TODO(leif): remove and use the TEQ directly
  EnqueueTeamEvent e -> do
    logEffect "TeamStore.EnqueueTeamEvent"
    TEQ.enqueueTeamEvent e
  SelectTeamMembersPaginated tid uids mps lim -> do
    logEffect "TeamStore.SelectTeamMembersPaginated"
    selectSomeTeamMembersPaginated tid uids mps lim

createTeam ::
  ( Member (Input ClientState) r,
    Member (Embed IO) r
  ) =>
  Maybe TeamId ->
  UserId ->
  Range 1 256 Text ->
  Icon ->
  Maybe (Range 1 256 Text) ->
  TeamBinding ->
  Sem r Team
createTeam t uid (fromRange -> n) i k b = do
  tid <- embed @IO $ maybe (Id <$> liftIO nextRandom) pure t
  embedClientInput $ retry x5 $ write Cql.insertTeam (params LocalQuorum (tid, uid, n, i, fromRange <$> k, initialStatus b, b))
  pure (newTeam tid uid n i b & teamIconKey .~ (fromRange <$> k))
  where
    initialStatus Binding = PendingActive
    initialStatus NonBinding = Active

listBillingTeamMembers :: TeamId -> Client [UserId]
listBillingTeamMembers tid = fmap runIdentity <$> retry x1 (query Cql.listBillingTeamMembers (params LocalQuorum (Identity tid)))

listTeamAdmins :: TeamId -> Client [UserId]
listTeamAdmins tid = fmap runIdentity <$> retry x1 (query Cql.listTeamAdmins (params LocalQuorum (Identity tid)))

getTeamName :: TeamId -> Client (Maybe Text)
getTeamName tid = fmap runIdentity <$> retry x1 (query1 Cql.selectTeamName (params LocalQuorum (Identity tid)))

teamIdsOf :: UserId -> [TeamId] -> Client [TeamId]
teamIdsOf uid tids = fmap runIdentity <$> retry x1 (query Cql.selectUserTeamsIn (params LocalQuorum (uid, tids)))

team :: TeamId -> Client (Maybe TeamData)
team tid = fmap toTeam <$> retry x1 (query1 Cql.selectTeam (params LocalQuorum (Identity tid)))
  where
    toTeam (u, n, i, k, d, s, st, b, ss) =
      let t = newTeam tid u n i (fromMaybe NonBinding b) & teamIconKey .~ k & teamSplashScreen .~ fromMaybe DefaultIcon ss
          status = if d then PendingDelete else fromMaybe Active s
       in TeamData t status (writetimeToUTC <$> st)

teamMember ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member LegalHoldStore r
  ) =>
  TeamId ->
  UserId ->
  Sem r (Maybe TeamMember)
teamMember t u = do
  mres <- embedClientInput $ retry x1 (query1 Cql.selectTeamMember (params LocalQuorum (t, u)))
  case mres of
    Nothing -> pure Nothing
    Just (perms, minvu, minvt, mulhStatus) -> Just <$> newTeamMember' t (u, perms, minvu, minvt, mulhStatus)

addTeamMember :: TeamId -> TeamMember -> Client ()
addTeamMember t m =
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery Cql.insertTeamMember (t, m ^. userId, m ^. permissions, m ^? invitation . _Just . _1, m ^? invitation . _Just . _2)
    addPrepQuery Cql.insertUserTeam (m ^. userId, t)
    when (m `hasPermission` SetBilling) $ addPrepQuery Cql.insertBillingTeamMember (t, m ^. userId)
    when (isAdminOrOwner (m ^. permissions)) $ addPrepQuery Cql.insertTeamAdmin (t, m ^. userId)

updateTeamMember :: Permissions -> TeamId -> UserId -> Permissions -> Client ()
updateTeamMember oldPerms tid uid newPerms = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery Cql.updatePermissions (newPerms, tid, uid)
    let permDiff = Set.difference `on` self
        acquiredPerms = newPerms `permDiff` oldPerms
        lostPerms = oldPerms `permDiff` newPerms
    when (SetBilling `Set.member` acquiredPerms) $ addPrepQuery Cql.insertBillingTeamMember (tid, uid)
    when (SetBilling `Set.member` lostPerms) $ addPrepQuery Cql.deleteBillingTeamMember (tid, uid)
    when (isAdminOrOwner newPerms && not (isAdminOrOwner oldPerms)) $ addPrepQuery Cql.insertTeamAdmin (tid, uid)
    when (isAdminOrOwner oldPerms && not (isAdminOrOwner newPerms)) $ addPrepQuery Cql.deleteTeamAdmin (tid, uid)

removeTeamMember :: TeamId -> UserId -> Client ()
removeTeamMember tid uid = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery Cql.deleteTeamMember (tid, uid)
    addPrepQuery Cql.deleteUserTeam (uid, tid)
    addPrepQuery Cql.deleteBillingTeamMember (tid, uid)
    addPrepQuery Cql.deleteTeamAdmin (tid, uid)

userTeams :: UserId -> Client [TeamId]
userTeams u = map runIdentity <$> retry x1 (query Cql.selectUserTeams (params LocalQuorum (Identity u)))

usersTeams :: [UserId] -> Client (Map UserId TeamId)
usersTeams uids = do
  pairs :: [(UserId, TeamId)] <- catMaybes <$> UnliftIO.pooledMapConcurrentlyN 8 (\uid -> (uid,) <$$> oneUserTeam uid) uids
  pure $ foldl' (\m (k, v) -> Map.insert k v m) Map.empty pairs

oneUserTeam :: UserId -> Client (Maybe TeamId)
oneUserTeam u = fmap runIdentity <$> retry x1 (query1 Cql.selectOneUserTeam (params LocalQuorum (Identity u)))

teamCreationTime :: TeamId -> Client (Maybe TeamCreationTime)
teamCreationTime t = checkCreation . fmap runIdentity <$> retry x1 (query1 Cql.selectTeamBindingWritetime (params LocalQuorum (Identity t)))
  where
    checkCreation (Just (Just ts)) = Just $ TeamCreationTime ts
    checkCreation _ = Nothing

getTeamBinding :: TeamId -> Client (Maybe TeamBinding)
getTeamBinding t = fmap (fromMaybe NonBinding . runIdentity) <$> retry x1 (query1 Cql.selectTeamBinding (params LocalQuorum (Identity t)))

getTeamsBindings :: [TeamId] -> Client [TeamBinding]
getTeamsBindings = fmap catMaybes . UnliftIO.pooledMapConcurrentlyN 8 getTeamBinding

deleteTeam ::
  ( Member (Input ClientState) r,
    Member (Embed IO) r,
    Member ConversationStore r
  ) =>
  TeamId ->
  Sem r ()
deleteTeam tid = do
  embedClientInput (markTeamDeletedAndRemoveTeamMembers tid)
  E.deleteTeamConversations tid
  embedClientInput (retry x5 $ write Cql.deleteTeam (params LocalQuorum (Deleted, tid)))

markTeamDeletedAndRemoveTeamMembers :: TeamId -> Client ()
markTeamDeletedAndRemoveTeamMembers tid = do
  retry x5 $ write Cql.markTeamDeleted (params LocalQuorum (PendingDelete, tid))
  mems <- teamMembersForPagination tid Nothing (unsafeRange 2000)
  removeTeamMembers mems
  where
    removeTeamMembers mems = do
      mapM_ (removeTeamMember tid . view _1) (result mems)
      unless (null $ result mems) $ removeTeamMembers =<< liftClient (nextPage mems)

updateTeamStatus :: TeamId -> TeamStatus -> Client ()
updateTeamStatus t s = retry x5 $ write Cql.updateTeamStatus (params LocalQuorum (s, t))

updateTeam :: TeamId -> TeamUpdateData -> Client ()
updateTeam tid u = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  for_ (u ^. nameUpdate) $ \n -> addPrepQuery Cql.updateTeamName (fromRange n, tid)
  for_ (u ^. iconUpdate) $ \i -> addPrepQuery Cql.updateTeamIcon (decodeUtf8 . toByteString' $ i, tid)
  for_ (u ^. iconKeyUpdate) $ \k -> addPrepQuery Cql.updateTeamIconKey (fromRange k, tid)
  for_ (u ^. splashScreenUpdate) $ \ss -> addPrepQuery Cql.updateTeamSplashScreen (decodeUtf8 . toByteString' $ ss, tid)

newTeamMember' ::
  (Member LegalHoldStore r) =>
  TeamId ->
  (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus) ->
  Sem r TeamMember
newTeamMember' tid (uid, perms, minvu, minvt, fromMaybe defUserLegalHoldStatus -> lhStatus) = do
  mk minvu minvt >>= maybeGrant
  where
    maybeGrant m =
      ifM (LH.isTeamLegalholdWhitelisted tid) (pure (grantImplicitConsent m)) (pure m)
    grantImplicitConsent =
      legalHoldStatus %~ \case
        UserLegalHoldNoConsent -> UserLegalHoldDisabled
        UserLegalHoldDisabled -> UserLegalHoldDisabled
        UserLegalHoldPending -> UserLegalHoldPending
        UserLegalHoldEnabled -> UserLegalHoldEnabled
    mk (Just invu) (Just invt) = pure $ mkTeamMember uid perms (Just (invu, invt)) lhStatus
    mk Nothing Nothing = pure $ mkTeamMember uid perms Nothing lhStatus
    -- TODO(leif): proper error handling
    mk _ _ = error "TeamMember with incomplete metadata."

type RawTeamMember = (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus)

teamMembersForPagination :: TeamId -> Maybe UserId -> Range 1 HardTruncationLimit Int32 -> Client (Page RawTeamMember)
teamMembersForPagination tid start (fromRange -> max) =
  case start of
    Just u -> paginate Cql.selectTeamMembersFrom (paramsP LocalQuorum (tid, u) max)
    Nothing -> paginate Cql.selectTeamMembers (paramsP LocalQuorum (Identity tid) max)

teamMembersCollectedWithPagination ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member LegalHoldStore r
  ) =>
  TeamId ->
  Sem r [TeamMember]
teamMembersCollectedWithPagination tid = do
  mems <- embedClientInput $ teamMembersForPagination tid Nothing (unsafeRange 2000)
  collect [] mems
  where
    collect acc page = do
      tMembers <- mapM (newTeamMember' tid) (result page)
      if hasMore page
        then do
          page' <- embedClientInput (nextPage page)
          collect (tMembers ++ acc) page'
        else pure (tMembers ++ acc)

teamMembersWithLimit ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member LegalHoldStore r
  ) =>
  TeamId ->
  Range 1 HardTruncationLimit Int32 ->
  Sem r TeamMemberList
teamMembersWithLimit t (fromRange -> limit) = do
  page <- embedClientInput $ retry x1 (paginate Cql.selectTeamMembers (paramsP LocalQuorum (Identity t) (limit + 1)))
  ms <- mapM (newTeamMember' t) . take (fromIntegral limit) $ result page
  pure $ if hasMore page then newTeamMemberList ms ListTruncated else newTeamMemberList ms ListComplete

teamMembersLimited ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member LegalHoldStore r
  ) =>
  TeamId ->
  [UserId] ->
  Sem r [TeamMember]
teamMembersLimited t u = do
  rows <- embedClientInput $ retry x1 (query Cql.selectTeamMembers' (params LocalQuorum (t, u)))
  mapM (\(uid, perms, _, minvu, minvt, mlh) -> newTeamMember' t (uid, perms, minvu, minvt, mlh)) rows

teamMemberInfos :: TeamId -> [UserId] -> Client [TeamMemberInfo]
teamMemberInfos t u = mkTeamMemberInfo <$$> retry x1 (query Cql.selectTeamMembers' (params LocalQuorum (t, u)))
  where
    mkTeamMemberInfo (uid, perms, permsWT, _, _, _) =
      TeamMemberInfo {Info.userId = uid, Info.permissions = perms, Info.permissionsWriteTime = toUTCTimeMillis $ writetimeToUTC permsWT}

selectSomeTeamMembersPaginated ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member LegalHoldStore r
  ) =>
  TeamId ->
  [UserId] ->
  Maybe PagingState ->
  Range 1 HardTruncationLimit Int32 ->
  Sem r (PageWithState TeamMember)
selectSomeTeamMembersPaginated tid uids pagingState (fromRange -> max) = do
  page <- embedClientInput $ paginateWithState Cql.selectTeamMembers' (paramsPagingState LocalQuorum (tid, uids) max pagingState)
  members <- mapM mkTm (pwsResults page)
  pure $ PageWithState members (pwsState page)
  where
    mkTm (uid, perms, _, minvu, minvt, fromMaybe defUserLegalHoldStatus -> lhStatus) = newTeamMember' tid (uid, perms, minvu, minvt, Just lhStatus)
