-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Cassandra.Team where

import Cassandra
import Cassandra.Util
import Control.Exception (ErrorCall (ErrorCall))
import Control.Lens hiding ((<|))
import Control.Monad.Catch (throwM)
import Control.Monad.Extra (ifM)
import Data.Id as Id
import Data.Json.Util (UTCTimeMillis (..))
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import qualified Data.Map.Strict as Map
import Data.Range
import qualified Data.Set as Set
import Data.UUID.V4 (nextRandom)
import qualified Galley.Cassandra.Conversation as C
import Galley.Cassandra.LegalHold (isTeamLegalholdWhitelisted)
import Galley.Cassandra.Paging
import Galley.Cassandra.Store
import Galley.Data.Instances ()
import qualified Galley.Data.Queries as Cql
import Galley.Data.ResultSet
import Galley.Effects.ListItems
import Galley.Effects.TeamStore (TeamStore (..))
import Galley.Types.Teams hiding
  ( DeleteTeam,
    GetTeamConversations,
    SetTeamData,
  )
import qualified Galley.Types.Teams as Teams
import Galley.Types.Teams.Intra
import Imports hiding (Set, max)
import Polysemy
import qualified Polysemy.Reader as P
import qualified UnliftIO
import Wire.API.Team.Member

interpretTeamStoreToCassandra ::
  Members '[Embed IO, P.Reader ClientState] r =>
  FeatureLegalHold ->
  Sem (TeamStore ': r) a ->
  Sem r a
interpretTeamStoreToCassandra lh = interpret $ \case
  CreateTeamMember tid mem -> embedClient $ addTeamMember tid mem
  SetTeamMemberPermissions perm0 tid uid perm1 ->
    embedClient $ updateTeamMember perm0 tid uid perm1
  CreateTeam t uid n i k b -> embedClient $ createTeam t uid n i k b
  DeleteTeamMember tid uid -> embedClient $ removeTeamMember tid uid
  GetBillingTeamMembers tid -> embedClient $ listBillingTeamMembers tid
  GetTeam tid -> embedClient $ team tid
  GetTeamName tid -> embedClient $ getTeamName tid
  GetTeamConversation tid cid -> embedClient $ teamConversation tid cid
  GetTeamConversations tid -> embedClient $ getTeamConversations tid
  SelectTeams uid tids -> embedClient $ teamIdsOf uid tids
  GetTeamMember tid uid -> embedClient $ teamMember lh tid uid
  GetTeamMembersWithLimit tid n -> embedClient $ teamMembersWithLimit lh tid n
  GetTeamMembers tid -> embedClient $ teamMembersCollectedWithPagination lh tid
  SelectTeamMembers tid uids -> embedClient $ teamMembersLimited lh tid uids
  GetUserTeams uid -> embedClient $ userTeams uid
  GetUsersTeams uids -> embedClient $ usersTeams uids
  GetOneUserTeam uid -> embedClient $ oneUserTeam uid
  GetTeamsBindings tid -> embedClient $ getTeamsBindings tid
  GetTeamBinding tid -> embedClient $ getTeamBinding tid
  GetTeamCreationTime tid -> embedClient $ teamCreationTime tid
  DeleteTeam tid -> embedClient $ deleteTeam tid
  DeleteTeamConversation tid cid -> embedClient $ removeTeamConv tid cid
  SetTeamData tid upd -> embedClient $ updateTeam tid upd
  SetTeamStatus tid st -> embedClient $ updateTeamStatus tid st

interpretTeamListToCassandra ::
  Members '[Embed IO, P.Reader ClientState] r =>
  Sem (ListItems LegacyPaging TeamId ': r) a ->
  Sem r a
interpretTeamListToCassandra = interpret $ \case
  ListItems uid ps lim -> embedClient $ teamIdsFrom uid ps lim

createTeam ::
  Maybe TeamId ->
  UserId ->
  Range 1 256 Text ->
  Range 1 256 Text ->
  Maybe (Range 1 256 Text) ->
  TeamBinding ->
  Client Team
createTeam t uid (fromRange -> n) (fromRange -> i) k b = do
  tid <- maybe (Id <$> liftIO nextRandom) return t
  retry x5 $ write Cql.insertTeam (params Quorum (tid, uid, n, i, fromRange <$> k, initialStatus b, b))
  pure (newTeam tid uid n i b & teamIconKey .~ (fromRange <$> k))
  where
    initialStatus Binding = PendingActive -- Team becomes Active after User account activation
    initialStatus NonBinding = Active

listBillingTeamMembers :: TeamId -> Client [UserId]
listBillingTeamMembers tid =
  fmap runIdentity
    <$> retry x1 (query Cql.listBillingTeamMembers (params Quorum (Identity tid)))

getTeamName :: TeamId -> Client (Maybe Text)
getTeamName tid =
  fmap runIdentity
    <$> retry x1 (query1 Cql.selectTeamName (params Quorum (Identity tid)))

teamConversation :: TeamId -> ConvId -> Client (Maybe TeamConversation)
teamConversation t c =
  fmap (newTeamConversation c . runIdentity)
    <$> retry x1 (query1 Cql.selectTeamConv (params Quorum (t, c)))

getTeamConversations :: TeamId -> Client [TeamConversation]
getTeamConversations t =
  map (uncurry newTeamConversation)
    <$> retry x1 (query Cql.selectTeamConvs (params Quorum (Identity t)))

teamIdsFrom :: UserId -> Maybe TeamId -> Range 1 100 Int32 -> Client (ResultSet TeamId)
teamIdsFrom usr range (fromRange -> max) =
  mkResultSet . fmap runIdentity . strip <$> case range of
    Just c -> paginate Cql.selectUserTeamsFrom (paramsP Quorum (usr, c) (max + 1))
    Nothing -> paginate Cql.selectUserTeams (paramsP Quorum (Identity usr) (max + 1))
  where
    strip p = p {result = take (fromIntegral max) (result p)}

teamIdsForPagination :: UserId -> Maybe TeamId -> Range 1 100 Int32 -> Client (Page TeamId)
teamIdsForPagination usr range (fromRange -> max) =
  fmap runIdentity <$> case range of
    Just c -> paginate Cql.selectUserTeamsFrom (paramsP Quorum (usr, c) max)
    Nothing -> paginate Cql.selectUserTeams (paramsP Quorum (Identity usr) max)

teamMember :: FeatureLegalHold -> TeamId -> UserId -> Client (Maybe TeamMember)
teamMember lh t u =
  newTeamMember'' u =<< retry x1 (query1 Cql.selectTeamMember (params Quorum (t, u)))
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
    setConsistency Quorum
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

updateTeamMember ::
  -- | Old permissions, used for maintaining 'billing_team_member' table
  Permissions ->
  TeamId ->
  UserId ->
  -- | New permissions
  Permissions ->
  Client ()
updateTeamMember oldPerms tid uid newPerms = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    addPrepQuery Cql.updatePermissions (newPerms, tid, uid)

    when (SetBilling `Set.member` acquiredPerms) $
      addPrepQuery Cql.insertBillingTeamMember (tid, uid)

    when (SetBilling `Set.member` lostPerms) $
      addPrepQuery Cql.deleteBillingTeamMember (tid, uid)
  where
    permDiff = Set.difference `on` view Teams.self
    acquiredPerms = newPerms `permDiff` oldPerms
    lostPerms = oldPerms `permDiff` newPerms

removeTeamMember :: TeamId -> UserId -> Client ()
removeTeamMember t m =
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency Quorum
    addPrepQuery Cql.deleteTeamMember (t, m)
    addPrepQuery Cql.deleteUserTeam (m, t)
    addPrepQuery Cql.deleteBillingTeamMember (t, m)

team :: TeamId -> Client (Maybe TeamData)
team tid =
  fmap toTeam <$> retry x1 (query1 Cql.selectTeam (params Quorum (Identity tid)))
  where
    toTeam (u, n, i, k, d, s, st, b) =
      let t = newTeam tid u n i (fromMaybe NonBinding b) & teamIconKey .~ k
          status = if d then PendingDelete else fromMaybe Active s
       in TeamData t status (writeTimeToUTC <$> st)

teamIdsOf :: UserId -> [TeamId] -> Client [TeamId]
teamIdsOf usr tids =
  map runIdentity <$> retry x1 (query Cql.selectUserTeamsIn (params Quorum (usr, toList tids)))

teamMembersWithLimit ::
  FeatureLegalHold ->
  TeamId ->
  Range 1 HardTruncationLimit Int32 ->
  Client TeamMemberList
teamMembersWithLimit lh t (fromRange -> limit) = do
  -- NOTE: We use +1 as size and then trim it due to the semantics of C* when getting a page with the exact same size
  pageTuple <- retry x1 (paginate Cql.selectTeamMembers (paramsP Quorum (Identity t) (limit + 1)))
  ms <- mapM (newTeamMember' lh t) . take (fromIntegral limit) $ result pageTuple
  pure $
    if hasMore pageTuple
      then newTeamMemberList ms ListTruncated
      else newTeamMemberList ms ListComplete

-- NOTE: Use this function with care... should only be required when deleting a team!
--       Maybe should be left explicitly for the caller?
teamMembersCollectedWithPagination :: FeatureLegalHold -> TeamId -> Client [TeamMember]
teamMembersCollectedWithPagination lh tid = do
  mems <- teamMembersForPagination tid Nothing (unsafeRange 2000)
  collectTeamMembersPaginated [] mems
  where
    collectTeamMembersPaginated acc mems = do
      tMembers <- mapM (newTeamMember' lh tid) (result mems)
      if (null $ result mems)
        then collectTeamMembersPaginated (tMembers ++ acc) =<< nextPage mems
        else return (tMembers ++ acc)

-- Lookup only specific team members: this is particularly useful for large teams when
-- needed to look up only a small subset of members (typically 2, user to perform the action
-- and the target user)
teamMembersLimited :: FeatureLegalHold -> TeamId -> [UserId] -> Client [TeamMember]
teamMembersLimited lh t u =
  mapM (newTeamMember' lh t)
    =<< retry x1 (query Cql.selectTeamMembers' (params Quorum (t, u)))

userTeams :: UserId -> Client [TeamId]
userTeams u =
  map runIdentity
    <$> retry x1 (query Cql.selectUserTeams (params Quorum (Identity u)))

usersTeams :: [UserId] -> Client (Map UserId TeamId)
usersTeams uids = do
  pairs :: [(UserId, TeamId)] <-
    catMaybes
      <$> UnliftIO.pooledMapConcurrentlyN 8 (\uid -> (uid,) <$$> oneUserTeam uid) uids
  pure $ foldl' (\m (k, v) -> Map.insert k v m) Map.empty pairs

oneUserTeam :: UserId -> Client (Maybe TeamId)
oneUserTeam u =
  fmap runIdentity
    <$> retry x1 (query1 Cql.selectOneUserTeam (params Quorum (Identity u)))

teamCreationTime :: TeamId -> Client (Maybe TeamCreationTime)
teamCreationTime t =
  checkCreation . fmap runIdentity
    <$> retry x1 (query1 Cql.selectTeamBindingWritetime (params Quorum (Identity t)))
  where
    checkCreation (Just (Just ts)) = Just $ TeamCreationTime ts
    checkCreation _ = Nothing

getTeamBinding :: TeamId -> Client (Maybe TeamBinding)
getTeamBinding t =
  fmap (fromMaybe NonBinding . runIdentity)
    <$> retry x1 (query1 Cql.selectTeamBinding (params Quorum (Identity t)))

getTeamsBindings :: [TeamId] -> Client [TeamBinding]
getTeamsBindings =
  fmap catMaybes
    . UnliftIO.pooledMapConcurrentlyN 8 getTeamBinding

deleteTeam :: TeamId -> Client ()
deleteTeam tid = do
  -- TODO: delete service_whitelist records that mention this team
  retry x5 $ write Cql.markTeamDeleted (params Quorum (PendingDelete, tid))
  mems <- teamMembersForPagination tid Nothing (unsafeRange 2000)
  removeTeamMembers mems
  cnvs <- teamConversationsForPagination tid Nothing (unsafeRange 2000)
  removeConvs cnvs
  retry x5 $ write Cql.deleteTeam (params Quorum (Deleted, tid))
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
    setConsistency Quorum
    addPrepQuery Cql.markConvDeleted (Identity cid)
    addPrepQuery Cql.deleteTeamConv (tid, cid)
  C.deleteConversation cid

updateTeamStatus :: TeamId -> TeamStatus -> Client ()
updateTeamStatus t s = retry x5 $ write Cql.updateTeamStatus (params Quorum (s, t))

updateTeam :: TeamId -> TeamUpdateData -> Client ()
updateTeam tid u = retry x5 . batch $ do
  setType BatchLogged
  setConsistency Quorum
  for_ (u ^. nameUpdate) $ \n ->
    addPrepQuery Cql.updateTeamName (fromRange n, tid)
  for_ (u ^. iconUpdate) $ \i ->
    addPrepQuery Cql.updateTeamIcon (fromRange i, tid)
  for_ (u ^. iconKeyUpdate) $ \k ->
    addPrepQuery Cql.updateTeamIconKey (fromRange k, tid)

-- | Construct 'TeamMember' from database tuple.
-- If FeatureLegalHoldWhitelistTeamsAndImplicitConsent is enabled set UserLegalHoldDisabled
-- if team is whitelisted.
--
-- Throw an exception if one of invitation timestamp and inviter is 'Nothing' and the
-- other is 'Just', which can only be caused by inconsistent database content.
newTeamMember' ::
  FeatureLegalHold ->
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

    mk (Just invu) (Just invt) = pure $ TeamMember uid perms (Just (invu, invt)) lhStatus
    mk Nothing Nothing = pure $ TeamMember uid perms Nothing lhStatus
    mk _ _ = throwM $ ErrorCall "TeamMember with incomplete metadata."

teamConversationsForPagination :: TeamId -> Maybe ConvId -> Range 1 HardTruncationLimit Int32 -> Client (Page TeamConversation)
teamConversationsForPagination tid start (fromRange -> max) =
  fmap (uncurry newTeamConversation) <$> case start of
    Just c -> paginate Cql.selectTeamConvsFrom (paramsP Quorum (tid, c) max)
    Nothing -> paginate Cql.selectTeamConvs (paramsP Quorum (Identity tid) max)

type RawTeamMember = (UserId, Permissions, Maybe UserId, Maybe UTCTimeMillis, Maybe UserLegalHoldStatus)

-- This function has a bit of a difficult type to work with because we don't
-- have a pure function of type RawTeamMember -> TeamMember so we cannot fmap
-- over the ResultSet. We don't want to mess around with the Result size
-- nextPage either otherwise
teamMembersForPagination :: TeamId -> Maybe UserId -> Range 1 HardTruncationLimit Int32 -> Client (Page RawTeamMember)
teamMembersForPagination tid start (fromRange -> max) =
  case start of
    Just u -> paginate Cql.selectTeamMembersFrom (paramsP Quorum (tid, u) max)
    Nothing -> paginate Cql.selectTeamMembers (paramsP Quorum (Identity tid) max)
