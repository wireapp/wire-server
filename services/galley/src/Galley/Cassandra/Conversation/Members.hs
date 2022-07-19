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

module Galley.Cassandra.Conversation.Members
  ( addMembers,
    members,
    lookupRemoteMembers,
    removeMembersFromLocalConv,
    toMemberStatus,
    interpretMemberStoreToCassandra,
  )
where

import Cassandra
import Data.Domain
import Data.Id
import qualified Data.List.Extra as List
import Data.Monoid
import Data.Qualified
import qualified Data.Set as Set
import Galley.Cassandra.Instances ()
import qualified Galley.Cassandra.Queries as Cql
import Galley.Cassandra.Services
import Galley.Cassandra.Store
import Galley.Effects.MemberStore (MemberStore (..))
import Galley.Types.Conversations.Members
import Galley.Types.ToUserRole
import Galley.Types.UserList
import Imports hiding (Set)
import Polysemy
import Polysemy.Input
import qualified UnliftIO
import Wire.API.Conversation.Member hiding (Member)
import Wire.API.Conversation.Role
import Wire.API.Provider.Service

-- | Add members to a local conversation.
-- Conversation is local, so we can add any member to it (including remote ones).
-- When the role is not specified, it defaults to admin.
-- Please make sure the conversation doesn't exceed the maximum size!
addMembers ::
  ToUserRole a =>
  ConvId ->
  UserList a ->
  Client ([LocalMember], [RemoteMember])
addMembers conv (fmap toUserRole -> UserList lusers rusers) = do
  -- batch statement with 500 users are known to be above the batch size limit
  -- and throw "Batch too large" errors. Therefor we chunk requests and insert
  -- sequentially. (parallelizing would not aid performance as the partition
  -- key, i.e. the convId, is on the same cassandra node)
  -- chunk size 32 was chosen to lead to batch statements
  -- below the batch threshold
  -- With chunk size of 64:
  -- [galley] Server warning: Batch for [galley_test.member, galley_test.user] is of size 7040, exceeding specified threshold of 5120 by 1920.
  --
  for_ (List.chunksOf 32 lusers) $ \chunk -> do
    retry x5 . batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      for_ chunk $ \(u, r) -> do
        -- User is local, too, so we add it to both the member and the user table
        addPrepQuery Cql.insertMember (conv, u, Nothing, Nothing, r, Nothing)
        addPrepQuery Cql.insertUserConv (u, conv)

  for_ (List.chunksOf 32 rusers) $ \chunk -> do
    retry x5 . batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      for_ chunk $ \(qUntagged -> Qualified (uid, role) domain) -> do
        -- User is remote, so we only add it to the member_remote_user
        -- table, but the reverse mapping has to be done on the remote
        -- backend; so we assume an additional call to their backend has
        -- been (or will be) made separately. See Galley.API.Update.addMembers
        addPrepQuery Cql.insertRemoteMember (conv, domain, uid, role)

  pure (map newMemberWithRole lusers, map newRemoteMemberWithRole rusers)

removeMembersFromLocalConv :: ConvId -> UserList UserId -> Client ()
removeMembersFromLocalConv cnv victims = void $ do
  UnliftIO.concurrently
    (removeLocalMembersFromLocalConv cnv (ulLocals victims))
    (removeRemoteMembersFromLocalConv cnv (ulRemotes victims))

removeLocalMembersFromLocalConv :: ConvId -> [UserId] -> Client ()
removeLocalMembersFromLocalConv _ [] = pure ()
removeLocalMembersFromLocalConv cnv victims = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    for_ victims $ \victim -> do
      addPrepQuery Cql.removeMember (cnv, victim)
      addPrepQuery Cql.deleteUserConv (victim, cnv)

removeRemoteMembersFromLocalConv :: ConvId -> [Remote UserId] -> Client ()
removeRemoteMembersFromLocalConv _ [] = pure ()
removeRemoteMembersFromLocalConv cnv victims = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    for_ victims $ \(qUntagged -> Qualified uid domain) ->
      addPrepQuery Cql.removeRemoteMember (cnv, domain, uid)

members :: ConvId -> Client [LocalMember]
members conv =
  fmap (mapMaybe toMember) . retry x1 $
    query Cql.selectMembers (params LocalQuorum (Identity conv))

toMemberStatus ::
  ( -- otr muted
    Maybe MutedStatus,
    Maybe Text,
    -- otr archived
    Maybe Bool,
    Maybe Text,
    -- hidden
    Maybe Bool,
    Maybe Text
  ) ->
  MemberStatus
toMemberStatus (omus, omur, oar, oarr, hid, hidr) =
  MemberStatus
    { msOtrMutedStatus = omus,
      msOtrMutedRef = omur,
      msOtrArchived = fromMaybe False oar,
      msOtrArchivedRef = oarr,
      msHidden = fromMaybe False hid,
      msHiddenRef = hidr
    }

toMember ::
  ( UserId,
    Maybe ServiceId,
    Maybe ProviderId,
    Maybe Cql.MemberStatus,
    -- otr muted
    Maybe MutedStatus,
    Maybe Text,
    -- otr archived
    Maybe Bool,
    Maybe Text,
    -- hidden
    Maybe Bool,
    Maybe Text,
    -- conversation role name
    Maybe RoleName,
    Maybe (Cassandra.Set ClientId)
  ) ->
  Maybe LocalMember
toMember (usr, srv, prv, Just 0, omus, omur, oar, oarr, hid, hidr, crn, cs) =
  Just $
    LocalMember
      { lmId = usr,
        lmService = newServiceRef <$> srv <*> prv,
        lmStatus = toMemberStatus (omus, omur, oar, oarr, hid, hidr),
        lmConvRoleName = fromMaybe roleNameWireAdmin crn,
        lmMLSClients = maybe Set.empty (Set.fromList . fromSet) cs
      }
toMember _ = Nothing

newRemoteMemberWithRole :: Remote (UserId, RoleName) -> RemoteMember
newRemoteMemberWithRole ur@(qUntagged -> (Qualified (u, r) _)) =
  RemoteMember
    { rmId = qualifyAs ur u,
      rmConvRoleName = r,
      rmMLSClients = mempty
    }

lookupRemoteMember :: ConvId -> Domain -> UserId -> Client (Maybe RemoteMember)
lookupRemoteMember conv domain usr = do
  mkMem <$$> retry x1 (query1 Cql.selectRemoteMember (params LocalQuorum (conv, domain, usr)))
  where
    mkMem (role, clients) =
      RemoteMember
        { rmId = toRemoteUnsafe domain usr,
          rmConvRoleName = role,
          rmMLSClients = Set.fromList (fromSet clients)
        }

lookupRemoteMembers :: ConvId -> Client [RemoteMember]
lookupRemoteMembers conv = do
  fmap (map mkMem) . retry x1 $ query Cql.selectRemoteMembers (params LocalQuorum (Identity conv))
  where
    mkMem (domain, usr, role, clients) =
      RemoteMember
        { rmId = toRemoteUnsafe domain usr,
          rmConvRoleName = role,
          rmMLSClients = Set.fromList (fromSet clients)
        }

member ::
  ConvId ->
  UserId ->
  Client (Maybe LocalMember)
member cnv usr =
  (toMember =<<)
    <$> retry x1 (query1 Cql.selectMember (params LocalQuorum (cnv, usr)))

-- | Set local users as belonging to a remote conversation. This is invoked by a
-- remote galley when users from the current backend are added to conversations
-- on the remote end.
addLocalMembersToRemoteConv :: Remote ConvId -> [UserId] -> Client ()
addLocalMembersToRemoteConv _ [] = pure ()
addLocalMembersToRemoteConv rconv users = do
  -- FUTUREWORK: consider using pooledMapConcurrentlyN
  for_ (List.chunksOf 32 users) $ \chunk ->
    retry x5 . batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      for_ chunk $ \u ->
        addPrepQuery
          Cql.insertUserRemoteConv
          (u, tDomain rconv, tUnqualified rconv)

updateSelfMember ::
  Qualified ConvId ->
  Local UserId ->
  MemberUpdate ->
  Client ()
updateSelfMember qcnv lusr =
  foldQualified
    lusr
    updateSelfMemberLocalConv
    updateSelfMemberRemoteConv
    qcnv
    lusr

updateSelfMemberLocalConv ::
  Local ConvId ->
  Local UserId ->
  MemberUpdate ->
  Client ()
updateSelfMemberLocalConv lcid luid mup = do
  retry x5 . batch $ do
    setType BatchUnLogged
    setConsistency LocalQuorum
    for_ (mupOtrMuteStatus mup) $ \ms ->
      addPrepQuery
        Cql.updateOtrMemberMutedStatus
        (ms, mupOtrMuteRef mup, tUnqualified lcid, tUnqualified luid)
    for_ (mupOtrArchive mup) $ \a ->
      addPrepQuery
        Cql.updateOtrMemberArchived
        (a, mupOtrArchiveRef mup, tUnqualified lcid, tUnqualified luid)
    for_ (mupHidden mup) $ \h ->
      addPrepQuery
        Cql.updateMemberHidden
        (h, mupHiddenRef mup, tUnqualified lcid, tUnqualified luid)

updateSelfMemberRemoteConv ::
  Remote ConvId ->
  Local UserId ->
  MemberUpdate ->
  Client ()
updateSelfMemberRemoteConv (qUntagged -> Qualified cid domain) luid mup = do
  retry x5 . batch $ do
    setType BatchUnLogged
    setConsistency LocalQuorum
    for_ (mupOtrMuteStatus mup) $ \ms ->
      addPrepQuery
        Cql.updateRemoteOtrMemberMutedStatus
        (ms, mupOtrMuteRef mup, domain, cid, tUnqualified luid)
    for_ (mupOtrArchive mup) $ \a ->
      addPrepQuery
        Cql.updateRemoteOtrMemberArchived
        (a, mupOtrArchiveRef mup, domain, cid, tUnqualified luid)
    for_ (mupHidden mup) $ \h ->
      addPrepQuery
        Cql.updateRemoteMemberHidden
        (h, mupHiddenRef mup, domain, cid, tUnqualified luid)

updateOtherMemberLocalConv ::
  Local ConvId ->
  Qualified UserId ->
  OtherMemberUpdate ->
  Client ()
updateOtherMemberLocalConv lcid quid omu =
  do
    let addQuery r
          | tDomain lcid == qDomain quid =
            addPrepQuery
              Cql.updateMemberConvRoleName
              (r, tUnqualified lcid, qUnqualified quid)
          | otherwise =
            addPrepQuery
              Cql.updateRemoteMemberConvRoleName
              (r, tUnqualified lcid, qDomain quid, qUnqualified quid)
    retry x5 . batch $ do
      setType BatchUnLogged
      setConsistency LocalQuorum
      traverse_ addQuery (omuConvRoleName omu)

-- | Select only the members of a remote conversation from a list of users.
-- Return the filtered list and a boolean indicating whether the all the input
-- users are members.
filterRemoteConvMembers ::
  [UserId] ->
  Remote ConvId ->
  Client ([UserId], Bool)
filterRemoteConvMembers users (qUntagged -> Qualified conv dom) =
  fmap Data.Monoid.getAll
    . foldMap (\muser -> (muser, Data.Monoid.All (not (null muser))))
    <$> UnliftIO.pooledMapConcurrentlyN 8 filterMember users
  where
    filterMember :: UserId -> Client [UserId]
    filterMember user =
      fmap (map runIdentity)
        . retry x1
        $ query Cql.selectRemoteConvMembers (params LocalQuorum (user, dom, conv))

removeLocalMembersFromRemoteConv ::
  -- | The conversation to remove members from
  Remote ConvId ->
  -- | Members to remove local to this backend
  [UserId] ->
  Client ()
removeLocalMembersFromRemoteConv _ [] = pure ()
removeLocalMembersFromRemoteConv (qUntagged -> Qualified conv convDomain) victims =
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    for_ victims $ \u -> addPrepQuery Cql.deleteUserRemoteConv (u, convDomain, conv)

addMLSClients :: Local ConvId -> Qualified UserId -> Set.Set ClientId -> Client ()
addMLSClients lcnv =
  foldQualified
    lcnv
    (addLocalMLSClients (tUnqualified lcnv))
    (addRemoteMLSClients (tUnqualified lcnv))

addRemoteMLSClients :: ConvId -> Remote UserId -> Set.Set ClientId -> Client ()
addRemoteMLSClients cid ruid cs =
  retry x5 $
    write
      Cql.addRemoteMLSClients
      ( params
          LocalQuorum
          (Cassandra.Set (toList cs), cid, tDomain ruid, tUnqualified ruid)
      )

addLocalMLSClients :: ConvId -> Local UserId -> Set.Set ClientId -> Client ()
addLocalMLSClients cid lusr cs =
  retry x5 $
    write
      Cql.addLocalMLSClients
      ( params
          LocalQuorum
          (Cassandra.Set (toList cs), cid, tUnqualified lusr)
      )

interpretMemberStoreToCassandra ::
  Members '[Embed IO, Input ClientState] r =>
  Sem (MemberStore ': r) a ->
  Sem r a
interpretMemberStoreToCassandra = interpret $ \case
  CreateMembers cid ul -> embedClient $ addMembers cid ul
  CreateMembersInRemoteConversation rcid uids ->
    embedClient $ addLocalMembersToRemoteConv rcid uids
  CreateBotMember sr bid cid -> embedClient $ addBotMember sr bid cid
  GetLocalMember cid uid -> embedClient $ member cid uid
  GetLocalMembers cid -> embedClient $ members cid
  GetRemoteMember cid uid -> embedClient $ lookupRemoteMember cid (tDomain uid) (tUnqualified uid)
  GetRemoteMembers rcid -> embedClient $ lookupRemoteMembers rcid
  SelectRemoteMembers uids rcnv -> embedClient $ filterRemoteConvMembers uids rcnv
  SetSelfMember qcid luid upd -> embedClient $ updateSelfMember qcid luid upd
  SetOtherMember lcid quid upd ->
    embedClient $ updateOtherMemberLocalConv lcid quid upd
  DeleteMembers cnv ul -> embedClient $ removeMembersFromLocalConv cnv ul
  DeleteMembersInRemoteConversation rcnv uids ->
    embedClient $
      removeLocalMembersFromRemoteConv rcnv uids
  AddMLSClients lcnv quid cs -> embedClient $ addMLSClients lcnv quid cs
