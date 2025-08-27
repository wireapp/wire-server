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

-- FUTUREWORK: Some queries are leaking from this module. We should eventually only export
-- the interpreter to ensure proper encapsulation when we want to migrate to a different storage backend e.g.
module Wire.MemberStore.Cassandra
  ( addMembers,
    members,
    lookupRemoteMembers,
    removeMembersFromLocalConv,
    toMemberStatus,
    lookupMLSClientLeafIndices,
    interpretMemberStoreToCassandra,
  )
where

import Cassandra
import Control.Arrow
import Control.Lens
import Data.Domain
import Data.Id
import Data.List.Extra qualified as List
import Data.Monoid
import Data.Qualified
import Data.Set qualified as Set
import Imports hiding (Set)
import Polysemy
import Polysemy.Embed
import Polysemy.TinyLog
import UnliftIO qualified
import Wire.API.Conversation.Member hiding (Member)
import Wire.API.Conversation.Role
import Wire.API.MLS.Credential
import Wire.API.MLS.Group
import Wire.API.MLS.LeafNode (LeafIndex)
import Wire.API.Provider.Service
import Wire.ConversationStore.Cassandra.Instances ()
import Wire.ConversationStore.Cassandra.Queries qualified as Cql
import Wire.ConversationStore.MLS.Types
import Wire.MemberStore (MemberStore (..))
import Wire.StoredConversation
import Wire.UserList
import Wire.Util

-- | Add members to a local conversation.
-- Conversation is local, so we can add any member to it (including remote ones).
-- When the role is not specified, it defaults to admin.
-- Please make sure the conversation doesn't exceed the maximum size!
addMembers ::
  (ToUserRole a) =>
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
        addPrepQuery Cql.insertMember (conv, u, Nothing, Nothing, r)
        addPrepQuery Cql.insertUserConv (u, conv)

  for_ (List.chunksOf 32 rusers) $ \chunk -> do
    retry x5 . batch $ do
      setType BatchLogged
      setConsistency LocalQuorum
      for_ chunk $ \(tUntagged -> Qualified (uid, role) domain) -> do
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
    for_ victims $ \(tUntagged -> Qualified uid domain) ->
      addPrepQuery Cql.removeRemoteMember (cnv, domain, uid)

members :: ConvId -> Client [LocalMember]
members conv =
  fmap (mapMaybe toMember) . retry x1 $
    query Cql.selectMembers (params LocalQuorum (Identity conv))

allMembers :: Client [LocalMember]
allMembers =
  fmap (mapMaybe toMember) . retry x1 $
    query Cql.selectAllMembers (params LocalQuorum ())

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
    Maybe RoleName
  ) ->
  Maybe LocalMember
toMember (usr, srv, prv, Just 0, omus, omur, oar, oarr, hid, hidr, crn) =
  Just $
    LocalMember
      { id_ = usr,
        service = newServiceRef <$> srv <*> prv,
        status = toMemberStatus (omus, omur, oar, oarr, hid, hidr),
        convRoleName = fromMaybe roleNameWireAdmin crn
      }
toMember _ = Nothing

newRemoteMemberWithRole :: Remote (UserId, RoleName) -> RemoteMember
newRemoteMemberWithRole ur@(tUntagged -> (Qualified (u, r) _)) =
  RemoteMember
    { id_ = qualifyAs ur u,
      convRoleName = r
    }

lookupRemoteMember :: ConvId -> Domain -> UserId -> Client (Maybe RemoteMember)
lookupRemoteMember conv domain usr = do
  mkMem <$$> retry x1 (query1 Cql.selectRemoteMember (params LocalQuorum (conv, domain, usr)))
  where
    mkMem (Identity role) =
      RemoteMember
        { id_ = toRemoteUnsafe domain usr,
          convRoleName = role
        }

lookupRemoteMembers :: ConvId -> Client [RemoteMember]
lookupRemoteMembers conv = do
  fmap (map mkMem) . retry x1 $ query Cql.selectRemoteMembers (params LocalQuorum (Identity conv))
  where
    mkMem (domain, usr, role) =
      RemoteMember
        { id_ = toRemoteUnsafe domain usr,
          convRoleName = role
        }

lookupRemoteMembersByDomain :: Domain -> Client [(ConvId, RemoteMember)]
lookupRemoteMembersByDomain dom = do
  fmap (fmap mkConvMem) . retry x1 $ query Cql.selectRemoteMembersByDomain (params LocalQuorum (Identity dom))
  where
    mkConvMem (convId, usr, role) = (convId, RemoteMember (toRemoteUnsafe dom usr) role)

lookupLocalMembersByDomain :: Domain -> Client [(ConvId, UserId)]
lookupLocalMembersByDomain dom = do
  retry x1 $ query Cql.selectLocalMembersByDomain (params LocalQuorum (Identity dom))

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
updateSelfMemberRemoteConv (tUntagged -> Qualified cid domain) luid mup = do
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
filterRemoteConvMembers users (tUntagged -> Qualified conv dom) =
  fmap Data.Monoid.getAll
    . foldMap (\muser -> (muser, Data.Monoid.All (not (null muser))))
    <$> UnliftIO.pooledMapConcurrentlyN 8 filterMember users
  where
    filterMember :: UserId -> Client [UserId]
    filterMember user =
      fmap (map runIdentity)
        . retry x1
        $ query Cql.selectRemoteConvMembers (params LocalQuorum (user, dom, conv))

lookupLocalMemberRemoteConv ::
  UserId ->
  Remote ConvId ->
  Client (Maybe UserId)
lookupLocalMemberRemoteConv userId (tUntagged -> Qualified conv dom) =
  runIdentity
    <$$> retry
      x5
      (query1 Cql.selectRemoteConvMembers (params LocalQuorum (userId, dom, conv)))

removeLocalMembersFromRemoteConv ::
  -- | The conversation to remove members from
  Remote ConvId ->
  -- | Members to remove local to this backend
  [UserId] ->
  Client ()
removeLocalMembersFromRemoteConv _ [] = pure ()
removeLocalMembersFromRemoteConv (tUntagged -> Qualified conv convDomain) victims =
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    for_ victims $ \u -> addPrepQuery Cql.deleteUserRemoteConv (u, convDomain, conv)

addMLSClients :: GroupId -> Qualified UserId -> Set.Set (ClientId, LeafIndex) -> Client ()
addMLSClients groupId (Qualified usr domain) cs = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  for_ cs $ \(c, idx) ->
    addPrepQuery Cql.addMLSClient (groupId, domain, usr, c, fromIntegral idx)

planMLSClientRemoval :: (Foldable f) => GroupId -> f ClientIdentity -> Client ()
planMLSClientRemoval groupId cids =
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    for_ cids $ \cid -> do
      addPrepQuery
        Cql.planMLSClientRemoval
        (groupId, ciDomain cid, ciUser cid, ciClient cid)

removeMLSClients :: GroupId -> Qualified UserId -> Set.Set ClientId -> Client ()
removeMLSClients groupId (Qualified usr domain) cs = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  for_ cs $ \c ->
    addPrepQuery Cql.removeMLSClient (groupId, domain, usr, c)

removeAllMLSClients :: GroupId -> Client ()
removeAllMLSClients groupId = do
  retry x5 $ write Cql.removeAllMLSClients (params LocalQuorum (Identity groupId))

-- FUTUREWORK: support adding bots to a remote conversation
addBotMember :: ServiceRef -> BotId -> ConvId -> Client BotMember
addBotMember s bot cnv = do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery Cql.insertUserConv (botUserId bot, cnv)
    addPrepQuery Cql.insertBot (cnv, bot, sid, pid)
  pure (BotMember mem)
  where
    pid = s ^. serviceRefProvider
    sid = s ^. serviceRefId
    mem = (newMember (botUserId bot)) {service = Just s}

lookupMLSClientLeafIndices :: GroupId -> Client (ClientMap LeafIndex, IndexMap)
lookupMLSClientLeafIndices groupId = do
  entries <- retry x5 (query Cql.lookupMLSClients (params LocalQuorum (Identity groupId)))
  pure $ (mkClientMap &&& mkIndexMap) entries

lookupMLSClients :: GroupId -> Client (ClientMap LeafIndex)
lookupMLSClients = fmap fst . lookupMLSClientLeafIndices

interpretMemberStoreToCassandra ::
  ( Member (Embed IO) r,
    Member TinyLog r
  ) =>
  ClientState ->
  Sem (MemberStore ': r) a ->
  Sem r a
interpretMemberStoreToCassandra client = interpret $ \case
  CreateMembers cid ul -> do
    logEffect "MemberStore.CreateMembers"
    runEmbedded (runClient client) $ embed $ addMembers cid ul
  CreateMembersInRemoteConversation rcid uids -> do
    logEffect "MemberStore.CreateMembersInRemoteConversation"
    runEmbedded (runClient client) $ embed $ addLocalMembersToRemoteConv rcid uids
  CreateBotMember sr bid cid -> do
    logEffect "MemberStore.CreateBotMember"
    runEmbedded (runClient client) $ embed $ addBotMember sr bid cid
  GetLocalMember cid uid -> do
    logEffect "MemberStore.GetLocalMember"
    runEmbedded (runClient client) $ embed $ member cid uid
  GetLocalMembers cid -> do
    logEffect "MemberStore.GetLocalMembers"
    runEmbedded (runClient client) $ embed $ members cid
  GetAllLocalMembers -> do
    logEffect "MemberStore.GetAllLocalMembers"
    runEmbedded (runClient client) $ embed $ allMembers
  GetRemoteMember cid uid -> do
    logEffect "MemberStore.GetRemoteMember"
    runEmbedded (runClient client) $ embed $ lookupRemoteMember cid (tDomain uid) (tUnqualified uid)
  GetRemoteMembers rcid -> do
    logEffect "MemberStore.GetRemoteMembers"
    runEmbedded (runClient client) $ embed $ lookupRemoteMembers rcid
  CheckLocalMemberRemoteConv uid rcnv -> do
    logEffect "MemberStore.CheckLocalMemberRemoteConv"
    fmap (not . null) $ runEmbedded (runClient client) $ embed $ lookupLocalMemberRemoteConv uid rcnv
  SelectRemoteMembers uids rcnv -> do
    logEffect "MemberStore.SelectRemoteMembers"
    runEmbedded (runClient client) $ embed $ filterRemoteConvMembers uids rcnv
  SetSelfMember qcid luid upd -> do
    logEffect "MemberStore.SetSelfMember"
    runEmbedded (runClient client) $ embed $ updateSelfMember qcid luid upd
  SetOtherMember lcid quid upd -> do
    logEffect "MemberStore.SetOtherMember"
    runEmbedded (runClient client) $ embed $ updateOtherMemberLocalConv lcid quid upd
  DeleteMembers cnv ul -> do
    logEffect "MemberStore.DeleteMembers"
    runEmbedded (runClient client) $ embed $ removeMembersFromLocalConv cnv ul
  DeleteMembersInRemoteConversation rcnv uids -> do
    logEffect "MemberStore.DeleteMembersInRemoteConversation"
    runEmbedded (runClient client) $
      embed $
        removeLocalMembersFromRemoteConv rcnv uids
  AddMLSClients lcnv quid cs -> do
    logEffect "MemberStore.AddMLSClients"
    runEmbedded (runClient client) $ embed $ addMLSClients lcnv quid cs
  PlanClientRemoval lcnv cids -> do
    logEffect "MemberStore.PlanClientRemoval"
    runEmbedded (runClient client) $ embed $ planMLSClientRemoval lcnv cids
  RemoveMLSClients lcnv quid cs -> do
    logEffect "MemberStore.RemoveMLSClients"
    runEmbedded (runClient client) $ embed $ removeMLSClients lcnv quid cs
  RemoveAllMLSClients gid -> do
    logEffect "MemberStore.RemoveAllMLSClients"
    runEmbedded (runClient client) $ embed $ removeAllMLSClients gid
  LookupMLSClients lcnv -> do
    logEffect "MemberStore.LookupMLSClients"
    runEmbedded (runClient client) $ embed $ lookupMLSClients lcnv
  LookupMLSClientLeafIndices lcnv -> do
    logEffect "MemberStore.LookupMLSClientLeafIndices"
    runEmbedded (runClient client) $ embed $ lookupMLSClientLeafIndices lcnv
  GetRemoteMembersByDomain dom -> do
    logEffect "MemberStore.GetRemoteMembersByDomain"
    runEmbedded (runClient client) $ embed $ lookupRemoteMembersByDomain dom
  GetLocalMembersByDomain dom -> do
    logEffect "MemberStore.GetLocalMembersByDomain"
    runEmbedded (runClient client) $ embed $ lookupLocalMembersByDomain dom
