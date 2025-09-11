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

module Wire.ConversationStore.Cassandra
  ( interpretMLSCommitLockStoreToCassandra,
    interpretConversationStoreToCassandra,
  )
where

import Cassandra
import Cassandra qualified as Cql
import Cassandra.Settings
import Cassandra.Util
import Control.Arrow
import Control.Error.Util hiding (hoistMaybe)
import Control.Lens
import Control.Monad.Trans.Maybe
import Data.ByteString.Conversion
import Data.Default
import Data.Domain
import Data.Id
import Data.List.Extra qualified as List
import Data.Map qualified as Map
import Data.Misc
import Data.Monoid
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Data.Time
import Imports
import Polysemy
import Polysemy.Embed
import Polysemy.TinyLog
import System.Logger qualified as Log
import UnliftIO qualified
import Wire.API.Conversation hiding (Conversation, Member, members, newGroupId)
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role hiding (DeleteConversation)
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.Group.Serialisation
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.LeafNode (LeafIndex)
import Wire.API.MLS.SubConversation
import Wire.API.Provider.Service
import Wire.ConversationStore (ConversationStore (..), LockAcquired (..), MLSCommitLockStore (..))
import Wire.ConversationStore.Cassandra.Instances ()
import Wire.ConversationStore.Cassandra.Queries qualified as Cql
import Wire.ConversationStore.MLS.Types
import Wire.StoredConversation
import Wire.UserList
import Wire.Util

-----------------------------------------------------------------------------------------
-- CONVERSATION STORE

createConversation :: Local ConvId -> NewConversation -> Client StoredConversation
createConversation lcnv nc = do
  let storedConv = newStoredConversation lcnv nc
      meta = storedConv.metadata
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery
      Cql.insertConv
      ( storedConv.id_,
        meta.cnvmType,
        meta.cnvmCreator,
        Cql.Set meta.cnvmAccess,
        Cql.Set (toList meta.cnvmAccessRoles),
        meta.cnvmName,
        meta.cnvmTeam,
        meta.cnvmMessageTimer,
        meta.cnvmReceiptMode,
        protocolTag storedConv.protocol,
        getGroupId storedConv.protocol,
        meta.cnvmGroupConvType,
        meta.cnvmChannelAddPermission,
        meta.cnvmCellsState,
        meta.cnvmParent
      )
    for_ (cnvmTeam meta) $ \tid -> addPrepQuery Cql.insertTeamConv (tid, storedConv.id_)
  let localUsers = map (\m -> (m.id_, m.convRoleName)) storedConv.localMembers
      remoteUsers = map (\m -> (,m.convRoleName) <$> m.id_) storedConv.remoteMembers
  void $ addMembers storedConv.id_ $ UserList localUsers remoteUsers
  pure storedConv

deleteConversation :: ConvId -> Client ()
deleteConversation cid = do
  retry x5 $ write Cql.markConvDeleted (params LocalQuorum (Identity cid))

  localMembers <- members cid
  remoteMembers <- lookupRemoteMembers cid

  removeMembersFromLocalConv cid $
    UserList ((.id_) <$> localMembers) ((.id_) <$> remoteMembers)

  retry x5 $ write Cql.deleteConv (params LocalQuorum (Identity cid))

conversationMeta :: ConvId -> Client (Maybe ConversationMetadata)
conversationMeta conv =
  (toConvMeta =<<)
    <$> retry x1 (query1 Cql.selectConv (params LocalQuorum (Identity conv)))
  where
    toConvMeta (t, mc, a, r, r', n, i, _, mt, rm, _, _, _, _, _, mgct, mcap, mcs, mcp) = do
      let mbAccessRolesV2 = Set.fromList . Cql.fromSet <$> r'
          accessRoles = maybeRole t $ parseAccessRoles r mbAccessRolesV2
      pure $ ConversationMetadata t mc (defAccess t a) accessRoles n i mt rm mgct mcap (fromMaybe def mcs) mcp

getGroupInfo :: ConvId -> Client (Maybe GroupInfoData)
getGroupInfo cid = do
  (runIdentity =<<)
    <$> retry
      x1
      ( query1
          Cql.selectGroupInfo
          (params LocalQuorum (Identity cid))
      )

isConvAlive :: ConvId -> Client Bool
isConvAlive cid = do
  result <- retry x1 (query1 Cql.isConvDeleted (params LocalQuorum (Identity cid)))
  case runIdentity <$> result of
    Nothing -> pure False
    Just Nothing -> pure True
    Just (Just True) -> pure False
    Just (Just False) -> pure True

updateConvType :: ConvId -> ConvType -> Client ()
updateConvType cid ty =
  retry x5 $
    write Cql.updateConvType (params LocalQuorum (ty, cid))

updateConvName :: ConvId -> Range 1 256 Text -> Client ()
updateConvName cid name = retry x5 $ write Cql.updateConvName (params LocalQuorum (fromRange name, cid))

updateConvAccess :: ConvId -> ConversationAccessData -> Client ()
updateConvAccess cid (ConversationAccessData acc role) =
  retry x5 $
    write Cql.updateConvAccess (params LocalQuorum (Cql.Set (toList acc), Cql.Set (toList role), cid))

updateConvReceiptMode :: ConvId -> ReceiptMode -> Client ()
updateConvReceiptMode cid receiptMode = retry x5 $ write Cql.updateConvReceiptMode (params LocalQuorum (receiptMode, cid))

updateConvMessageTimer :: ConvId -> Maybe Milliseconds -> Client ()
updateConvMessageTimer cid mtimer = retry x5 $ write Cql.updateConvMessageTimer (params LocalQuorum (mtimer, cid))

getConvEpoch :: ConvId -> Client (Maybe Epoch)
getConvEpoch cid =
  (runIdentity =<<)
    <$> retry
      x1
      (query1 Cql.getConvEpoch (params LocalQuorum (Identity cid)))

updateConvEpoch :: ConvId -> Epoch -> Client ()
updateConvEpoch cid epoch = retry x5 $ write Cql.updateConvEpoch (params LocalQuorum (epoch, cid))

updateConvCipherSuite :: ConvId -> CipherSuiteTag -> Client ()
updateConvCipherSuite cid cs =
  retry x5 $
    write
      Cql.updateConvCipherSuite
      (params LocalQuorum (cs, cid))

updateConvCellsState :: ConvId -> CellsState -> Client ()
updateConvCellsState cid ps =
  retry x5 $
    write
      Cql.updateConvCellsState
      (params LocalQuorum (ps, cid))

resetConversation :: ConvId -> GroupId -> Client ()
resetConversation cid groupId =
  retry x5 $
    write
      Cql.resetConversation
      (params LocalQuorum (groupId, cid))

setGroupInfo :: ConvId -> GroupInfoData -> Client ()
setGroupInfo conv gid =
  write Cql.updateGroupInfo (params LocalQuorum (gid, conv))

getConversation :: ConvId -> Client (Maybe StoredConversation)
getConversation conv = do
  cdata <- UnliftIO.async $ retry x1 (query1 Cql.selectConv (params LocalQuorum (Identity conv)))
  remoteMems <- UnliftIO.async $ lookupRemoteMembers conv
  mbConv <-
    toConv conv
      <$> members conv
      <*> UnliftIO.wait remoteMems
      <*> UnliftIO.wait cdata
  runMaybeT $ conversationGC =<< maybe mzero pure mbConv

-- | "Garbage collect" a 'Conversation', i.e. if the conversation is
-- marked as deleted, actually remove it from the database and return
-- 'Nothing'.
conversationGC ::
  StoredConversation ->
  MaybeT Client StoredConversation
conversationGC conv =
  asum
    [ -- return conversation if not deleted
      guard (not conv.deleted) $> conv,
      -- actually delete it and fail
      lift (deleteConversation conv.id_) *> mzero
    ]

localConversation :: ConvId -> Client (Maybe StoredConversation)
localConversation cid =
  UnliftIO.runConcurrently $
    toConv cid
      <$> UnliftIO.Concurrently (members cid)
      <*> UnliftIO.Concurrently (lookupRemoteMembers cid)
      <*> UnliftIO.Concurrently
        ( retry x1 $
            query1 Cql.selectConv (params LocalQuorum (Identity cid))
        )

localConversations ::
  ( Member (Embed IO) r,
    Member TinyLog r
  ) =>
  ClientState ->
  [ConvId] ->
  Sem r [StoredConversation]
localConversations client =
  collectAndLog
    <=< ( runEmbedded (runClient client) . embed . UnliftIO.pooledMapConcurrentlyN 8 localConversation'
        )
  where
    collectAndLog cs = case partitionEithers cs of
      (errs, convs) -> traverse_ (warn . Log.msg) errs $> convs

    localConversation' :: ConvId -> Client (Either ByteString StoredConversation)
    localConversation' cid =
      note ("No conversation for: " <> toByteString' cid) <$> localConversation cid

-- | Takes a list of conversation ids and returns those found for the given
-- user.
localConversationIdsOf :: UserId -> [ConvId] -> Client [ConvId]
localConversationIdsOf usr cids = do
  runIdentity <$$> retry x1 (query Cql.selectUserConvsIn (params LocalQuorum (usr, cids)))

-- | Takes a list of remote conversation ids and fetches member status flags
-- for the given user
remoteConversationStatus ::
  UserId ->
  [Remote ConvId] ->
  Client (Map (Remote ConvId) MemberStatus)
remoteConversationStatus uid =
  fmap mconcat
    . UnliftIO.pooledMapConcurrentlyN 8 (remoteConversationStatusOnDomain uid)
    . bucketRemote

remoteConversationStatusOnDomain :: UserId -> Remote [ConvId] -> Client (Map (Remote ConvId) MemberStatus)
remoteConversationStatusOnDomain uid rconvs =
  Map.fromList . map toPair
    <$> query Cql.selectRemoteConvMemberStatuses (params LocalQuorum (uid, tDomain rconvs, tUnqualified rconvs))
  where
    toPair (conv, omus, omur, oar, oarr, hid, hidr) =
      ( qualifyAs rconvs conv,
        toMemberStatus (omus, omur, oar, oarr, hid, hidr)
      )

toProtocol ::
  Maybe ProtocolTag ->
  Maybe GroupId ->
  Maybe Epoch ->
  Maybe UTCTime ->
  Maybe CipherSuiteTag ->
  Maybe Protocol
toProtocol Nothing _ _ _ _ = Just ProtocolProteus
toProtocol (Just ProtocolProteusTag) _ _ _ _ = Just ProtocolProteus
toProtocol (Just ProtocolMLSTag) mgid mepoch mtimestamp mcs = ProtocolMLS <$> toConversationMLSData mgid mepoch mtimestamp mcs
toProtocol (Just ProtocolMixedTag) mgid mepoch mtimestamp mcs = ProtocolMixed <$> toConversationMLSData mgid mepoch mtimestamp mcs

toConversationMLSData :: Maybe GroupId -> Maybe Epoch -> Maybe UTCTime -> Maybe CipherSuiteTag -> Maybe ConversationMLSData
toConversationMLSData mgid mepoch mtimestamp mcs =
  ConversationMLSData
    <$> mgid
    <*> pure
      ( ActiveMLSConversationData
          <$> mepoch
          <*> mtimestamp
          <*> mcs
      )

toConv ::
  ConvId ->
  [LocalMember] ->
  [RemoteMember] ->
  Maybe Cql.ConvRow ->
  Maybe StoredConversation
toConv cid ms remoteMems mconv = do
  (cty, muid, acc, role, roleV2, nme, ti, del, timer, rm, ptag, mgid, mep, mts, mcs, mgct, mAp, mcells, mparent) <- mconv
  let mbAccessRolesV2 = Set.fromList . Cql.fromSet <$> roleV2
      accessRoles = maybeRole cty $ parseAccessRoles role mbAccessRolesV2
  proto <- toProtocol ptag mgid mep (writetimeToUTC <$> mts) mcs
  pure
    StoredConversation
      { id_ = cid,
        deleted = fromMaybe False del,
        localMembers = ms,
        remoteMembers = remoteMems,
        protocol = proto,
        metadata =
          ConversationMetadata
            { cnvmType = cty,
              cnvmCreator = muid,
              cnvmAccess = defAccess cty acc,
              cnvmAccessRoles = accessRoles,
              cnvmName = nme,
              cnvmTeam = ti,
              cnvmMessageTimer = timer,
              cnvmReceiptMode = rm,
              cnvmGroupConvType = mgct,
              cnvmCellsState = fromMaybe def mcells,
              cnvmChannelAddPermission = mAp,
              cnvmParent = mparent
            }
      }

updateToMixedProtocol ::
  (Member (Embed IO) r) =>
  ClientState ->
  Local ConvId ->
  ConvType ->
  Sem r ()
updateToMixedProtocol client lcnv ct = do
  let gid = newGroupId ct $ Conv <$> tUntagged lcnv
      epoch = Epoch 0
  runEmbedded (runClient client) . embed . retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery Cql.updateToMixedConv (tUnqualified lcnv, ProtocolMixedTag, gid, epoch)
  pure ()

updateToMLSProtocol ::
  (Member (Embed IO) r) =>
  ClientState ->
  Local ConvId ->
  Sem r ()
updateToMLSProtocol client lcnv =
  runEmbedded (runClient client) . embed . retry x5 $
    write Cql.updateToMLSConv (params LocalQuorum (tUnqualified lcnv, ProtocolMLSTag))

updateChannelAddPermissions :: ConvId -> AddPermission -> Client ()
updateChannelAddPermissions cid cap = retry x5 $ write Cql.updateChannelAddPermission (params LocalQuorum (cap, cid))

acquireCommitLock :: GroupId -> Epoch -> NominalDiffTime -> Client LockAcquired
acquireCommitLock groupId epoch ttl = do
  rows <-
    retry x5 $
      trans
        Cql.acquireCommitLock
        ( params
            LocalQuorum
            (groupId, epoch, round ttl)
        )
          { serialConsistency = Just LocalSerialConsistency
          }
  pure $
    if checkTransSuccess rows
      then Acquired
      else NotAcquired

releaseCommitLock :: GroupId -> Epoch -> Client ()
releaseCommitLock groupId epoch =
  retry x5 $
    write
      Cql.releaseCommitLock
      ( params
          LocalQuorum
          (groupId, epoch)
      )

checkTransSuccess :: [Row] -> Bool
checkTransSuccess [] = False
checkTransSuccess (row : _) = either (const False) (fromMaybe False) $ fromRow 0 row

removeTeamConv :: TeamId -> ConvId -> Client ()
removeTeamConv tid cid = liftClient $ do
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery Cql.markConvDeleted (Identity cid)
    addPrepQuery Cql.deleteTeamConv (tid, cid)
  deleteConversation cid

teamConversation :: TeamId -> ConvId -> Client (Maybe ConvId)
teamConversation t c =
  runIdentity <$$> retry x1 (query1 Cql.selectTeamConv (params LocalQuorum (t, c)))

getTeamConversations :: TeamId -> Client [ConvId]
getTeamConversations t =
  runIdentity <$$> retry x1 (query Cql.selectTeamConvs (params LocalQuorum (Identity t)))

deleteTeamConversations :: TeamId -> Client ()
deleteTeamConversations tid = do
  convs <- teamConversationsForPagination Nothing 2000
  remove convs
  where
    remove :: Page ConvId -> Client ()
    remove convs = do
      for_ (result convs) $ removeTeamConv tid
      unless (null $ result convs) $
        remove =<< nextPage convs

    teamConversationsForPagination ::
      Maybe ConvId ->
      Int32 ->
      Client (Page ConvId)
    teamConversationsForPagination start n =
      runIdentity <$$> case start of
        Just c -> paginate Cql.selectTeamConvsFrom (paramsP LocalQuorum (tid, c) n)
        Nothing -> paginate Cql.selectTeamConvs (paramsP LocalQuorum (Identity tid) n)

-----------------------------------------------------------------------------------------
-- MEMBERS STORE

-- | Add members to a local conversation.
-- Conversation is local, so we can add any member to it (including remote ones).
-- When the role is not specified, it defaults to admin.
-- Please make sure the conversation doesn't exceed the maximum size!
addMembers ::
  ConvId ->
  UserList (UserId, RoleName) ->
  Client ([LocalMember], [RemoteMember])
addMembers conv (UserList lusers rusers) = do
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
members conv = do
  parents <- retry x1 $ query Cql.selectConvParent (params LocalQuorum (Identity conv))
  nubBy ((==) `on` (.id_)) . concatMap (mapMaybe toMember)
    <$> UnliftIO.pooledMapConcurrentlyN 16 fetchMembers (conv : mapMaybe runIdentity parents)
  where
    fetchMembers convId =
      retry x1 $
        query Cql.selectMembers (params LocalQuorum (Identity convId))

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
member cnv usr = do
  parents <- retry x1 $ query Cql.selectConvParent (params LocalQuorum (Identity cnv))
  asum . map (toMember =<<)
    <$> UnliftIO.pooledMapConcurrentlyN 16 fetchMembers (cnv : mapMaybe runIdentity parents)
  where
    fetchMembers convId =
      retry x1 (query1 Cql.selectMember (params LocalQuorum (convId, usr)))

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
    let add r
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
      traverse_ add (omuConvRoleName omu)

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
lookupLocalMemberRemoteConv uid (tUntagged -> Qualified conv dom) =
  runIdentity
    <$$> retry
      x5
      (query1 Cql.selectRemoteConvMembers (params LocalQuorum (uid, dom, conv)))

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

-----------------------------------------------------------------------------------------
-- SUB CONVERSATION STORE

selectSubConversation :: ConvId -> SubConvId -> Client (Maybe SubConversation)
selectSubConversation convId subConvId = runMaybeT $ do
  (mSuite, mEpoch, mEpochWritetime, mGroupId) <-
    MaybeT $
      retry x5 (query1 Cql.selectSubConversation (params LocalQuorum (convId, subConvId)))
  let activeData =
        ActiveMLSConversationData
          <$> mEpoch
          <*> fmap writetimeToUTC mEpochWritetime
          <*> mSuite
  groupId <- hoistMaybe mGroupId
  (cm, im) <- lift $ lookupMLSClientLeafIndices groupId
  pure $
    SubConversation
      { scParentConvId = convId,
        scSubConvId = subConvId,
        scMLSData =
          ConversationMLSData
            { cnvmlsGroupId = groupId,
              cnvmlsActiveData = activeData
            },
        scMembers = cm,
        scIndexMap = im
      }

insertSubConversation ::
  ConvId ->
  SubConvId ->
  GroupId ->
  Client SubConversation
insertSubConversation convId subConvId groupId = do
  retry
    x5
    ( write
        Cql.insertSubConversation
        ( params
            LocalQuorum
            (convId, subConvId, Epoch 0, groupId, Nothing)
        )
    )
  pure (newSubConversation convId subConvId groupId)

updateSubConvGroupInfo :: ConvId -> SubConvId -> Maybe GroupInfoData -> Client ()
updateSubConvGroupInfo convId subConvId mGroupInfo =
  retry x5 (write Cql.updateSubConvGroupInfo (params LocalQuorum (convId, subConvId, mGroupInfo)))

selectSubConvGroupInfo :: ConvId -> SubConvId -> Client (Maybe GroupInfoData)
selectSubConvGroupInfo convId subConvId =
  (runIdentity =<<) <$> retry x5 (query1 Cql.selectSubConvGroupInfo (params LocalQuorum (convId, subConvId)))

selectSubConvEpoch :: ConvId -> SubConvId -> Client (Maybe Epoch)
selectSubConvEpoch convId subConvId =
  (runIdentity =<<) <$> retry x5 (query1 Cql.selectSubConvEpoch (params LocalQuorum (convId, subConvId)))

setEpochForSubConversation :: ConvId -> SubConvId -> Epoch -> Client ()
setEpochForSubConversation cid sconv epoch =
  retry x5 (write Cql.insertEpochForSubConversation (params LocalQuorum (epoch, cid, sconv)))

setCipherSuiteForSubConversation :: ConvId -> SubConvId -> CipherSuiteTag -> Client ()
setCipherSuiteForSubConversation cid sconv cs =
  retry x5 (write Cql.insertCipherSuiteForSubConversation (params LocalQuorum (cs, cid, sconv)))

deleteSubConversation :: ConvId -> SubConvId -> Client ()
deleteSubConversation cid sconv =
  retry x5 $ write Cql.deleteSubConversation (params LocalQuorum (cid, sconv))

listSubConversations :: ConvId -> Client (Map SubConvId ConversationMLSData)
listSubConversations cid = do
  subs <- retry x1 (query Cql.listSubConversations (params LocalQuorum (Identity cid)))
  pure . Map.fromList $ do
    (subId, cs, epoch, ts, gid) <- subs
    let activeData = case (epoch, ts) of
          (Epoch 0, _) -> Nothing
          (_, Writetime t) ->
            Just
              ActiveMLSConversationData
                { epoch = epoch,
                  epochTimestamp = t,
                  ciphersuite = cs
                }

    pure
      ( subId,
        ConversationMLSData
          { cnvmlsGroupId = gid,
            cnvmlsActiveData = activeData
          }
      )

interpretMLSCommitLockStoreToCassandra :: (Member (Embed IO) r, Member TinyLog r) => ClientState -> InterpreterFor MLSCommitLockStore r
interpretMLSCommitLockStoreToCassandra client = interpret $ \case
  AcquireCommitLock gId epoch ttl -> do
    logEffect "MLSCommitLockStore.AcquireCommitLock"
    embedClient client $ acquireCommitLock gId epoch ttl
  ReleaseCommitLock gId epoch -> do
    logEffect "MLSCommitLockStore.ReleaseCommitLock"
    embedClient client $ releaseCommitLock gId epoch

interpretConversationStoreToCassandra ::
  forall r a.
  ( Member (Embed IO) r,
    Member TinyLog r
  ) =>
  ClientState ->
  Sem (ConversationStore ': r) a ->
  Sem r a
interpretConversationStoreToCassandra client = interpret $ \case
  CreateConversation lcnv nc -> do
    logEffect "ConversationStore.CreateConversation"
    embedClient client $ createConversation lcnv nc
  GetConversation cid -> do
    logEffect "ConversationStore.GetConversation"
    embedClient client $ getConversation cid
  GetConversationEpoch cid -> do
    logEffect "ConversationStore.GetConversationEpoch"
    embedClient client $ getConvEpoch cid
  GetConversations cids -> do
    logEffect "ConversationStore.GetConversations"
    localConversations client cids
  GetConversationMetadata cid -> do
    logEffect "ConversationStore.GetConversationMetadata"
    embedClient client $ conversationMeta cid
  GetGroupInfo cid -> do
    logEffect "ConversationStore.GetGroupInfo"
    embedClient client $ getGroupInfo cid
  IsConversationAlive cid -> do
    logEffect "ConversationStore.IsConversationAlive"
    embedClient client $ isConvAlive cid
  SelectConversations uid cids -> do
    logEffect "ConversationStore.SelectConversations"
    embedClient client $ localConversationIdsOf uid cids
  GetRemoteConversationStatus uid cids -> do
    logEffect "ConversationStore.GetRemoteConversationStatus"
    embedClient client $ remoteConversationStatus uid cids
  SetConversationType cid ty -> do
    logEffect "ConversationStore.SetConversationType"
    embedClient client $ updateConvType cid ty
  SetConversationName cid value -> do
    logEffect "ConversationStore.SetConversationName"
    embedClient client $ updateConvName cid value
  SetConversationAccess cid value -> do
    logEffect "ConversationStore.SetConversationAccess"
    embedClient client $ updateConvAccess cid value
  SetConversationReceiptMode cid value -> do
    logEffect "ConversationStore.SetConversationReceiptMode"
    embedClient client $ updateConvReceiptMode cid value
  SetConversationMessageTimer cid value -> do
    logEffect "ConversationStore.SetConversationMessageTimer"
    embedClient client $ updateConvMessageTimer cid value
  SetConversationEpoch cid epoch -> do
    logEffect "ConversationStore.SetConversationEpoch"
    embedClient client $ updateConvEpoch cid epoch
  SetConversationCipherSuite cid cs -> do
    logEffect "ConversationStore.SetConversationCipherSuite"
    embedClient client $ updateConvCipherSuite cid cs
  SetConversationCellsState cid ps -> do
    logEffect "ConversationStore.SetConversationCellsState"
    embedClient client $ updateConvCellsState cid ps
  ResetConversation cid groupId -> do
    logEffect "ConversationStore.ResetConversation"
    embedClient client $ resetConversation cid groupId
  DeleteConversation cid -> do
    logEffect "ConversationStore.DeleteConversation"
    embedClient client $ deleteConversation cid
  SetGroupInfo cid gib -> do
    logEffect "ConversationStore.SetGroupInfo"
    embedClient client $ setGroupInfo cid gib
  UpdateToMixedProtocol cid ct -> do
    logEffect "ConversationStore.UpdateToMixedProtocol"
    updateToMixedProtocol client cid ct
  UpdateToMLSProtocol cid -> do
    logEffect "ConversationStore.UpdateToMLSProtocol"
    updateToMLSProtocol client cid
  UpdateChannelAddPermissions cid cap -> do
    logEffect "ConversationStore.UpdateChannelAddPermissions"
    embedClient client $ updateChannelAddPermissions cid cap
  DeleteTeamConversation tid cid -> do
    logEffect "ConversationStore.DeleteTeamConversation"
    embedClient client $ removeTeamConv tid cid
  GetTeamConversation tid cid -> do
    logEffect "ConversationStore.GetTeamConversation"
    embedClient client $ teamConversation tid cid
  GetTeamConversations tid -> do
    logEffect "ConversationStore.GetTeamConversations"
    embedClient client $ getTeamConversations tid
  DeleteTeamConversations tid -> do
    logEffect "ConversationStore.DeleteTeamConversations"
    embedClient client $ deleteTeamConversations tid
  CreateMembers cid ul -> do
    logEffect "ConversationStore.CreateMembers"
    runEmbedded (runClient client) $ embed $ addMembers cid ul
  CreateMembersInRemoteConversation rcid uids -> do
    logEffect "ConversationStore.CreateMembersInRemoteConversation"
    runEmbedded (runClient client) $ embed $ addLocalMembersToRemoteConv rcid uids
  CreateBotMember sr bid cid -> do
    logEffect "ConversationStore.CreateBotMember"
    runEmbedded (runClient client) $ embed $ addBotMember sr bid cid
  GetLocalMember cid uid -> do
    logEffect "ConversationStore.GetLocalMember"
    runEmbedded (runClient client) $ embed $ member cid uid
  GetLocalMembers cid -> do
    logEffect "ConversationStore.GetLocalMembers"
    runEmbedded (runClient client) $ embed $ members cid
  GetRemoteMember cid uid -> do
    logEffect "ConversationStore.GetRemoteMember"
    runEmbedded (runClient client) $ embed $ lookupRemoteMember cid (tDomain uid) (tUnqualified uid)
  GetRemoteMembers rcid -> do
    logEffect "ConversationStore.GetRemoteMembers"
    runEmbedded (runClient client) $ embed $ lookupRemoteMembers rcid
  CheckLocalMemberRemoteConv uid rcnv -> do
    logEffect "ConversationStore.CheckLocalMemberRemoteConv"
    fmap (not . null) $ runEmbedded (runClient client) $ embed $ lookupLocalMemberRemoteConv uid rcnv
  SelectRemoteMembers uids rcnv -> do
    logEffect "ConversationStore.SelectRemoteMembers"
    runEmbedded (runClient client) $ embed $ filterRemoteConvMembers uids rcnv
  SetSelfMember qcid luid upd -> do
    logEffect "ConversationStore.SetSelfMember"
    runEmbedded (runClient client) $ embed $ updateSelfMember qcid luid upd
  SetOtherMember lcid quid upd -> do
    logEffect "ConversationStore.SetOtherMember"
    runEmbedded (runClient client) $ embed $ updateOtherMemberLocalConv lcid quid upd
  DeleteMembers cnv ul -> do
    logEffect "ConversationStore.DeleteMembers"
    runEmbedded (runClient client) $ embed $ removeMembersFromLocalConv cnv ul
  DeleteMembersInRemoteConversation rcnv uids -> do
    logEffect "ConversationStore.DeleteMembersInRemoteConversation"
    runEmbedded (runClient client) $
      embed $
        removeLocalMembersFromRemoteConv rcnv uids
  AddMLSClients lcnv quid cs -> do
    logEffect "ConversationStore.AddMLSClients"
    runEmbedded (runClient client) $ embed $ addMLSClients lcnv quid cs
  PlanClientRemoval lcnv cids -> do
    logEffect "ConversationStore.PlanClientRemoval"
    runEmbedded (runClient client) $ embed $ planMLSClientRemoval lcnv cids
  RemoveMLSClients lcnv quid cs -> do
    logEffect "ConversationStore.RemoveMLSClients"
    runEmbedded (runClient client) $ embed $ removeMLSClients lcnv quid cs
  RemoveAllMLSClients gid -> do
    logEffect "ConversationStore.RemoveAllMLSClients"
    runEmbedded (runClient client) $ embed $ removeAllMLSClients gid
  LookupMLSClients lcnv -> do
    logEffect "ConversationStore.LookupMLSClients"
    runEmbedded (runClient client) $ embed $ lookupMLSClients lcnv
  LookupMLSClientLeafIndices lcnv -> do
    logEffect "ConversationStore.LookupMLSClientLeafIndices"
    runEmbedded (runClient client) $ embed $ lookupMLSClientLeafIndices lcnv
  GetRemoteMembersByDomain dom -> do
    logEffect "ConversationStore.GetRemoteMembersByDomain"
    runEmbedded (runClient client) $ embed $ lookupRemoteMembersByDomain dom
  GetLocalMembersByDomain dom -> do
    logEffect "ConversationStore.GetLocalMembersByDomain"
    runEmbedded (runClient client) $ embed $ lookupLocalMembersByDomain dom
  CreateSubConversation convId subConvId groupId -> do
    logEffect "ConversationStore.CreateSubConversation"
    runEmbedded (runClient client) $ embed $ insertSubConversation convId subConvId groupId
  GetSubConversation convId subConvId -> do
    logEffect "ConversationStore.GetSubConversation"
    runEmbedded (runClient client) $ embed $ selectSubConversation convId subConvId
  GetSubConversationGroupInfo convId subConvId -> do
    logEffect "ConversationStore.GetSubConversationGroupInfo"
    runEmbedded (runClient client) $ embed $ selectSubConvGroupInfo convId subConvId
  GetSubConversationEpoch convId subConvId -> do
    logEffect "ConversationStore.GetSubConversationEpoch"
    runEmbedded (runClient client) $ embed $ selectSubConvEpoch convId subConvId
  SetSubConversationGroupInfo convId subConvId mPgs -> do
    logEffect "ConversationStore.SetSubConversationGroupInfo"
    runEmbedded (runClient client) $ embed $ updateSubConvGroupInfo convId subConvId mPgs
  SetSubConversationEpoch cid sconv epoch -> do
    logEffect "ConversationStore.SetSubConversationEpoch"
    runEmbedded (runClient client) $ embed $ setEpochForSubConversation cid sconv epoch
  SetSubConversationCipherSuite cid sconv cs -> do
    logEffect "ConversationStore.SetSubConversationCipherSuite"
    runEmbedded (runClient client) $ embed $ setCipherSuiteForSubConversation cid sconv cs
  ListSubConversations cid -> do
    logEffect "ConversationStore.ListSubConversations"
    runEmbedded (runClient client) $ embed $ listSubConversations cid
  DeleteSubConversation convId subConvId -> do
    logEffect "ConversationStore.DeleteSubConversation"
    runEmbedded (runClient client) $ embed $ deleteSubConversation convId subConvId
