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
  ( interpretConversationStoreToCassandra,
  )
where

import Cassandra hiding (Set)
import Cassandra qualified as Cql
import Cassandra.Settings
import Cassandra.Util
import Control.Error.Util
import Control.Monad.Trans.Maybe
import Data.ByteString.Conversion
import Data.Default
import Data.Id
import Data.Map qualified as Map
import Data.Misc
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Data.Time
import Data.UUID.V4 (nextRandom)
import Imports
import Polysemy
import Polysemy.Embed
import Polysemy.TinyLog
import System.Logger qualified as Log
import System.Logger.Message
import UnliftIO qualified
import Wire.API.Conversation hiding (Conversation, Member, members, newGroupId)
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role hiding (DeleteConversation)
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Group.Serialisation
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.SubConversation
import Wire.API.User
import Wire.ConversationStore (ConversationStore (..), LockAcquired (..))
import Wire.ConversationStore.Cassandra.Instances ()
import Wire.ConversationStore.Cassandra.Queries qualified as Cql
import Wire.MemberStore.Cassandra
import Wire.StoredConversation
import Wire.UserList

createMLSSelfConversation ::
  Local UserId ->
  Client StoredConversation
createMLSSelfConversation lusr = do
  let cnv = mlsSelfConvId . tUnqualified $ lusr
      usr = tUnqualified lusr
      nc =
        NewConversation
          { metadata =
              (defConversationMetadata (Just usr)) {cnvmType = SelfConv},
            users = ulFromLocals [toUserRole usr],
            protocol = BaseProtocolMLSTag
          }
      meta = nc.metadata
      gid =
        newGroupId meta.cnvmType
          . fmap Conv
          . tUntagged
          . qualifyAs lusr
          $ cnv
      proto =
        ProtocolMLS
          ConversationMLSData
            { cnvmlsGroupId = gid,
              cnvmlsActiveData = Nothing
            }
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery
      Cql.insertMLSSelfConv
      ( cnv,
        cnvmType meta,
        cnvmCreator meta,
        Cql.Set (cnvmAccess meta),
        Cql.Set (toList (cnvmAccessRoles meta)),
        cnvmName meta,
        cnvmTeam meta,
        cnvmMessageTimer meta,
        cnvmReceiptMode meta,
        Just gid
      )

  (lmems, rmems) <- addMembers cnv nc.users
  pure
    StoredConversation
      { id_ = cnv,
        localMembers = lmems,
        remoteMembers = rmems,
        deleted = False,
        metadata = meta,
        protocol = proto
      }

createConversation :: Local ConvId -> NewConversation -> Client StoredConversation
createConversation lcnv nc = do
  let meta = nc.metadata
      (proto, mgid) = case nc.protocol of
        BaseProtocolProteusTag -> (ProtocolProteus, Nothing)
        BaseProtocolMLSTag ->
          let gid =
                newGroupId meta.cnvmType $
                  Conv <$> tUntagged lcnv
           in ( ProtocolMLS
                  ConversationMLSData
                    { cnvmlsGroupId = gid,
                      cnvmlsActiveData = Nothing
                    },
                Just gid
              )
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery
      Cql.insertConv
      ( tUnqualified lcnv,
        meta.cnvmType,
        meta.cnvmCreator,
        Cql.Set meta.cnvmAccess,
        Cql.Set (toList meta.cnvmAccessRoles),
        meta.cnvmName,
        meta.cnvmTeam,
        meta.cnvmMessageTimer,
        meta.cnvmReceiptMode,
        baseProtocolToProtocol nc.protocol,
        mgid,
        meta.cnvmGroupConvType,
        meta.cnvmChannelAddPermission,
        meta.cnvmCellsState
      )
    for_ (cnvmTeam meta) $ \tid -> addPrepQuery Cql.insertTeamConv (tid, tUnqualified lcnv)
  (lmems, rmems) <- addMembers (tUnqualified lcnv) nc.users
  pure
    StoredConversation
      { id_ = tUnqualified lcnv,
        localMembers = lmems,
        remoteMembers = rmems,
        deleted = False,
        metadata = meta,
        protocol = proto
      }

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
    toConvMeta (t, mc, a, r, r', n, i, _, mt, rm, _, _, _, _, _, mgct, mcap, mcs) = do
      let mbAccessRolesV2 = Set.fromList . Cql.fromSet <$> r'
          accessRoles = maybeRole t $ parseAccessRoles r mbAccessRolesV2
      pure $ ConversationMetadata t mc (defAccess t a) accessRoles n i mt rm mgct mcap (fromMaybe def mcs)

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
  (cty, muid, acc, role, roleV2, nme, ti, del, timer, rm, ptag, mgid, mep, mts, mcs, mgct, mAp, mcells) <- mconv
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
              cnvmChannelAddPermission = mAp
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

interpretConversationStoreToCassandra ::
  forall r a.
  ( Member (Embed IO) r,
    Member TinyLog r
  ) =>
  ClientState ->
  Sem (ConversationStore ': r) a ->
  Sem r a
interpretConversationStoreToCassandra client = interpret $ \case
  CreateConversationId -> do
    logEffect "ConversationStore.CreateConversationId"
    Id <$> embed nextRandom
  CreateConversation loc nc -> do
    logEffect "ConversationStore.CreateConversation"
    embedClient $ createConversation loc nc
  CreateMLSSelfConversation lusr -> do
    logEffect "ConversationStore.CreateMLSSelfConversation"
    embedClient $ createMLSSelfConversation lusr
  GetConversation cid -> do
    logEffect "ConversationStore.GetConversation"
    embedClient $ getConversation cid
  GetConversationEpoch cid -> do
    logEffect "ConversationStore.GetConversationEpoch"
    embedClient $ getConvEpoch cid
  GetConversations cids -> do
    logEffect "ConversationStore.GetConversations"
    localConversations client cids
  GetConversationMetadata cid -> do
    logEffect "ConversationStore.GetConversationMetadata"
    embedClient $ conversationMeta cid
  GetGroupInfo cid -> do
    logEffect "ConversationStore.GetGroupInfo"
    embedClient $ getGroupInfo cid
  IsConversationAlive cid -> do
    logEffect "ConversationStore.IsConversationAlive"
    embedClient $ isConvAlive cid
  SelectConversations uid cids -> do
    logEffect "ConversationStore.SelectConversations"
    embedClient $ localConversationIdsOf uid cids
  GetRemoteConversationStatus uid cids -> do
    logEffect "ConversationStore.GetRemoteConversationStatus"
    embedClient $ remoteConversationStatus uid cids
  SetConversationType cid ty -> do
    logEffect "ConversationStore.SetConversationType"
    embedClient $ updateConvType cid ty
  SetConversationName cid value -> do
    logEffect "ConversationStore.SetConversationName"
    embedClient $ updateConvName cid value
  SetConversationAccess cid value -> do
    logEffect "ConversationStore.SetConversationAccess"
    embedClient $ updateConvAccess cid value
  SetConversationReceiptMode cid value -> do
    logEffect "ConversationStore.SetConversationReceiptMode"
    embedClient $ updateConvReceiptMode cid value
  SetConversationMessageTimer cid value -> do
    logEffect "ConversationStore.SetConversationMessageTimer"
    embedClient $ updateConvMessageTimer cid value
  SetConversationEpoch cid epoch -> do
    logEffect "ConversationStore.SetConversationEpoch"
    embedClient $ updateConvEpoch cid epoch
  SetConversationCipherSuite cid cs -> do
    logEffect "ConversationStore.SetConversationCipherSuite"
    embedClient $ updateConvCipherSuite cid cs
  SetConversationCellsState cid ps -> do
    logEffect "ConversationStore.SetConversationCellsState"
    embedClient $ updateConvCellsState cid ps
  ResetConversation cid groupId -> do
    logEffect "ConversationStore.ResetConversation"
    embedClient $ resetConversation cid groupId
  DeleteConversation cid -> do
    logEffect "ConversationStore.DeleteConversation"
    embedClient $ deleteConversation cid
  SetGroupInfo cid gib -> do
    logEffect "ConversationStore.SetGroupInfo"
    embedClient $ setGroupInfo cid gib
  AcquireCommitLock gId epoch ttl -> do
    logEffect "ConversationStore.AcquireCommitLock"
    embedClient $ acquireCommitLock gId epoch ttl
  ReleaseCommitLock gId epoch -> do
    logEffect "ConversationStore.ReleaseCommitLock"
    embedClient $ releaseCommitLock gId epoch
  UpdateToMixedProtocol cid ct -> do
    logEffect "ConversationStore.UpdateToMixedProtocol"
    updateToMixedProtocol client cid ct
  UpdateToMLSProtocol cid -> do
    logEffect "ConversationStore.UpdateToMLSProtocol"
    updateToMLSProtocol client cid
  UpdateChannelAddPermissions cid cap -> do
    logEffect "ConversationStore.UpdateChannelAddPermissions"
    embedClient $ updateChannelAddPermissions cid cap
  DeleteTeamConversation tid cid -> do
    logEffect "ConversationStore.DeleteTeamConversation"
    embedClient $ removeTeamConv tid cid
  where
    embedClient :: Client x -> Sem r x
    embedClient = runEmbedded (runClient client) . embed

    logEffect :: ByteString -> Sem r ()
    logEffect = debug . msg . val
