{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wwarn #-}

module Wire.ConversationStore.Postgres where

import Data.Domain
import Data.Id
import Data.Misc
import Data.Qualified
import Data.Range
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector qualified as Vector
import Hasql.Pool qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Hasql.Transaction (Transaction)
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (ReadCommitted), Mode (Write))
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role hiding (DeleteConversation)
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.LeafNode
import Wire.API.MLS.SubConversation
import Wire.API.PostgresMarshall
import Wire.API.Provider.Service
import Wire.ConversationStore
import Wire.ConversationStore.MLS.Types
import Wire.Postgres
import Wire.StoredConversation
import Wire.UserList

type PGConstraints r =
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Error Hasql.UsageError) r
  )

interpretConversationStoreToPostgres :: (PGConstraints r) => InterpreterFor ConversationStore r
interpretConversationStoreToPostgres = interpret $ \case
  CreateConversation lcnv nc -> createConversationImpl lcnv nc
  GetConversation cid -> getConversationImpl cid
  GetConversationEpoch cid -> getConversationEpochImpl cid
  GetConversations cids -> getConversationsImpl cids
  GetConversationMetadata cid -> getConversationMetadataImpl cid
  GetGroupInfo cid -> getGroupInfoImpl cid
  IsConversationAlive cid -> isConversationAliveImpl cid
  SelectConversations uid cids -> selectConversationsImpl uid cids
  GetRemoteConversationStatus uid cids -> getRemoteConversationStatusImpl uid cids
  SetConversationType cid ty -> setConversationTypeImpl cid ty
  SetConversationName cid value -> setConversationNameImpl cid value
  SetConversationAccess cid value -> setConversationAccessImpl cid value
  SetConversationReceiptMode cid value -> setConversationReceiptModeImpl cid value
  SetConversationMessageTimer cid value -> setConversationMessageTimerImpl cid value
  SetConversationEpoch cid epoch -> setConversationEpochImpl cid epoch
  SetConversationCipherSuite cid cs -> setConversationCipherSuiteImpl cid cs
  SetConversationCellsState cid ps -> setConversationCellsStateImpl cid ps
  ResetConversation cid groupId -> resetConversationImpl cid groupId
  DeleteConversation cid -> deleteConversationImpl cid
  SetGroupInfo cid gib -> setGroupInfoImpl cid gib
  UpdateToMixedProtocol cid gid epoch -> updateToMixedProtocolImpl cid gid epoch
  UpdateToMLSProtocol cid -> updateToMLSProtocolImpl cid
  UpdateChannelAddPermissions cid cap -> updateChannelAddPermissionsImpl cid cap
  DeleteTeamConversation tid cid -> deleteTeamConversationImpl tid cid
  GetTeamConversation tid cid -> getTeamConversationImpl tid cid
  GetTeamConversations tid -> getTeamConversationsImpl tid
  DeleteTeamConversations tid -> deleteTeamConversationsImpl tid
  CreateMembers cid ul -> createMembersImpl cid ul
  CreateMembersInRemoteConversation rcid uids -> createMembersInRemoteConversationImpl rcid uids
  CreateBotMember sr bid cid -> createBotMemberImpl sr bid cid
  GetLocalMember cid uid -> getLocalMemberImpl cid uid
  GetLocalMembers cid -> getLocalMembersImpl cid
  GetRemoteMember cid uid -> getRemoteMemberImpl cid uid
  GetRemoteMembers rcid -> getRemoteMembersImpl rcid
  CheckLocalMemberRemoteConv uid rcnv -> checkLocalMemberRemoteConvImpl uid rcnv
  SelectRemoteMembers uids rcnv -> selectRemoteMembersImpl uids rcnv
  SetSelfMember qcid luid upd -> setSelfMemberImpl qcid luid upd
  SetOtherMember lcid quid upd -> setOtherMemberImpl lcid quid upd
  DeleteMembers cnv ul -> deleteMembersImpl cnv ul
  DeleteMembersInRemoteConversation rcnv uids -> deleteMembersInRemoteConversationImpl rcnv uids
  AddMLSClients lcnv quid cs -> addMLSClientsImpl lcnv quid cs
  PlanClientRemoval lcnv cids -> planClientRemovalImpl lcnv cids
  RemoveMLSClients lcnv quid cs -> removeMLSClientsImpl lcnv quid cs
  RemoveAllMLSClients gid -> removeAllMLSClientsImpl gid
  LookupMLSClients lcnv -> lookupMLSClientsImpl lcnv
  LookupMLSClientLeafIndices lcnv -> lookupMLSClientLeafIndicesImpl lcnv
  GetRemoteMembersByDomain dom -> getRemoteMembersByDomainImpl dom
  GetLocalMembersByDomain dom -> getLocalMembersByDomainImpl dom
  CreateSubConversation convId subConvId groupId -> createSubConversationImpl convId subConvId groupId
  GetSubConversation convId subConvId -> getSubConversationImpl convId subConvId
  GetSubConversationGroupInfo convId subConvId -> getSubConversationGroupInfoImpl convId subConvId
  GetSubConversationEpoch convId subConvId -> getSubConversationEpochImpl convId subConvId
  SetSubConversationGroupInfo convId subConvId mPgs -> setSubConversationGroupInfoImpl convId subConvId mPgs
  SetSubConversationEpoch cid sconv epoch -> setSubConversationEpochImpl cid sconv epoch
  SetSubConversationCipherSuite cid sconv cs -> setSubConversationCipherSuiteImpl cid sconv cs
  ListSubConversations cid -> listSubConversationsImpl cid
  DeleteSubConversation convId subConvId -> deleteSubConversationImpl convId subConvId

createConversationImpl :: (PGConstraints r) => Local ConvId -> NewConversation -> Sem r StoredConversation
createConversationImpl lcnv nc = do
  let storedConv = newStoredConversation lcnv nc
      meta = storedConv.metadata
      localUsers = map (\m -> (m.id_, m.convRoleName)) storedConv.localMembers
      remoteUsers = map (\m -> (,m.convRoleName) <$> m.id_) storedConv.remoteMembers
      convRow =
        ( storedConv.id_,
          meta.cnvmType,
          meta.cnvmCreator,
          Vector.fromList meta.cnvmAccess,
          meta.cnvmAccessRoles,
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
  runTransaction ReadCommitted Write $ do
    Transaction.statement convRow insertConvStatement
    createMembersTransaction storedConv.id_ $ UserList localUsers remoteUsers
  pure storedConv
  where
    insertConvStatement =
      lmapPG @_ @(_, _, _, Vector Int32, Vector Int32, _, _, _, _, _, _, _, _, _, _)
        [resultlessStatement|insert into conversation
                             (id, type, creator, access, access_roles_v2,
                              name, team, message_timer, receipt_mode, protocol,
                              group_id, group_conv_type, channel_add_permission, cells_state, parent_conv)
                             values
                             ($1 :: uuid, $2 :: integer, $3 :: uuid?, $4 :: integer[], $5 :: integer[],
                              $6 :: text?, $7 :: uuid?, $8 :: bigint?, $9 :: integer?, $10 :: integer,
                              $11 :: bytea?, $12 ::integer?, $13 :: integer?, $14 :: integer, $15 :: uuid?)|]

deleteConversationImpl :: ConvId -> Sem r ()
deleteConversationImpl = undefined

getConversationImpl :: ConvId -> Sem r (Maybe StoredConversation)
getConversationImpl = undefined

getConversationEpochImpl :: ConvId -> Sem r (Maybe Epoch)
getConversationEpochImpl = undefined

getConversationsImpl :: [ConvId] -> Sem r [StoredConversation]
getConversationsImpl = undefined

getConversationMetadataImpl :: ConvId -> Sem r (Maybe ConversationMetadata)
getConversationMetadataImpl = undefined

getGroupInfoImpl :: ConvId -> Sem r (Maybe GroupInfoData)
getGroupInfoImpl = undefined

isConversationAliveImpl :: ConvId -> Sem r Bool
isConversationAliveImpl = undefined

getRemoteConversationStatusImpl :: UserId -> [Remote ConvId] -> Sem r (Map (Remote ConvId) MemberStatus)
getRemoteConversationStatusImpl = undefined

selectConversationsImpl :: UserId -> [ConvId] -> Sem r [ConvId]
selectConversationsImpl = undefined

setConversationTypeImpl :: (PGConstraints r) => ConvId -> ConvType -> Sem r ()
setConversationTypeImpl convId typ =
  runStatement (convId, typ) update
  where
    update :: Hasql.Statement (ConvId, ConvType) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET type = ($2 :: integer)
                             WHERE conv = ($1 :: uuid)|]

setConversationNameImpl :: (PGConstraints r) => ConvId -> Range 1 256 Text -> Sem r ()
setConversationNameImpl convId (fromRange -> name) =
  runStatement (convId, name) update
  where
    update :: Hasql.Statement (ConvId, Text) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET name = ($2 :: text)
                             WHERE conv = ($1 :: uuid)|]

setConversationAccessImpl :: (PGConstraints r) => ConvId -> ConversationAccessData -> Sem r ()
setConversationAccessImpl convId accessData =
  runStatement (convId, accessData.cupAccess, accessData.cupAccessRoles) update
  where
    update :: Hasql.Statement (ConvId, Set Access, Set AccessRole) ()
    update =
      lmapPG @_ @(_, Vector _, Vector _)
        [resultlessStatement|UPDATE conversation
                             SET access = ($2 :: integer[]), access_roles_v2 = ($3 :: integer[])
                             WHERE conv = ($1 :: uuid)|]

setConversationReceiptModeImpl :: (PGConstraints r) => ConvId -> ReceiptMode -> Sem r ()
setConversationReceiptModeImpl convId receiptMode =
  runStatement (convId, receiptMode) update
  where
    update :: Hasql.Statement (ConvId, ReceiptMode) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET receipt_mode = ($2 :: integer)
                             WHERE conv = ($1 :: uuid)|]

setConversationMessageTimerImpl :: (PGConstraints r) => ConvId -> Maybe Milliseconds -> Sem r ()
setConversationMessageTimerImpl convId timer =
  runStatement (convId, timer) update
  where
    update :: Hasql.Statement (ConvId, Maybe Milliseconds) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET message_timer = ($2 :: bigint?)
                             WHERE conv = ($1 :: uuid)|]

setConversationEpochImpl :: (PGConstraints r) => ConvId -> Epoch -> Sem r ()
setConversationEpochImpl convId epoch =
  runStatement (convId, epoch) update
  where
    update :: Hasql.Statement (ConvId, Epoch) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET epoch = ($2 :: bigint), epoch_timestamp = NOW()
                             WHERE conv = ($1 :: uuid)|]

setConversationCipherSuiteImpl :: (PGConstraints r) => ConvId -> CipherSuiteTag -> Sem r ()
setConversationCipherSuiteImpl convId cs =
  runStatement (convId, cs) update
  where
    update :: Hasql.Statement (ConvId, CipherSuiteTag) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET cipher_suite = ($2 :: integer)
                             WHERE conv = ($1 :: uuid)|]

setConversationCellsStateImpl :: (PGConstraints r) => ConvId -> CellsState -> Sem r ()
setConversationCellsStateImpl convId cells =
  runStatement (convId, cells) update
  where
    update :: Hasql.Statement (ConvId, CellsState) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET cells_state = ($2 :: integer)
                             WHERE conv = ($1 :: uuid)|]

resetConversationImpl :: (PGConstraints r) => ConvId -> GroupId -> Sem r ()
resetConversationImpl convId groupId =
  runStatement (convId, groupId) update
  where
    update :: Hasql.Statement (ConvId, GroupId) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET group_id = ($2 :: bytea), epoch = 0, epoch_timestamp = NOW()
                             WHERE conv = ($1 :: uuid)|]

setGroupInfoImpl :: (PGConstraints r) => ConvId -> GroupInfoData -> Sem r ()
setGroupInfoImpl convId groupInfo =
  runStatement (convId, groupInfo) update
  where
    update :: Hasql.Statement (ConvId, GroupInfoData) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET group_info = ($2 :: bytea)
                             WHERE conv = ($1 :: uuid)|]

updateChannelAddPermissionsImpl :: (PGConstraints r) => ConvId -> AddPermission -> Sem r ()
updateChannelAddPermissionsImpl convId addPerm =
  runStatement (convId, addPerm) update
  where
    update :: Hasql.Statement (ConvId, AddPermission) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET add_permission = ($2 :: integer)
                             WHERE conv = ($1 :: uuid)|]

updateToMixedProtocolImpl :: (PGConstraints r) => ConvId -> GroupId -> Epoch -> Sem r ()
updateToMixedProtocolImpl convId gid epoch =
  runStatement (convId, ProtocolMixedTag, gid, epoch) update
  where
    update :: Hasql.Statement (ConvId, ProtocolTag, GroupId, Epoch) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET protocol = ($2 :: integer), group_id = ($3 :: bytea), epoch = ($4 :: bigint), epoch_timestamp = NOW()
                             WHERE conv = ($1 :: uuid)|]

updateToMLSProtocolImpl :: (PGConstraints r) => ConvId -> Sem r ()
updateToMLSProtocolImpl convId =
  runStatement (convId, ProtocolMLSTag) update
  where
    update :: Hasql.Statement (ConvId, ProtocolTag) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET protocol = ($2 :: integer), receipt_mode = 0
                             WHERE conv = ($1 :: uuid)|]

deleteTeamConversationImpl :: TeamId -> ConvId -> Sem r ()
deleteTeamConversationImpl = undefined

getTeamConversationImpl :: TeamId -> ConvId -> Sem r (Maybe ConvId)
getTeamConversationImpl = undefined

getTeamConversationsImpl :: TeamId -> Sem r [ConvId]
getTeamConversationsImpl = undefined

deleteTeamConversationsImpl :: TeamId -> Sem r ()
deleteTeamConversationsImpl = undefined

-- MEMBER OPERATIONS
createMembersImpl :: (PGConstraints r) => ConvId -> UserList (UserId, RoleName) -> Sem r ([LocalMember], [RemoteMember])
createMembersImpl convId users@(UserList lusers rusers) = do
  runTransaction ReadCommitted Write $ createMembersTransaction convId users
  pure (map newMemberWithRole lusers, map newRemoteMemberWithRole rusers)

createMembersTransaction :: ConvId -> UserList (UserId, RoleName) -> Transaction ()
createMembersTransaction convId (UserList lusers rusers) = do
  for_ lusers $ \(u, r) ->
    Transaction.statement (convId, u, r) insertLocalStatement
  for_ rusers $ \(tUntagged -> Qualified (uid, role) domain) ->
    Transaction.statement (convId, domain, uid, role) insertRemoteStatement
  where
    insertLocalStatement :: Hasql.Statement (ConvId, UserId, RoleName) ()
    insertLocalStatement =
      lmapPG
        [resultlessStatement|insert into conversation_member (conv, "user", status, conversation_role)
                             values ($1 :: uuid, $2 :: uuid, 0, $3 :: text)|]
    insertRemoteStatement :: Hasql.Statement (ConvId, Domain, UserId, RoleName) ()
    insertRemoteStatement =
      lmapPG
        [resultlessStatement|insert into member_remote_user (conv, user_remote_domain, user_remote_id, conversation_role)
                             values ($1 :: uuid, $2 :: text, $3 :: uuid, $4 :: text)|]

createMembersInRemoteConversationImpl :: (PGConstraints r) => Remote ConvId -> [UserId] -> Sem r ()
createMembersInRemoteConversationImpl (tUntagged -> Qualified cnv domain) users =
  -- TODO: Use 'unnest' instead of multiple insert statements
  runTransaction ReadCommitted Write $
    for_ users $ \uid ->
      Transaction.statement (uid, domain, cnv) insertMember
  where
    insertMember :: Hasql.Statement (UserId, Domain, ConvId) ()
    insertMember =
      lmapPG
        [resultlessStatement|INSERT INTO user_remote_conv ("user", conv_remote_domain, conv_remote_id)
                             VALUES ($1 :: uuid, $2 :: text, $3 :: uuid)|]

createBotMemberImpl :: (PGConstraints r) => ServiceRef -> BotId -> ConvId -> Sem r BotMember
createBotMemberImpl serviceRef botId convId = do
  runStatement (convId, botId, serviceRef._serviceRefId, serviceRef._serviceRefProvider) insert
  pure . BotMember $ (newMember botId.botUserId) {service = Just serviceRef}
  where
    insert :: Hasql.Statement (ConvId, BotId, ServiceId, ProviderId) ()
    insert =
      lmapPG
        [resultlessStatement|INSERT INTO member (conv, "user", service, provider, status)
                             VALUES ($1 :: uuid, $2 :: uuid, $3 :: uuid, $4 :: uuid, 0)
                            |]

getLocalMemberImpl :: (PGConstraints r) => ConvId -> UserId -> Sem r (Maybe LocalMember)
getLocalMemberImpl convId userId = do
  mRow <- runStatement (convId, userId) selectMember
  pure $ snd . mkLocalMember <$> mRow
  where
    selectMember :: Hasql.Statement (ConvId, UserId) (Maybe (ConvId, UserId, Maybe ServiceId, Maybe ProviderId, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text, Maybe RoleName))
    selectMember =
      dimapPG
        [maybeStatement|SELECT (conv :: uuid), ("user" :: uuid), (service :: uuid?), (provider :: uuid?), (otr_muted_status :: integer?), (otr_muted_ref :: text?),
                                (otr_archived :: boolean?), (otr_archived_ref :: text?), (hidden :: boolean?), (hidden_ref :: text?), (conversation_role :: text?)
                        FROM conversation_member
                        WHERE status != 0
                        AND (conv = ($1 :: uuid)
                             OR conv IN (SELECT parent_conv FROM conversation WHERE id = ($1 :: uuid)))
                        AND "user" = ($2 :: uuid)
                        ORDER BY CASE
                          WHEN conv = ($1 :: uuid) THEN 1
                          ELSE 2
                          END
                        LIMIT 1
                       |]

getLocalMembersImpl :: (PGConstraints r) => ConvId -> Sem r [LocalMember]
getLocalMembersImpl convId = do
  rows <- runStatement convId selectMembers
  pure . map snd $ nubBy ((==) `on` ((.id_) . snd)) (V.toList (mkLocalMember <$> rows))
  where
    selectMembers :: Hasql.Statement ConvId (Vector (ConvId, UserId, Maybe ServiceId, Maybe ProviderId, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text, Maybe RoleName))
    selectMembers =
      dimapPG
        [vectorStatement|SELECT (conv :: uuid), (user :: uuid), (service :: uuid?), (provider :: uuid?), (otr_muted_status :: integer?), (otr_muted_ref :: text?),
                                (otr_archived :: boolean?), (otr_archived_ref :: text?), (hidden :: boolean?), (hidden_ref :: text?), (conversation_role :: text?)
                         FROM conversation_member
                         WHERE status != 0
                         AND (conv = ($1 :: uuid)
                              OR conv IN (SELECT parent_conv FROM conversation WHERE id = ($1 :: uuid)))
                         ORDER BY CASE
                           WHEN conv = ($1 :: uuid) THEN 1
                           ELSE 2
                           END
                        |]

mkLocalMember :: (ConvId, UserId, Maybe ServiceId, Maybe ProviderId, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text, Maybe RoleName) -> (ConvId, LocalMember)
mkLocalMember (cid, uid, mServiceId, mProviderId, msOtrMutedStatus, msOtrMutedRef, archived, msOtrArchivedRef, hidden, msHiddenRef, mRole) =
  ( cid,
    LocalMember
      { id_ = uid,
        status =
          MemberStatus
            { msOtrArchived = fromMaybe False archived,
              msHidden = fromMaybe False hidden,
              ..
            },
        service = ServiceRef <$> mServiceId <*> mProviderId,
        convRoleName = fromMaybe roleNameWireAdmin mRole
      }
  )

getRemoteMemberImpl :: (PGConstraints r) => ConvId -> Remote UserId -> Sem r (Maybe RemoteMember)
getRemoteMemberImpl convId (tUntagged -> Qualified uid domain) =
  snd . mkRemoteMember <$$> runStatement (convId, domain, uid) selectMember
  where
    selectMember :: Hasql.Statement (ConvId, Domain, UserId) (Maybe (ConvId, Domain, UserId, RoleName))
    selectMember =
      dimapPG
        [maybeStatement|SELECT (conv :: uuid), (user_remote_domain :: text), (user_remote_id :: uuid), (conversation_role :: text)
                         FROM member_remote_user
                         WHERE user_remote_domain = ($2 :: text)
                         AND user_remote_id = ($3 :: uuid)
                         AND (conv = ($1 :: uuid)
                                OR conv IN (SELECT parent_conv FROM conversation WHERE id = ($1 :: uuid)))
                         ORDER BY CASE
                           WHEN conv = ($1 :: uuid) THEN 1
                           ELSE 2
                           END
                         LIMIT 1
                        |]

getRemoteMembersImpl :: (PGConstraints r) => ConvId -> Sem r [RemoteMember]
getRemoteMembersImpl convId = do
  rows <- runStatement convId selectMembers
  pure . map snd $ nubBy ((==) `on` ((.id_) . snd)) (V.toList (mkRemoteMember <$> rows))
  where
    selectMembers :: Hasql.Statement ConvId (Vector (ConvId, Domain, UserId, RoleName))
    selectMembers =
      dimapPG
        [vectorStatement|SELECT (conv :: uuid), (user_remote_domain :: text), (user_remote_id :: uuid), (conversation_role :: text)
                         FROM member_remote_user
                         WHERE (conv = ($1 :: uuid)
                                OR conv IN (SELECT parent_conv FROM conversation WHERE id = ($1 :: uuid)))
                         ORDER BY CASE
                           WHEN conv = ($1 :: uuid) THEN 1
                           ELSE 2
                           END
                        |]

mkRemoteMember :: (ConvId, Domain, UserId, RoleName) -> (ConvId, RemoteMember)
mkRemoteMember (convId, domain, uid, role) =
  ( convId,
    RemoteMember
      { id_ = toRemoteUnsafe domain uid,
        convRoleName = role
      }
  )

checkLocalMemberRemoteConvImpl :: (PGConstraints r) => UserId -> Remote ConvId -> Sem r Bool
checkLocalMemberRemoteConvImpl uid (tUntagged -> Qualified convId domain) =
  runStatement (domain, convId, uid) select
  where
    select :: Hasql.Statement (Domain, ConvId, UserId) Bool
    select =
      lmapPG
        [singletonStatement|SELECT EXISTS(
                              SELECT 1 FROM remote_conversation_local_member
                              WHERE domain = ($1 :: text)
                              AND conv_remote_domain = ($2 :: uuid)
                              AND conv_remote_id = ($3 :: uuid)
                            ) :: boolean
                           |]

selectRemoteMembersImpl :: [UserId] -> Remote ConvId -> Sem r ([UserId], Bool)
selectRemoteMembersImpl = undefined

setSelfMemberImpl :: (PGConstraints r) => Qualified ConvId -> Local UserId -> MemberUpdate -> Sem r ()
setSelfMemberImpl qcnv lusr =
  foldQualified
    lusr
    (setSelfMemberLocalConv (tUnqualified lusr) . tUnqualified)
    (setSelfMemberRemoteConv (tUnqualified lusr))
    qcnv

setSelfMemberLocalConv :: (PGConstraints r) => UserId -> ConvId -> MemberUpdate -> Sem r ()
setSelfMemberLocalConv uid cid MemberUpdate {..} =
  runStatement
    (uid, cid, mupOtrMuteStatus, mupOtrMuteRef, mupOtrArchive, mupOtrArchiveRef, mupHidden, mupHiddenRef)
    update
  where
    update :: Hasql.Statement (UserId, ConvId, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation_member
                             SET otr_muted_status = COALESCE($3 :: integer?, otr_muted_status),
                                 otr_muted_ref =    COALESCE($4 :: text?,    otr_muted_ref),
                                 otr_archived =     COALESCE($5 :: boolean?, otr_archived),
                                 otr_archived_ref = COALESCE($6 :: text?,    otr_archived_ref),
                                 hidden =           COALESCE($7 :: boolean?, hidden),
                                 hidden_ref =       COALESCE($8 :: text?,    hidden_ref)
                             WHERE "user" = ($1 :: uuid)
                             AND conv = ($2 :: uuid)
                            |]

setSelfMemberRemoteConv :: (PGConstraints r) => UserId -> Remote ConvId -> MemberUpdate -> Sem r ()
setSelfMemberRemoteConv uid (tUntagged -> Qualified cid domain) MemberUpdate {..} =
  runStatement
    (uid, domain, cid, mupOtrMuteStatus, mupOtrMuteRef, mupOtrArchive, mupOtrArchiveRef, mupHidden, mupHiddenRef)
    update
  where
    update :: Hasql.Statement (UserId, Domain, ConvId, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE remote_conversation_local_member
                             SET otr_muted_status = COALESCE($4 :: integer?, otr_muted_status),
                                 otr_muted_ref =    COALESCE($5 :: text?,    otr_muted_ref),
                                 otr_archived =     COALESCE($6 :: boolean?, otr_archived),
                                 otr_archived_ref = COALESCE($7 :: text?,    otr_archived_ref),
                                 hidden =           COALESCE($8 :: boolean?, hidden),
                                 hidden_ref =       COALESCE($9 :: text?,    hidden_ref)
                             WHERE "user" = ($1 :: uuid)
                             AND conv_remote_domain = ($2 :: text)
                             AND conv_remote_id = ($3 :: uuid)
                            |]

setOtherMemberImpl :: (PGConstraints r) => Local ConvId -> Qualified UserId -> OtherMemberUpdate -> Sem r ()
setOtherMemberImpl lcnv =
  foldQualified
    lcnv
    (setOtherLocalMember (tUnqualified lcnv) . tUnqualified)
    (setOtherRemoteMember (tUnqualified lcnv))

setOtherLocalMember :: (PGConstraints r) => ConvId -> UserId -> OtherMemberUpdate -> Sem r ()
setOtherLocalMember cid uid upd =
  for_ upd.omuConvRoleName $ \newRole ->
    runStatement (cid, uid, newRole) update
  where
    update :: Hasql.Statement (ConvId, UserId, RoleName) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation_member
                             SET conversation_role = ($3 :: text)
                             WHERE conv = ($1 :: uuid)
                             AND user = ($2 :: uuid)
                            |]

setOtherRemoteMember :: (PGConstraints r) => ConvId -> Remote UserId -> OtherMemberUpdate -> Sem r ()
setOtherRemoteMember cid (tUntagged -> Qualified uid domain) upd =
  for_ upd.omuConvRoleName $ \newRole ->
    runStatement (cid, domain, uid, newRole) update
  where
    update :: Hasql.Statement (ConvId, Domain, UserId, RoleName) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE local_conversation_remote_member
                             SET conversation_role = ($4 :: text)
                             WHERE conv = ($1 :: uuid)
                             AND user_remote_domain = ($2 :: text)
                             AND user_remote_id = ($3 :: uuid)
                            |]

deleteMembersImpl :: (PGConstraints r) => ConvId -> UserList UserId -> Sem r ()
deleteMembersImpl cid users =
  runTransaction ReadCommitted Write $ do
    Transaction.statement (cid, users.ulLocals) deleteLocalsStmt
    for_ (bucketRemote users.ulRemotes) $ \(tUntagged -> Qualified remotes domain) ->
      Transaction.statement (cid, domain, remotes) deleteRemotesStmt
  where
    deleteLocalsStmt :: Hasql.Statement (ConvId, [UserId]) ()
    deleteLocalsStmt =
      lmapPG @_ @(_, Vector _)
        [resultlessStatement|DELETE FROM conversation_member
                             WHERE conv = ($1 :: uuid)
                             AND users = ANY ($2 :: uuid[])
                            |]

    -- TODO: make this able to delete all remote users at once
    deleteRemotesStmt :: Hasql.Statement (ConvId, Domain, [UserId]) ()
    deleteRemotesStmt =
      lmapPG @_ @(_, _, Vector _)
        [resultlessStatement|DELETE FROM local_conversation_remote_member
                             WHERE conv = ($1 :: uuid)
                             AND user_remote_domain = ($2 :: text)
                             AND user_remote_id = ANY ($3 :: uuid[])
                            |]

deleteMembersInRemoteConversationImpl :: (PGConstraints r) => Remote ConvId -> [UserId] -> Sem r ()
deleteMembersInRemoteConversationImpl (tUntagged -> Qualified cid domain) uids =
  runStatement (domain, cid, uids) delete
  where
    delete :: Hasql.Statement (Domain, ConvId, [UserId]) ()
    delete =
      lmapPG @_ @(_, _, Vector _)
        [resultlessStatement|DELETE FROM remote_conversation_local_member
                             WHERE conv_remote_domain = ($1 :: text)
                             AND conv_remote_id = ($2 :: uuid)
                             AND "user" = ($3 ::uuid[])
                            |]

addMLSClientsImpl :: GroupId -> Qualified UserId -> Set (ClientId, LeafIndex) -> Sem r ()
addMLSClientsImpl = undefined

planClientRemovalImpl :: (Foldable f) => GroupId -> f ClientIdentity -> Sem r ()
planClientRemovalImpl = undefined

removeMLSClientsImpl :: GroupId -> Qualified UserId -> Set ClientId -> Sem r ()
removeMLSClientsImpl = undefined

removeAllMLSClientsImpl :: GroupId -> Sem r ()
removeAllMLSClientsImpl = undefined

lookupMLSClientsImpl :: GroupId -> Sem r (ClientMap LeafIndex)
lookupMLSClientsImpl = undefined

lookupMLSClientLeafIndicesImpl :: GroupId -> Sem r (ClientMap LeafIndex, IndexMap)
lookupMLSClientLeafIndicesImpl = undefined

getRemoteMembersByDomainImpl :: Domain -> Sem r [(ConvId, RemoteMember)]
getRemoteMembersByDomainImpl = undefined

getLocalMembersByDomainImpl :: Domain -> Sem r [(ConvId, UserId)]
getLocalMembersByDomainImpl = undefined

-- SUB CONVERSATION OPERATIONS
createSubConversationImpl :: ConvId -> SubConvId -> GroupId -> Sem r SubConversation
createSubConversationImpl = undefined

getSubConversationImpl :: ConvId -> SubConvId -> Sem r (Maybe SubConversation)
getSubConversationImpl = undefined

getSubConversationGroupInfoImpl :: ConvId -> SubConvId -> Sem r (Maybe GroupInfoData)
getSubConversationGroupInfoImpl = undefined

getSubConversationEpochImpl :: ConvId -> SubConvId -> Sem r (Maybe Epoch)
getSubConversationEpochImpl = undefined

setSubConversationGroupInfoImpl :: ConvId -> SubConvId -> Maybe GroupInfoData -> Sem r ()
setSubConversationGroupInfoImpl = undefined

setSubConversationEpochImpl :: ConvId -> SubConvId -> Epoch -> Sem r ()
setSubConversationEpochImpl = undefined

setSubConversationCipherSuiteImpl :: ConvId -> SubConvId -> CipherSuiteTag -> Sem r ()
setSubConversationCipherSuiteImpl = undefined

listSubConversationsImpl :: ConvId -> Sem r (Map SubConvId ConversationMLSData)
listSubConversationsImpl = undefined

deleteSubConversationImpl :: ConvId -> SubConvId -> Sem r ()
deleteSubConversationImpl = undefined
