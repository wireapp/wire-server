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

interpretConversationStoreToPostgres :: (Member (Input Hasql.Pool) r, Member (Embed IO) r, Member (Error Hasql.UsageError) r) => InterpreterFor ConversationStore r
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
  UpdateToMixedProtocol cid ct -> updateToMixedProtocolImpl cid ct
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
  GetAllLocalMembers -> getAllLocalMembersImpl
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

createConversationImpl ::
  ( Member (Input Hasql.Pool) r,
    Member (Error Hasql.UsageError) r,
    Member (Embed IO) r
  ) =>
  Local ConvId ->
  NewConversation ->
  Sem r StoredConversation
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

setConversationTypeImpl :: ConvId -> ConvType -> Sem r ()
setConversationTypeImpl = undefined

setConversationNameImpl :: ConvId -> Range 1 256 Text -> Sem r ()
setConversationNameImpl = undefined

setConversationAccessImpl :: ConvId -> ConversationAccessData -> Sem r ()
setConversationAccessImpl = undefined

setConversationReceiptModeImpl :: ConvId -> ReceiptMode -> Sem r ()
setConversationReceiptModeImpl = undefined

setConversationMessageTimerImpl :: ConvId -> Maybe Milliseconds -> Sem r ()
setConversationMessageTimerImpl = undefined

setConversationEpochImpl :: ConvId -> Epoch -> Sem r ()
setConversationEpochImpl = undefined

setConversationCipherSuiteImpl :: ConvId -> CipherSuiteTag -> Sem r ()
setConversationCipherSuiteImpl = undefined

setConversationCellsStateImpl :: ConvId -> CellsState -> Sem r ()
setConversationCellsStateImpl = undefined

resetConversationImpl :: ConvId -> GroupId -> Sem r ()
resetConversationImpl = undefined

setGroupInfoImpl :: ConvId -> GroupInfoData -> Sem r ()
setGroupInfoImpl = undefined

updateChannelAddPermissionsImpl :: ConvId -> AddPermission -> Sem r ()
updateChannelAddPermissionsImpl = undefined

updateToMixedProtocolImpl :: Local ConvId -> ConvType -> Sem r ()
updateToMixedProtocolImpl = undefined

updateToMLSProtocolImpl :: Local ConvId -> Sem r ()
updateToMLSProtocolImpl = undefined

deleteTeamConversationImpl :: TeamId -> ConvId -> Sem r ()
deleteTeamConversationImpl = undefined

getTeamConversationImpl :: TeamId -> ConvId -> Sem r (Maybe ConvId)
getTeamConversationImpl = undefined

getTeamConversationsImpl :: TeamId -> Sem r [ConvId]
getTeamConversationsImpl = undefined

deleteTeamConversationsImpl :: TeamId -> Sem r ()
deleteTeamConversationsImpl = undefined

-- MEMBER OPERATIONS
createMembersImpl ::
  (Member (Input Hasql.Pool) r, Member (Embed IO) r, Member (Error Hasql.UsageError) r) =>
  ConvId ->
  UserList (UserId, RoleName) ->
  Sem r ([LocalMember], [RemoteMember])
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

createMembersInRemoteConversationImpl :: Remote ConvId -> [UserId] -> Sem r ()
createMembersInRemoteConversationImpl = undefined

createBotMemberImpl :: ServiceRef -> BotId -> ConvId -> Sem r BotMember
createBotMemberImpl = undefined

getLocalMemberImpl :: ConvId -> UserId -> Sem r (Maybe LocalMember)
getLocalMemberImpl = undefined

getLocalMembersImpl :: (Member (Input Hasql.Pool) r, Member (Embed IO) r, Member (Error Hasql.UsageError) r) => ConvId -> Sem r [LocalMember]
getLocalMembersImpl convId = do
  rows <- runStatement convId selectMembers
  pure . map snd $ nubBy ((==) `on` ((.id_) . snd)) (V.toList (mkMember <$> rows))
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
    mkMember :: (ConvId, UserId, Maybe ServiceId, Maybe ProviderId, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text, Maybe RoleName) -> (ConvId, LocalMember)
    mkMember (cid, uid, mServiceId, mProviderId, msOtrMutedStatus, msOtrMutedRef, archived, msOtrArchivedRef, hidden, msHiddenRef, mRole) =
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

getAllLocalMembersImpl :: Sem r [LocalMember]
getAllLocalMembersImpl = undefined

getRemoteMemberImpl :: ConvId -> Remote UserId -> Sem r (Maybe RemoteMember)
getRemoteMemberImpl = undefined

getRemoteMembersImpl :: ConvId -> Sem r [RemoteMember]
getRemoteMembersImpl = undefined

checkLocalMemberRemoteConvImpl :: UserId -> Remote ConvId -> Sem r Bool
checkLocalMemberRemoteConvImpl = undefined

selectRemoteMembersImpl :: [UserId] -> Remote ConvId -> Sem r ([UserId], Bool)
selectRemoteMembersImpl = undefined

setSelfMemberImpl :: Qualified ConvId -> Local UserId -> MemberUpdate -> Sem r ()
setSelfMemberImpl = undefined

setOtherMemberImpl :: Local ConvId -> Qualified UserId -> OtherMemberUpdate -> Sem r ()
setOtherMemberImpl = undefined

deleteMembersImpl :: ConvId -> UserList UserId -> Sem r ()
deleteMembersImpl = undefined

deleteMembersInRemoteConversationImpl :: Remote ConvId -> [UserId] -> Sem r ()
deleteMembersInRemoteConversationImpl = undefined

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
