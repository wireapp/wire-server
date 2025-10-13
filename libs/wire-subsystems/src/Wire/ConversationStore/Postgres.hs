{-# LANGUAGE RecordWildCards #-}

module Wire.ConversationStore.Postgres where

import Control.Error (lastMay)
import Control.Monad.Trans.Maybe
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Domain
import Data.Id
import Data.Map qualified as Map
import Data.Misc
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Data.Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Records (HasField)
import Hasql.Pipeline qualified as Pipeline
import Hasql.Pool qualified as Hasql
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Hasql.Transaction (Transaction)
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions (IsolationLevel (ReadCommitted), Mode (..))
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
import Wire.API.Routes.MultiTablePaging
import Wire.ConversationStore
import Wire.ConversationStore.MLS.Types
import Wire.Postgres
import Wire.Sem.Paging.Cassandra
import Wire.StoredConversation
import Wire.UserList

type PGConstraints r =
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Error Hasql.UsageError) r
  )

interpretConversationStoreToPostgres :: (PGConstraints r) => InterpreterFor ConversationStore r
interpretConversationStoreToPostgres = interpret $ \case
  UpsertConversation lcnv nc -> upsertConversationImpl lcnv nc
  GetConversation cid -> getConversationImpl cid
  GetConversationEpoch cid -> getConversationEpochImpl cid
  GetConversations cids -> getConversationsImpl cids
  GetLocalConversationIds uid lastConvId maxIds -> getLocalConversationIdsImpl uid lastConvId maxIds
  GetConversationIds uid maxIds pagingState -> getConversationIdsImpl uid maxIds pagingState
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
  UpsertMembers cid ul -> upsertMembersImpl cid ul
  UpsertMembersInRemoteConversation rcid uids -> upsertMembersInRemoteConversationImpl rcid uids
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
  UpsertSubConversation convId subConvId groupId -> createSubConversationImpl convId subConvId groupId
  GetSubConversation convId subConvId -> getSubConversationImpl convId subConvId
  GetSubConversationGroupInfo convId subConvId -> getSubConversationGroupInfoImpl convId subConvId
  GetSubConversationEpoch convId subConvId -> getSubConversationEpochImpl convId subConvId
  SetSubConversationGroupInfo convId subConvId mPgs -> setSubConversationGroupInfoImpl convId subConvId mPgs
  SetSubConversationEpoch cid sconv epoch -> setSubConversationEpochImpl cid sconv epoch
  SetSubConversationCipherSuite cid sconv cs -> setSubConversationCipherSuiteImpl cid sconv cs
  ListSubConversations cid -> listSubConversationsImpl cid
  DeleteSubConversation convId subConvId -> deleteSubConversationImpl convId subConvId

upsertConversationImpl :: (PGConstraints r) => Local ConvId -> NewConversation -> Sem r StoredConversation
upsertConversationImpl lcnv nc = do
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
    upsertMembersTransaction storedConv.id_ $ UserList localUsers remoteUsers
  pure storedConv
  where
    insertConvStatement =
      lmapPG @_ @(_, _, _, Vector Int32, Vector Int32, _, _, _, _, _, _, _, _, _, _)
        [resultlessStatement|INSERT INTO conversation
                             (id, type, creator, access, access_roles_v2,
                              name, team, message_timer, receipt_mode, protocol,
                              group_id, group_conv_type, channel_add_permission, cells_state, parent_conv)
                             VALUES
                             ($1 :: uuid, $2 :: integer, $3 :: uuid?, $4 :: integer[], $5 :: integer[],
                              $6 :: text?, $7 :: uuid?, $8 :: bigint?, $9 :: integer?, $10 :: integer,
                              $11 :: bytea?, $12 ::integer?, $13 :: integer?, $14 :: integer, $15 :: uuid?)
                             ON CONFLICT (id)
                             DO UPDATE
                                SET type = ($2 :: integer),
                                    creator = ($3 :: uuid?),
                                    access = ($4 :: integer[]),
                                    access_roles_v2 = ($5 :: integer[]),
                                    name = ($6 :: text?),
                                    team = ($7 :: uuid?),
                                    message_timer = ($8 :: bigint?),
                                    receipt_mode =  ($9 :: integer?),
                                    protocol = ($10 :: integer),
                                    group_id =  ($11 :: bytea?),
                                    group_conv_type =  ($12 :: integer?),
                                    channel_add_permission =  ($13 :: integer?),
                                    cells_state =  ($14 :: integer),
                                    parent_conv =  ($15 :: uuid?)
                            |]

deleteConversationImpl :: (PGConstraints r) => ConvId -> Sem r ()
deleteConversationImpl cid =
  runStatement cid delete
  where
    delete :: Hasql.Statement ConvId ()
    delete =
      -- cascades to shadow convs, subconvs, local and remote members
      lmapPG
        [resultlessStatement|DELETE FROM conversation
                             WHERE id = ($1 :: uuid)
                            |]

getConversationImpl :: (PGConstraints r) => ConvId -> Sem r (Maybe StoredConversation)
getConversationImpl cid =
  runTransaction ReadCommitted Read $ do
    mConvRow <- Transaction.statement cid selectConvMetadata
    case mConvRow of
      Nothing -> pure Nothing
      Just convRow -> do
        localMembers <- Transaction.statement cid selectLocalMembersStmt
        remoteMembers <- Transaction.statement cid selectRemoteMembersStmt
        pure $ toConv cid localMembers remoteMembers (Just convRow)

selectConvMetadata :: Hasql.Statement (ConvId) (Maybe ConvRow)
selectConvMetadata =
  dimapPG @_ @_
    @(Maybe (_, _, Maybe (Vector _), Maybe (Vector _), _, _, _, _, _, _, _, _, _, _, _, _, _))
    @(Maybe ConvRow)
    [maybeStatement|SELECT (type :: integer), (creator :: uuid?), (access :: integer[]?), (access_roles_v2 :: integer[]?),
                           (name :: text?), (team :: uuid?), (message_timer :: bigint?), (receipt_mode :: integer?), (protocol :: integer?),
                           (group_id :: bytea?), (epoch :: bigint?), (epoch_timestamp :: timestamptz?), (cipher_suite :: integer?),
                           (group_conv_type :: integer?), (channel_add_permission :: integer?), (cells_state :: integer?), (parent_conv :: uuid?)
                    FROM conversation
                    WHERE id = ($1 :: uuid)
                   |]

getConversationEpochImpl :: (PGConstraints r) => ConvId -> Sem r (Maybe Epoch)
getConversationEpochImpl cid = do
  join <$> runStatement cid select
  where
    select :: Hasql.Statement (ConvId) (Maybe (Maybe Epoch))
    select =
      dimapPG
        [maybeStatement|SELECT (epoch :: bigint?)
                        FROM conversation
                        WHERE id = ($1 :: uuid) |]

getConversationsImpl :: (PGConstraints r) => [ConvId] -> Sem r [StoredConversation]
getConversationsImpl cids = do
  (convRowsWithId, localMemRows, remoteMemRows) <-
    runPipeline $
      (,,)
        <$> Pipeline.statement cids selectMetadata
        <*> Pipeline.statement cids selectAllLocalMembers
        <*> Pipeline.statement cids selectAllRemoteMembers
  let convRowMap = Map.fromList $ map splitIdFromRow convRowsWithId
      localsWithConvId = mkLocalMember <$> localMemRows
      remotesWithConvId = mkRemoteMember <$> remoteMemRows
      -- Here we cannot loop over `convRowsWithId` to do this because the result
      -- is expected to be in same order as `cids` but the expressing that in
      -- the SQL query is tough.
      mConvs = flip map cids $ \convId -> do
        convRow@(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, mParent) <- Map.lookup convId convRowMap
        let localMems = findMembers convId mParent localsWithConvId
            remoteMems = findMembers convId mParent remotesWithConvId
        toConv convId localMems remoteMems (Just convRow)
  pure $ catMaybes mConvs
  where
    selectMetadata :: Hasql.Statement [ConvId] [ConvRowWithId]
    selectMetadata =
      dimapPG @[_] @(Vector _)
        @(Vector (_, _, _, Maybe (Vector _), Maybe (Vector _), _, _, _, _, _, _, _, _, _, _, _, _, _))
        @[ConvRowWithId]
        [vectorStatement|SELECT (id :: uuid), (type :: integer), (creator :: uuid?), (access :: integer[]?), (access_roles_v2 :: integer[]?),
                                (name :: text?), (team :: uuid?), (message_timer :: bigint?), (receipt_mode :: integer?), (protocol :: integer?),
                                (group_id :: bytea?), (epoch :: bigint?), (epoch_timestamp :: timestamptz?), (cipher_suite :: integer?),
                                (group_conv_type :: integer?), (channel_add_permission :: integer?), (cells_state :: integer?), (parent_conv :: uuid?)
                         FROM conversation
                         WHERE id = ANY($1 :: uuid[])
                        |]
    selectAllLocalMembers :: Hasql.Statement [ConvId] [LocalMemberRow]
    selectAllLocalMembers =
      dimapPG @[_] @(Vector _)
        [vectorStatement|SELECT (conv :: uuid), ("user" :: uuid), (service :: uuid?), (provider :: uuid?), (otr_muted_status :: integer?), (otr_muted_ref :: text?),
                                (otr_archived :: boolean?), (otr_archived_ref :: text?), (hidden :: boolean?), (hidden_ref :: text?), (conversation_role :: text?)
                         FROM conversation_member
                         WHERE conv = ANY ($1 :: uuid[])
                         OR conv IN (SELECT parent_conv FROM conversation WHERE id = ANY ($1 :: uuid[]))
                        |]
    selectAllRemoteMembers :: Hasql.Statement [ConvId] [RemoteMemberRow]
    selectAllRemoteMembers =
      dimapPG @[_] @(Vector _)
        [vectorStatement|SELECT (conv :: uuid), (user_remote_domain :: text), (user_remote_id :: uuid), (conversation_role :: text)
                         FROM local_conversation_remote_member
                         WHERE conv = ANY ($1 :: uuid[])
                         OR conv IN (SELECT parent_conv FROM conversation WHERE id = ANY ($1 :: uuid[]))
                        |]

    findMembers :: (HasField "id_" a b, Eq b) => ConvId -> Maybe ConvId -> [(ConvId, a)] -> [a]
    findMembers convId parentConvId allMembersWithConvId =
      let localMemsDirect = map snd $ filter (\(memConvId, _) -> memConvId == convId) allMembersWithConvId
          localMemsParent = map snd $ filter (\(memConvId, _) -> Just memConvId == parentConvId) allMembersWithConvId
       in nubBy ((==) `on` (.id_)) $ localMemsDirect <> localMemsParent

getLocalConversationIdsImpl :: (PGConstraints r) => UserId -> Maybe ConvId -> Range 1 1000 Int32 -> Sem r (ResultSet ConvId)
getLocalConversationIdsImpl usr start (fromRange -> maxIds) = do
  mkResultSetByLength (fromIntegral maxIds) <$> case start of
    Just c -> runStatement (usr, c, maxIds + 1) selectFrom
    Nothing -> runStatement (usr, maxIds + 1) selectStart
  where
    selectStart :: Hasql.Statement (UserId, Int32) [ConvId]
    selectStart =
      dimapPG
        [vectorStatement|SELECT (conv :: uuid)
                         FROM conversation_member
                         WHERE "user" = ($1 :: uuid)
                         ORDER BY conv
                         LIMIT ($2 :: integer)
                        |]

    selectFrom :: Hasql.Statement (UserId, ConvId, Int32) [ConvId]
    selectFrom =
      dimapPG
        [vectorStatement|SELECT (conv :: uuid)
                         FROM conversation_member
                         WHERE "user" = ($1 :: uuid)
                         AND conv > ($2 :: uuid)
                         ORDER BY conv
                         LIMIT ($3 :: integer)
                        |]

getConversationIdsImpl :: forall r. (PGConstraints r) => Local UserId -> Range 1 1000 Int32 -> Maybe ConversationPagingState -> Sem r ConvIdsPage
getConversationIdsImpl lusr (fromRange -> maxIds) pagingState = do
  let pagingTable = maybe PagingLocals (.mtpsTable) pagingState
      mLastId = Aeson.decode . BS.fromStrict =<< (.mtpsState) =<< pagingState
  case pagingTable of
    PagingLocals -> do
      localPage <- getLocals maxIds mLastId
      let remainingSize = maxIds - fromIntegral (length localPage.mtpResults)
      if remainingSize <= 0
        then pure localPage {mtpHasMore = True}
        else do
          remotePage <- getRemotes remainingSize Nothing
          pure $
            remotePage {mtpResults = localPage.mtpResults <> remotePage.mtpResults}
    PagingRemotes ->
      getRemotes maxIds mLastId
  where
    localDomain = tDomain lusr
    usr = tUnqualified lusr

    getLocals :: Int32 -> Maybe (Qualified ConvId) -> Sem r ConvIdsPage
    getLocals maxLocals mLastId = do
      mkLocalsPage <$> case mLastId of
        Nothing -> runStatement (usr, maxLocals) selectLocalsStart
        Just (Qualified lastId _) -> runStatement (usr, lastId, maxIds) selectLocalsFrom

    getRemotes :: Int32 -> Maybe (Qualified ConvId) -> Sem r ConvIdsPage
    getRemotes maxRemotes mLastId = do
      mkRemotesPage maxRemotes <$> case mLastId of
        Nothing -> runStatement (usr, maxRemotes) selectRemotesStart
        Just (Qualified lastId lastDomain) -> runStatement (usr, lastDomain, lastId, maxRemotes) selectRemotesFrom

    mkLocalsPage :: [ConvId] -> ConvIdsPage
    mkLocalsPage results =
      MultiTablePage
        { mtpResults = map (\cid -> Qualified cid localDomain) results,
          mtpHasMore = length results >= fromIntegral maxIds,
          mtpPagingState =
            case lastMay results of
              Nothing ->
                MultiTablePagingState
                  { mtpsTable = PagingRemotes,
                    mtpsState = Nothing
                  }
              Just newLastId ->
                MultiTablePagingState
                  { mtpsTable = PagingLocals,
                    mtpsState = Just . BS.toStrict . Aeson.encode $ Qualified newLastId localDomain
                  }
        }

    mkRemotesPage :: Int32 -> [(Domain, ConvId)] -> ConvIdsPage
    mkRemotesPage maxRemotes results =
      MultiTablePage
        { mtpResults = map (uncurry $ flip Qualified) results,
          mtpHasMore = length results >= fromIntegral maxRemotes,
          mtpPagingState =
            case lastMay results of
              Nothing ->
                -- This might look absurd because when this state is back here,
                -- we'll go to the first page, but 'mtpHasMore' should be set to
                -- false when we have empty results.
                MultiTablePagingState
                  { mtpsTable = PagingRemotes,
                    mtpsState = Nothing
                  }
              Just (newLastDomain, newLastId) ->
                MultiTablePagingState
                  { mtpsTable = PagingRemotes,
                    mtpsState = Just . BS.toStrict $ Aeson.encode $ Qualified newLastId newLastDomain
                  }
        }

    selectLocalsFrom :: Hasql.Statement (UserId, ConvId, Int32) [ConvId]
    selectLocalsFrom =
      dimapPG
        [vectorStatement|SELECT (conv :: uuid)
                         FROM conversation_member
                         WHERE "user" = ($1 :: uuid)
                         AND conv > ($2 :: uuid)
                         ORDER BY conv
                         LIMIT ($3 :: integer)
                        |]
    selectLocalsStart :: Hasql.Statement (UserId, Int32) [(ConvId)]
    selectLocalsStart =
      dimapPG
        [vectorStatement|SELECT (conv :: uuid)
                         FROM conversation_member
                         WHERE "user" = ($1 :: uuid)
                         ORDER BY conv
                         LIMIT ($2 :: integer)
                        |]

    selectRemotesFrom :: Hasql.Statement (UserId, Domain, ConvId, Int32) [(Domain, ConvId)]
    selectRemotesFrom =
      dimapPG
        [vectorStatement|SELECT (conv_remote_domain :: text), (conv_remote_id :: uuid)
                         FROM remote_conversation_local_member
                         WHERE "user" = ($1 :: uuid)
                         AND (conv_remote_domain, conv_remote_id) > ($2 :: text, $3 ::uuid)
                         ORDER BY (conv_remote_domain, conv_remote_id)
                         LIMIT ($4 :: integer)
                        |]
    selectRemotesStart :: Hasql.Statement (UserId, Int32) [(Domain, ConvId)]
    selectRemotesStart =
      dimapPG
        [vectorStatement|SELECT (conv_remote_domain :: text), (conv_remote_id :: uuid)
                         FROM remote_conversation_local_member
                         WHERE "user" = ($1 :: uuid)
                         ORDER BY (conv_remote_domain, conv_remote_id)
                         LIMIT ($2 :: integer)
                        |]

getConversationMetadataImpl :: (PGConstraints r) => ConvId -> Sem r (Maybe ConversationMetadata)
getConversationMetadataImpl cid =
  toConvMeta <$$> runStatement cid selectConvMetadata

getGroupInfoImpl :: (PGConstraints r) => ConvId -> Sem r (Maybe GroupInfoData)
getGroupInfoImpl cid =
  join <$> runStatement cid select
  where
    select :: Hasql.Statement ConvId (Maybe (Maybe GroupInfoData))
    select =
      dimapPG
        [maybeStatement|SELECT (public_group_state :: bytea?) FROM conversation where id = ($1 :: uuid)|]

isConversationAliveImpl :: (PGConstraints r) => ConvId -> Sem r Bool
isConversationAliveImpl cid =
  runStatement cid select
  where
    select :: Hasql.Statement ConvId Bool
    select =
      lmapPG
        [singletonStatement|SELECT EXISTS (SELECT 1 FROM conversation WHERE id = ($1 :: uuid)) :: boolean|]

getRemoteConversationStatusImpl :: (PGConstraints r) => UserId -> [Remote ConvId] -> Sem r (Map (Remote ConvId) MemberStatus)
getRemoteConversationStatusImpl uid remoteConvs = do
  fmap Map.unions . runPipeline $
    for (bucketRemote remoteConvs) $ \(tUntagged -> Qualified cids domain) ->
      let mkMap (convId, msOtrMutedStatus, msOtrMutedRef, archived, msOtrArchivedRef, hidden, msHiddenRef) =
            Map.singleton
              (toRemoteUnsafe domain convId)
              ( MemberStatus
                  { msOtrArchived = fromMaybe False archived,
                    msHidden = fromMaybe False hidden,
                    ..
                  }
              )
       in Map.unions <$> (mkMap <$$> Pipeline.statement (uid, domain, cids) select)
  where
    select :: Hasql.Statement (UserId, Domain, [ConvId]) [(ConvId, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text)]
    select =
      dimapPG @_ @(_, _, Vector _)
        [vectorStatement|SELECT (conv_remote_id :: uuid),
                                (otr_muted_status :: integer?), (otr_muted_ref :: text?),
                                (otr_archived :: boolean?), (otr_archived_ref :: text?),
                                (hidden :: boolean?), (hidden_ref :: text?)
                        FROM remote_conversation_local_member
                        WHERE "user" = ($1 :: uuid)
                        AND conv_remote_domain = ($2 :: text)
                        AND conv_remote_id = ANY ($3 :: uuid[])
                        |]

selectConversationsImpl :: (PGConstraints r) => UserId -> [ConvId] -> Sem r [ConvId]
selectConversationsImpl uid cids =
  runStatement (uid, cids) select
  where
    select :: Hasql.Statement (UserId, [ConvId]) [ConvId]
    select =
      dimapPG @_ @(_, Vector _)
        [vectorStatement|SELECT (conv :: uuid) from conversation_member
                         WHERE "user" = ($1 :: uuid)
                         AND conv = ANY ($2 :: uuid[])
                         ORDER BY conv
                        |]

setConversationTypeImpl :: (PGConstraints r) => ConvId -> ConvType -> Sem r ()
setConversationTypeImpl convId typ =
  runStatement (convId, typ) update
  where
    update :: Hasql.Statement (ConvId, ConvType) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET type = ($2 :: integer)
                             WHERE id = ($1 :: uuid)|]

setConversationNameImpl :: (PGConstraints r) => ConvId -> Range 1 256 Text -> Sem r ()
setConversationNameImpl convId (fromRange -> name) =
  runStatement (convId, name) update
  where
    update :: Hasql.Statement (ConvId, Text) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET name = ($2 :: text)
                             WHERE id = ($1 :: uuid)|]

setConversationAccessImpl :: (PGConstraints r) => ConvId -> ConversationAccessData -> Sem r ()
setConversationAccessImpl convId accessData =
  runStatement (convId, accessData.cupAccess, accessData.cupAccessRoles) update
  where
    update :: Hasql.Statement (ConvId, Set Access, Set AccessRole) ()
    update =
      lmapPG @_ @(_, Vector _, Vector _)
        [resultlessStatement|UPDATE conversation
                             SET access = ($2 :: integer[]), access_roles_v2 = ($3 :: integer[])
                             WHERE id = ($1 :: uuid)|]

setConversationReceiptModeImpl :: (PGConstraints r) => ConvId -> ReceiptMode -> Sem r ()
setConversationReceiptModeImpl convId receiptMode =
  runStatement (convId, receiptMode) update
  where
    update :: Hasql.Statement (ConvId, ReceiptMode) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET receipt_mode = ($2 :: integer)
                             WHERE id = ($1 :: uuid)|]

setConversationMessageTimerImpl :: (PGConstraints r) => ConvId -> Maybe Milliseconds -> Sem r ()
setConversationMessageTimerImpl convId timer =
  runStatement (convId, timer) update
  where
    update :: Hasql.Statement (ConvId, Maybe Milliseconds) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET message_timer = ($2 :: bigint?)
                             WHERE id = ($1 :: uuid)|]

setConversationEpochImpl :: (PGConstraints r) => ConvId -> Epoch -> Sem r ()
setConversationEpochImpl convId epoch =
  runStatement (convId, epoch) update
  where
    update :: Hasql.Statement (ConvId, Epoch) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET epoch = ($2 :: bigint), epoch_timestamp = NOW()
                             WHERE id = ($1 :: uuid)|]

setConversationCipherSuiteImpl :: (PGConstraints r) => ConvId -> CipherSuiteTag -> Sem r ()
setConversationCipherSuiteImpl convId cs =
  runStatement (convId, cs) update
  where
    update :: Hasql.Statement (ConvId, CipherSuiteTag) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET cipher_suite = ($2 :: integer)
                             WHERE id = ($1 :: uuid)|]

setConversationCellsStateImpl :: (PGConstraints r) => ConvId -> CellsState -> Sem r ()
setConversationCellsStateImpl convId cells =
  runStatement (convId, cells) update
  where
    update :: Hasql.Statement (ConvId, CellsState) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET cells_state = ($2 :: integer)
                             WHERE id = ($1 :: uuid)|]

resetConversationImpl :: (PGConstraints r) => ConvId -> GroupId -> Sem r ()
resetConversationImpl convId groupId =
  runStatement (convId, groupId) update
  where
    update :: Hasql.Statement (ConvId, GroupId) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET group_id = ($2 :: bytea), epoch = 0, epoch_timestamp = NOW()
                             WHERE id = ($1 :: uuid)|]

setGroupInfoImpl :: (PGConstraints r) => ConvId -> GroupInfoData -> Sem r ()
setGroupInfoImpl convId groupInfo =
  runStatement (convId, groupInfo) update
  where
    update :: Hasql.Statement (ConvId, GroupInfoData) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET public_group_state = ($2 :: bytea)
                             WHERE id = ($1 :: uuid)|]

updateChannelAddPermissionsImpl :: (PGConstraints r) => ConvId -> AddPermission -> Sem r ()
updateChannelAddPermissionsImpl convId addPerm =
  runStatement (convId, addPerm) update
  where
    update :: Hasql.Statement (ConvId, AddPermission) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET channel_add_permission = ($2 :: integer)
                             WHERE id = ($1 :: uuid)|]

updateToMixedProtocolImpl :: (PGConstraints r) => ConvId -> GroupId -> Epoch -> Sem r ()
updateToMixedProtocolImpl convId gid epoch =
  runStatement (convId, ProtocolMixedTag, gid, epoch) update
  where
    update :: Hasql.Statement (ConvId, ProtocolTag, GroupId, Epoch) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET protocol = ($2 :: integer), group_id = ($3 :: bytea), epoch = ($4 :: bigint), epoch_timestamp = NOW()
                             WHERE id = ($1 :: uuid)|]

updateToMLSProtocolImpl :: (PGConstraints r) => ConvId -> Sem r ()
updateToMLSProtocolImpl convId =
  runStatement (convId, ProtocolMLSTag) update
  where
    update :: Hasql.Statement (ConvId, ProtocolTag) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE conversation
                             SET protocol = ($2 :: integer), receipt_mode = 0
                             WHERE id = ($1 :: uuid)|]

-- This doesn't check whether the conv belongs to the team because the cassandra
-- interpreter doesn't do that either.
deleteTeamConversationImpl :: (PGConstraints r) => TeamId -> ConvId -> Sem r ()
deleteTeamConversationImpl _ = deleteConversationImpl

getTeamConversationImpl :: (PGConstraints r) => TeamId -> ConvId -> Sem r (Maybe ConvId)
getTeamConversationImpl tid cid = runStatement (tid, cid) select
  where
    select :: Hasql.Statement (TeamId, ConvId) (Maybe ConvId)
    select =
      dimapPG
        [maybeStatement|SELECT (id :: uuid)
                        FROM conversation
                        WHERE team = ($1 :: uuid)
                        AND id = ($2 :: uuid)
                       |]

getTeamConversationsImpl :: (PGConstraints r) => TeamId -> Sem r [ConvId]
getTeamConversationsImpl tid =
  runStatement tid select
  where
    select :: Hasql.Statement TeamId [ConvId]
    select =
      dimapPG
        [vectorStatement|SELECT (id :: uuid)
                         FROM conversation
                         WHERE team = ($1 :: uuid)
                        |]

deleteTeamConversationsImpl :: (PGConstraints r) => TeamId -> Sem r ()
deleteTeamConversationsImpl tid =
  runStatement tid delete
  where
    delete :: Hasql.Statement TeamId ()
    delete =
      lmapPG
        [resultlessStatement|DELETE FROM conversation
                             WHERE team = ($1 :: uuid)
                            |]

-- MEMBER OPERATIONS
upsertMembersImpl :: (PGConstraints r) => ConvId -> UserList (UserId, RoleName) -> Sem r ([LocalMember], [RemoteMember])
upsertMembersImpl convId users@(UserList lusers rusers) = do
  runTransaction ReadCommitted Write $ upsertMembersTransaction convId users
  pure (map newMemberWithRole lusers, map newRemoteMemberWithRole rusers)

upsertMembersTransaction :: ConvId -> UserList (UserId, RoleName) -> Transaction ()
upsertMembersTransaction convId (UserList lusers rusers) = do
  for_ lusers $ \(u, r) ->
    Transaction.statement (convId, u, r) insertLocalStatement
  for_ rusers $ \(tUntagged -> Qualified (uid, role) domain) ->
    Transaction.statement (convId, domain, uid, role) insertRemoteStatement
  where
    insertLocalStatement :: Hasql.Statement (ConvId, UserId, RoleName) ()
    insertLocalStatement =
      lmapPG
        [resultlessStatement|INSERT INTO conversation_member (conv, "user", conversation_role)
                             VALUES ($1 :: uuid, $2 :: uuid, $3 :: text)
                             ON CONFLICT (conv, "user")
                             DO UPDATE SET conversation_role = ($3 :: text)
                             |]
    insertRemoteStatement :: Hasql.Statement (ConvId, Domain, UserId, RoleName) ()
    insertRemoteStatement =
      lmapPG
        [resultlessStatement|INSERT INTO local_conversation_remote_member (conv, user_remote_domain, user_remote_id, conversation_role)
                             VALUES ($1 :: uuid, $2 :: text, $3 :: uuid, $4 :: text)
                             ON CONFLICT (conv, user_remote_domain, user_remote_id)
                             DO UPDATE SET conversation_role = ($4 :: text)
                             |]

upsertMembersInRemoteConversationImpl :: (PGConstraints r) => Remote ConvId -> [UserId] -> Sem r ()
upsertMembersInRemoteConversationImpl (tUntagged -> Qualified cnv domain) users = do
  let domains = replicate (length users) domain
      cnvs = replicate (length users) cnv
  runStatement (users, domains, cnvs) upsert
  where
    upsert :: Hasql.Statement ([UserId], [Domain], [ConvId]) ()
    upsert =
      lmapPG @_ @(Vector _, Vector _, Vector _)
        [resultlessStatement|INSERT INTO remote_conversation_local_member ("user", conv_remote_domain, conv_remote_id)
                             SELECT * FROM UNNEST($1 :: uuid[], $2 :: text[], $3 :: uuid[])
                             ON CONFLICT ("user", conv_remote_domain, conv_remote_id) DO NOTHING
                             |]

createBotMemberImpl :: (PGConstraints r) => ServiceRef -> BotId -> ConvId -> Sem r BotMember
createBotMemberImpl serviceRef botId convId = do
  runStatement (convId, botId, serviceRef._serviceRefId, serviceRef._serviceRefProvider) insert
  pure . BotMember $ (newMember botId.botUserId) {service = Just serviceRef}
  where
    insert :: Hasql.Statement (ConvId, BotId, ServiceId, ProviderId) ()
    insert =
      lmapPG
        [resultlessStatement|INSERT INTO conversation_member (conv, "user", service, provider)
                             VALUES ($1 :: uuid, $2 :: uuid, $3 :: uuid, $4 :: uuid)
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
                        WHERE (conv = ($1 :: uuid)
                             OR conv IN (SELECT parent_conv FROM conversation WHERE id = ($1 :: uuid)))
                        AND "user" = ($2 :: uuid)
                        ORDER BY CASE
                          WHEN conv = ($1 :: uuid) THEN 1
                          ELSE 2
                          END
                        LIMIT 1
                       |]

getLocalMembersImpl :: (PGConstraints r) => ConvId -> Sem r [LocalMember]
getLocalMembersImpl convId =
  runStatement convId selectLocalMembersStmt

type LocalMemberRow = (ConvId, UserId, Maybe ServiceId, Maybe ProviderId, Maybe MutedStatus, Maybe Text, Maybe Bool, Maybe Text, Maybe Bool, Maybe Text, Maybe RoleName)

selectLocalMembersStmt :: Hasql.Statement ConvId [LocalMember]
selectLocalMembersStmt =
  dedupMembers <$> select
  where
    dedupMembers rows =
      let localMembers = mkLocalMember <$> rows
       in map snd $ nubBy ((==) `on` ((.id_) . snd)) localMembers

    select :: Hasql.Statement ConvId [LocalMemberRow]
    select =
      dimapPG
        [vectorStatement|SELECT (conv :: uuid), ("user" :: uuid), (service :: uuid?), (provider :: uuid?), (otr_muted_status :: integer?), (otr_muted_ref :: text?),
                                (otr_archived :: boolean?), (otr_archived_ref :: text?), (hidden :: boolean?), (hidden_ref :: text?), (conversation_role :: text?)
                         FROM conversation_member
                         WHERE conv = ($1 :: uuid)
                         OR    conv IN (SELECT parent_conv FROM conversation WHERE id = ($1 :: uuid))
                         ORDER BY CASE
                           WHEN conv = ($1 :: uuid) THEN 1
                           ELSE 2
                           END
                        |]

mkLocalMember :: LocalMemberRow -> (ConvId, LocalMember)
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

type RemoteMemberRow = (ConvId, Domain, UserId, RoleName)

getRemoteMemberImpl :: (PGConstraints r) => ConvId -> Remote UserId -> Sem r (Maybe RemoteMember)
getRemoteMemberImpl convId (tUntagged -> Qualified uid domain) =
  snd . mkRemoteMember <$$> runStatement (convId, domain, uid) selectMember
  where
    selectMember :: Hasql.Statement (ConvId, Domain, UserId) (Maybe RemoteMemberRow)
    selectMember =
      dimapPG
        [maybeStatement|SELECT (conv :: uuid), (user_remote_domain :: text), (user_remote_id :: uuid), (conversation_role :: text)
                         FROM local_conversation_remote_member
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
getRemoteMembersImpl convId =
  runStatement convId selectRemoteMembersStmt

selectRemoteMembersStmt :: Hasql.Statement ConvId [RemoteMember]
selectRemoteMembersStmt =
  dedupMembers <$> select
  where
    dedupMembers rows =
      let localMembers = mkRemoteMember <$> rows
       in map snd $ nubBy ((==) `on` ((.id_) . snd)) localMembers

    select :: Hasql.Statement ConvId [(ConvId, Domain, UserId, RoleName)]
    select =
      dimapPG
        [vectorStatement|SELECT (conv :: uuid), (user_remote_domain :: text), (user_remote_id :: uuid), (conversation_role :: text)
                         FROM local_conversation_remote_member
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
                              WHERE conv_remote_domain = ($1 :: text)
                              AND conv_remote_id = ($2 :: uuid)
                              AND "user" = ($3 :: uuid)
                            ) :: boolean
                           |]

selectRemoteMembersImpl :: (PGConstraints r) => [UserId] -> Remote ConvId -> Sem r ([UserId], Bool)
selectRemoteMembersImpl uids (tUntagged -> Qualified cid domain) = do
  foundUids <- runStatement (domain, cid, uids) select
  pure (foundUids, Set.fromList foundUids == Set.fromList uids)
  where
    select :: Hasql.Statement (Domain, ConvId, [UserId]) [UserId]
    select =
      dimapPG @_ @(_, _, Vector _)
        [vectorStatement|SELECT ("user" :: uuid)
                         FROM remote_conversation_local_member
                         WHERE conv_remote_domain = ($1 :: text)
                         AND conv_remote_id = ($2 :: uuid)
                         AND "user" = ANY ($3 :: uuid[])
                        |]

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
                             AND "user" = ($2 :: uuid)
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
                             AND "user" = ANY($2 :: uuid[])
                            |]

    deleteRemotesStmt :: Hasql.Statement (ConvId, Domain, [UserId]) ()
    deleteRemotesStmt =
      lmapPG @_ @(_, _, Vector _)
        [resultlessStatement|DELETE FROM local_conversation_remote_member
                             WHERE conv = ($1 :: uuid)
                             AND user_remote_domain = ($2 :: text)
                             AND user_remote_id = ANY($3 :: uuid[])
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
                             AND "user" = ANY ($3 ::uuid[])
                            |]

addMLSClientsImpl :: (PGConstraints r) => GroupId -> Qualified UserId -> Set (ClientId, LeafIndex) -> Sem r ()
addMLSClientsImpl gid (Qualified uid domain) clients =
  runPipeline $
    for_ (Set.toList clients) $
      \(cid, idx) ->
        Pipeline.statement (gid, domain, uid, cid, fromIntegral idx) insert
  where
    insert :: Hasql.Statement (GroupId, Domain, UserId, ClientId, Int32) ()
    insert =
      lmapPG
        [resultlessStatement|INSERT INTO mls_group_member_client
                             (group_id, user_domain, "user", client, leaf_node_index, removal_pending)
                             VALUES
                             ($1 :: bytea, $2 :: text, $3 :: uuid, $4 :: text, $5 :: integer, false)
                            |]

planClientRemovalImpl :: (PGConstraints r, Foldable f) => GroupId -> f ClientIdentity -> Sem r ()
planClientRemovalImpl gid clients =
  runPipeline $
    for_ clients $ \ClientIdentity {..} ->
      Pipeline.statement (gid, ciDomain, ciUser, ciClient) update
  where
    update :: Hasql.Statement (GroupId, Domain, UserId, ClientId) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE mls_group_member_client
                             SET removal_pending = true
                             WHERE group_id = ($1 :: bytea)
                             AND user_domain = ($2 :: text)
                             AND "user" = ($3 :: uuid)
                             AND client = ($4 :: text)
                            |]

removeMLSClientsImpl :: (PGConstraints r) => GroupId -> Qualified UserId -> Set ClientId -> Sem r ()
removeMLSClientsImpl gid (Qualified uid domain) cids =
  runPipeline $
    for_ cids $ \cid ->
      Pipeline.statement (gid, domain, uid, cid) delete
  where
    delete :: Hasql.Statement (GroupId, Domain, UserId, ClientId) ()
    delete =
      lmapPG
        [resultlessStatement|DELETE FROM mls_group_member_client
                             WHERE group_id = ($1 :: bytea)
                             AND user_domain = ($2 :: text)
                             AND "user" = ($3 :: uuid)
                             AND client = ($4 :: text)
                            |]

removeAllMLSClientsImpl :: (PGConstraints r) => GroupId -> Sem r ()
removeAllMLSClientsImpl gid =
  runStatement gid delete
  where
    delete :: Hasql.Statement GroupId ()
    delete =
      lmapPG
        [resultlessStatement|DELETE FROM mls_group_member_client
                             WHERE group_id = ($1 :: bytea)
                            |]

lookupMLSClientsImpl :: (PGConstraints r) => GroupId -> Sem r (ClientMap LeafIndex)
lookupMLSClientsImpl gid = do
  mkClientMap <$> runStatement gid selectMLSClients

selectMLSClients :: Hasql.Statement GroupId [(Domain, UserId, ClientId, Int32, Bool)]
selectMLSClients =
  dimapPG
    [vectorStatement|SELECT (user_domain :: text), ("user" :: uuid), (client :: text), (leaf_node_index :: integer), (removal_pending :: bool)
                     FROM mls_group_member_client
                     WHERE group_id = ($1 :: bytea)
                    |]

lookupMLSClientLeafIndicesImpl :: (PGConstraints r) => GroupId -> Sem r (ClientMap LeafIndex, IndexMap)
lookupMLSClientLeafIndicesImpl gid = do
  rows <- runStatement gid selectMLSClients
  pure (mkClientMap rows, mkIndexMap rows)

-- SUB CONVERSATION OPERATIONS
createSubConversationImpl :: (PGConstraints r) => ConvId -> SubConvId -> GroupId -> Sem r SubConversation
createSubConversationImpl cid subConvId gid = do
  runStatement (cid, subConvId, gid) insert
  pure $ newSubConversation cid subConvId gid
  where
    insert :: Hasql.Statement (ConvId, SubConvId, GroupId) ()
    insert =
      lmapPG
        [resultlessStatement|INSERT INTO subconversation
                             (conv_id, subconv_id, epoch, epoch_timestamp, group_id)
                             VALUES
                             ($1 :: uuid, $2 :: text, 0, NOW(), $3 :: bytea)
                             ON CONFLICT (conv_id, subconv_id)
                             DO UPDATE SET
                                epoch = 0,
                                epoch_timestamp = NOW(),
                                group_id = ($3 :: bytea),
                                public_group_state = NULL
                            |]

getSubConversationImpl :: (PGConstraints r) => ConvId -> SubConvId -> Sem r (Maybe SubConversation)
getSubConversationImpl cid subConvId = runMaybeT $ do
  (mSuite, mEpoch, mEpochTimestamp, mGroupId) <- MaybeT $ runStatement (cid, subConvId) select
  let activeData =
        ActiveMLSConversationData
          <$> mEpoch
          <*> mEpochTimestamp
          <*> mSuite
  groupId <- hoistMaybe mGroupId
  (cm, im) <- lift $ lookupMLSClientLeafIndicesImpl groupId
  pure $
    SubConversation
      { scParentConvId = cid,
        scSubConvId = subConvId,
        scMLSData =
          ConversationMLSData
            { cnvmlsGroupId = groupId,
              cnvmlsActiveData = activeData
            },
        scMembers = cm,
        scIndexMap = im
      }
  where
    select :: Hasql.Statement (ConvId, SubConvId) (Maybe (Maybe CipherSuiteTag, Maybe Epoch, Maybe UTCTime, Maybe GroupId))
    select =
      dimapPG
        [maybeStatement|SELECT (cipher_suite :: integer?), (epoch :: bigint?), (epoch_timestamp :: timestamptz?), (group_id :: bytea?)
                        FROM subconversation
                        WHERE conv_id = ($1 :: uuid)
                        AND subconv_id = ($2 :: text)
                       |]

getSubConversationGroupInfoImpl :: (PGConstraints r) => ConvId -> SubConvId -> Sem r (Maybe GroupInfoData)
getSubConversationGroupInfoImpl cid subConvId =
  join <$> runStatement (cid, subConvId) select
  where
    select :: Hasql.Statement (ConvId, SubConvId) (Maybe (Maybe GroupInfoData))
    select =
      dimapPG
        [maybeStatement|SELECT (public_group_state :: bytea?)
                        FROM subconversation
                        WHERE conv_id = ($1 :: uuid)
                        AND subconv_id = ($2 :: text)
                       |]

getSubConversationEpochImpl :: (PGConstraints r) => ConvId -> SubConvId -> Sem r (Maybe Epoch)
getSubConversationEpochImpl cid subConvId =
  join <$> runStatement (cid, subConvId) select
  where
    select :: Hasql.Statement (ConvId, SubConvId) (Maybe (Maybe Epoch))
    select =
      dimapPG
        [maybeStatement|SELECT (epoch :: bigint?)
                        FROM subconversation
                        WHERE conv_id = ($1 :: uuid)
                        AND subconv_id = ($2 :: text)
                       |]

setSubConversationGroupInfoImpl :: (PGConstraints r) => ConvId -> SubConvId -> Maybe GroupInfoData -> Sem r ()
setSubConversationGroupInfoImpl cid subConvId mGroupInfo =
  runStatement (cid, subConvId, mGroupInfo) update
  where
    update :: Hasql.Statement (ConvId, SubConvId, Maybe GroupInfoData) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE subconversation
                             SET public_group_state = ($3 :: bytea?)
                             WHERE conv_id = ($1 :: uuid)
                             AND subconv_id = ($2 :: text)
                            |]

setSubConversationEpochImpl :: (PGConstraints r) => ConvId -> SubConvId -> Epoch -> Sem r ()
setSubConversationEpochImpl cid subConvId epoch =
  runStatement (cid, subConvId, epoch) update
  where
    update :: Hasql.Statement (ConvId, SubConvId, Epoch) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE subconversation
                             SET epoch = ($3 :: bigint), epoch_timestamp = NOW()
                             WHERE conv_id = ($1 :: uuid)
                             AND subconv_id = ($2 :: text)
                            |]

setSubConversationCipherSuiteImpl :: (PGConstraints r) => ConvId -> SubConvId -> CipherSuiteTag -> Sem r ()
setSubConversationCipherSuiteImpl cid subConvId cs =
  runStatement (cid, subConvId, cs) update
  where
    update :: Hasql.Statement (ConvId, SubConvId, CipherSuiteTag) ()
    update =
      lmapPG
        [resultlessStatement|UPDATE subconversation
                             SET cipher_suite = ($3 :: integer)
                             WHERE conv_id = ($1 :: uuid)
                             AND subconv_id = ($2 :: text)
                            |]

listSubConversationsImpl :: (PGConstraints r) => ConvId -> Sem r (Map SubConvId ConversationMLSData)
listSubConversationsImpl cid = do
  subs <- runStatement cid select
  pure . Map.fromList $ do
    (subId, mCipherSuite, epoch, ts, gid) <- subs
    let activeData = case (epoch, ts, mCipherSuite) of
          (Epoch 0, _, _) -> Nothing
          (_, _, Nothing) -> Nothing
          (_, t, Just cs) ->
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
  where
    select :: Hasql.Statement (ConvId) [(SubConvId, Maybe CipherSuiteTag, Epoch, UTCTime, GroupId)]
    select =
      dimapPG
        [vectorStatement|SELECT (subconv_id :: text), (cipher_suite :: integer?), (epoch :: bigint), (epoch_timestamp :: timestamptz), (group_id :: bytea)
                         FROM subconversation
                         WHERE conv_id = ($1 :: uuid)
                        |]

deleteSubConversationImpl :: (PGConstraints r) => ConvId -> SubConvId -> Sem r ()
deleteSubConversationImpl cid subConvId =
  runStatement (cid, subConvId) delete
  where
    delete :: Hasql.Statement (ConvId, SubConvId) ()
    delete =
      lmapPG
        [resultlessStatement|DELETE FROM subconversation
                             WHERE conv_id = ($1 :: uuid)
                             AND subconv_id = ($2 :: text)
                            |]
