{-# LANGUAGE RecordWildCards #-}

module Wire.ConversationStore.Migration where

import Cassandra (ClientState)
import Data.Domain
import Data.Id
import Data.IntMap qualified as IntMap
import Data.Map qualified as Map
import Data.Qualified
import Data.Time
import Data.Tuple.Extra
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions
import Imports
import Polysemy
import Polysemy.Async
import Polysemy.Conc
import Polysemy.Error
import Polysemy.Input
import Polysemy.Time
import Polysemy.TinyLog
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.LeafNode
import Wire.API.MLS.SubConversation
import Wire.API.PostgresMarshall
import Wire.API.Provider.Service
import Wire.ConversationStore
import Wire.ConversationStore.Cassandra (interpretConversationStoreToCassandra)
import Wire.ConversationStore.MLS.Types
import Wire.ConversationStore.MigrationLock
import Wire.ConversationStore.Postgres
import Wire.Postgres (runTransaction)
import Wire.StoredConversation

migrateAllConversations :: Sem r ()
migrateAllConversations = undefined

migrateConversation :: (PGConstraints r, Member (Input ClientState) r, Member TinyLog r, Member Async r, Member (Error MigrationLockError) r, Member Race r) => ConvId -> Sem r ()
migrateConversation cid = do
  void . withMigrationLocks LockExclusive (Seconds 10) [Left cid] $ do
    mConvData <- getConvFromCassandra cid
    for_ mConvData $ \convData -> do
      saveConvToPostgres convData
      deleteConvFromCassandra convData

data ConvMLSDetails = ConvMLSDetails
  { groupInfoData :: GroupInfoData,
    clientMap :: ClientMap LeafIndex,
    indexMap :: IndexMap
  }

data AllSubConvData = AllSubConvData
  { subConv :: SubConversation,
    groupInfoData :: Maybe GroupInfoData
  }

data AllConvData = AllConvData
  { conv :: StoredConversation,
    mlsDetails :: Maybe ConvMLSDetails,
    subConvs :: [AllSubConvData]
  }

getConvFromCassandra :: (Member (Input ClientState) r, Member TinyLog r, Member (Embed IO) r) => ConvId -> Sem r (Maybe AllConvData)
getConvFromCassandra cid = withCassandra $ do
  getConversation cid >>= \case
    Nothing -> pure Nothing
    Just conv -> do
      subConvMlsData <- listSubConversations cid
      mGroupInfo <- getGroupInfo cid
      mlsLeafIndices <- case mlsMetadata conv of
        Nothing -> pure Nothing
        Just (mlsData, _) -> do
          (cm, im) <- lookupMLSClientLeafIndices mlsData.cnvmlsGroupId
          pure $ Just (cm, im)
      let mlsDetails = ConvMLSDetails <$> mGroupInfo <*> fmap fst mlsLeafIndices <*> fmap snd mlsLeafIndices
      subConvs <- fmap Map.elems $ flip Map.traverseWithKey subConvMlsData $ \subConvId mlsData -> do
        (cm, im) <- lookupMLSClientLeafIndices mlsData.cnvmlsGroupId
        let subconv =
              SubConversation
                { scParentConvId = cid,
                  scSubConvId = subConvId,
                  scMLSData = mlsData,
                  scMembers = cm,
                  scIndexMap = im
                }
        gi <- getSubConversationGroupInfo cid subConvId
        pure $ AllSubConvData subconv gi
      pure . Just $ AllConvData {..}

deleteConvFromCassandra :: (Member (Input ClientState) r, Member TinyLog r, Member (Embed IO) r) => AllConvData -> Sem r ()
deleteConvFromCassandra allConvData = withCassandra $ do
  for_ allConvData.subConvs $ \subConvData -> do
    removeAllMLSClients subConvData.subConv.scMLSData.cnvmlsGroupId
    deleteSubConversation allConvData.conv.id_ subConvData.subConv.scSubConvId

  for_ (getMLSData allConvData.conv.protocol) $ \mlsData ->
    removeAllMLSClients mlsData.cnvmlsGroupId

  case allConvData.conv.metadata.cnvmTeam of
    Nothing -> deleteConversation allConvData.conv.id_
    Just tid -> deleteTeamConversation tid allConvData.conv.id_

saveConvToPostgres :: (PGConstraints r) => AllConvData -> Sem r ()
saveConvToPostgres allConvData = do
  let meta = storedConv.metadata
      mMlsData = getMLSData storedConv.protocol
      mActiveMLSData = cnvmlsActiveData =<< mMlsData
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
          (.epoch) <$> mActiveMLSData,
          epochTimestamp <$> mActiveMLSData,
          ciphersuite <$> mActiveMLSData,
          (.groupInfoData) <$> allConvData.mlsDetails,
          meta.cnvmGroupConvType,
          meta.cnvmChannelAddPermission,
          meta.cnvmCellsState,
          meta.cnvmParent
        )
  runTransaction ReadCommitted Write $ do
    Transaction.statement convRow insertConv
    Transaction.statement localMemberColumns insertLocalMembers
    Transaction.statement remoteMemberColumns insertRemoteMembers
    Transaction.statement subConvColumns insertSubConvs
    Transaction.statement mlsClientColumns insertMLSClients
  where
    storedConv = allConvData.conv
    -- In all these queries we do nothing on conflict because if the data is in
    -- Postgres it is considered fresher and data from Cassandra is ignored.
    insertConv =
      lmapPG @_ @(_, _, _, Vector Int32, Vector Int32, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
        [resultlessStatement|INSERT INTO conversation
                             (id, type, creator, access, access_roles_v2,
                              name, team, message_timer, receipt_mode, protocol,
                              group_id, epoch, epoch_timestamp, cipher_suite, group_conv_type,
                              channel_add_permission, cells_state, parent_conv)
                             VALUES
                             ($1 :: uuid, $2 :: integer, $3 :: uuid?, $4 :: integer[], $5 :: integer[],
                              $6 :: text?, $7 :: uuid?, $8 :: bigint?, $9 :: integer?, $10 :: integer,
                              $11 :: bytea?, $12 :: bigint?, $13 :: timestamptz?, $14 :: integer?,  $15 :: bytea?,
                              $16 ::integer?, $17 :: integer?, $18 :: integer, $19 :: uuid?)
                             ON CONFLICT (id) DO NOTHING
                            |]

    localMemberColumns ::
      ( [ConvId],
        [UserId],
        [Maybe ServiceId],
        [Maybe ProviderId],
        [Maybe MutedStatus],
        [Maybe Text],
        [Bool],
        [Maybe Text],
        [Bool],
        [Maybe Text],
        [RoleName]
      )
    localMemberColumns =
      let mems = storedConv.localMembers
       in ( replicate (length mems) storedConv.id_,
            map (.id_) mems,
            map (fmap (._serviceRefId) . (.service)) mems,
            map (fmap (._serviceRefProvider) . (.service)) mems,
            map (.status.msOtrMutedStatus) mems,
            map (.status.msOtrMutedRef) mems,
            map (.status.msOtrArchived) mems,
            map (.status.msOtrArchivedRef) mems,
            map (.status.msHidden) mems,
            map (.status.msHiddenRef) mems,
            map (.convRoleName) mems
          )

    remoteMemberColumns :: ([ConvId], [Domain], [UserId], [RoleName])
    remoteMemberColumns =
      ( replicate (length storedConv.remoteMembers) storedConv.id_,
        map (tDomain . (.id_)) storedConv.remoteMembers,
        map (tUnqualified . (.id_)) storedConv.remoteMembers,
        map (.convRoleName) storedConv.remoteMembers
      )

    insertLocalMembers ::
      Hasql.Statement
        ( [ConvId],
          [UserId],
          [Maybe ServiceId],
          [Maybe ProviderId],
          [Maybe MutedStatus],
          [Maybe Text],
          [Bool],
          [Maybe Text],
          [Bool],
          [Maybe Text],
          [RoleName]
        )
        ()
    insertLocalMembers =
      lmapPG @_ @(Vector _, Vector _, Vector _, Vector _, Vector _, Vector _, Vector _, Vector _, Vector _, Vector _, Vector _)
        [resultlessStatement|INSERT INTO conversation_member
                             (conv, "user", service, provider, otr_muted_status, otr_muted_ref,
                              otr_archived, otr_archived_ref, hidden, hidden_ref, conversation_role)
                             SELECT *
                             FROM UNNEST ($1 :: uuid[], $2 :: uuid[], $3 :: uuid?[], $4 :: uuid?[],
                                          $5 :: integer?[], $6 :: text?[], $7 :: boolean[], $8 :: text?[],
                                          $9 :: boolean[], $10 :: text?[], $11 :: text[])
                             ON CONFLICT (conv, "user") DO NOTHING
                            |]
    insertRemoteMembers :: Hasql.Statement ([ConvId], [Domain], [UserId], [RoleName]) ()
    insertRemoteMembers =
      lmapPG @_ @(Vector _, Vector _, Vector _, Vector _)
        [resultlessStatement|INSERT INTO local_conversation_remote_member
                             (conv, user_remote_domain, user_remote_id, conversation_role)
                             SELECT * FROM UNNEST($1 :: uuid[], $2 :: text[], $3 :: uuid[], $4 :: text[])
                            |]

    mlsClientRows :: GroupId -> ClientMap LeafIndex -> IndexMap -> [(GroupId, Domain, UserId, ClientId, Int32, Bool)]
    mlsClientRows gid clientMap indexMap =
      let clients :: [(LeafIndex, ClientIdentity, Bool)] =
            IntMap.elems $
              IntMap.mapWithKey
                (\idx ci -> (fromIntegral idx, ci, isNothing (cmLookupIndex ci clientMap)))
                indexMap.unIndexMap
       in flip map clients $ \(idx, ci, removalPending) ->
            (gid, ci.ciDomain, ci.ciUser, ci.ciClient, fromIntegral idx, removalPending)

    mlsClientColumns :: ([GroupId], [Domain], [UserId], [ClientId], [Int32], [Bool])
    mlsClientColumns =
      let mainConvGroupId = cnvmlsGroupId <$> getMLSData storedConv.protocol
          mainConvInputs = maybeToList $ (,,) <$> mainConvGroupId <*> (fmap (.clientMap) allConvData.mlsDetails) <*> (fmap (.indexMap) allConvData.mlsDetails)
          subConvsInputs = flip map allConvData.subConvs $ \(AllSubConvData sc _) -> (sc.scMLSData.cnvmlsGroupId, sc.scMembers, sc.scIndexMap)
          allInputs = mainConvInputs <> subConvsInputs
          allRows = concatMap (uncurry3 mlsClientRows) allInputs
       in unzip6 allRows

    insertMLSClients :: Hasql.Statement ([GroupId], [Domain], [UserId], [ClientId], [Int32], [Bool]) ()
    insertMLSClients =
      lmapPG @_ @(Vector _, Vector _, Vector _, Vector _, Vector _, Vector _)
        [resultlessStatement|INSERT INTO mls_group_member_client
                             (group_id, user_domain, "user", client, leaf_node_index, removal_pending)
                             SELECT *
                             FROM UNNEST ($1 :: bytea[], $2 :: text[], $3 :: uuid[],
                                          $4 :: text[], $5 :: integer[], $6 :: bool[])
                            |]

    subConvRows :: [(ConvId, SubConvId, Maybe CipherSuiteTag, Maybe Epoch, Maybe UTCTime, GroupId, Maybe GroupInfoData)]
    subConvRows =
      flip map allConvData.subConvs $ \scData ->
        ( storedConv.id_,
          scData.subConv.scSubConvId,
          (.ciphersuite) <$> scData.subConv.scMLSData.cnvmlsActiveData,
          (.epoch) <$> scData.subConv.scMLSData.cnvmlsActiveData,
          (.epochTimestamp) <$> scData.subConv.scMLSData.cnvmlsActiveData,
          scData.subConv.scMLSData.cnvmlsGroupId,
          scData.groupInfoData
        )

    subConvColumns :: ([ConvId], [SubConvId], [Maybe CipherSuiteTag], [Maybe Epoch], [Maybe UTCTime], [GroupId], [Maybe GroupInfoData])
    subConvColumns = unzip7 subConvRows

    insertSubConvs :: Hasql.Statement ([ConvId], [SubConvId], [Maybe CipherSuiteTag], [Maybe Epoch], [Maybe UTCTime], [GroupId], [Maybe GroupInfoData]) ()
    insertSubConvs =
      lmapPG @_ @(Vector _, Vector _, Vector _, Vector _, Vector _, Vector _, Vector _)
        [resultlessStatement|INSERT INTO subconversation
                             (conv_id, subconv_id, cipher_suite, epoch, epoch_timestamp, group_id, public_group_state)
                             SELECT *
                             FROM UNNEST ($1 :: uuid[], $2 :: text[], $3 :: integer?[],
                                          $4 :: bigint?[], $5 :: timestamptz?[], $6 :: bytea[], $7 :: bytea?[])
                            |]

withCassandra :: (Member (Input ClientState) r, Member TinyLog r, Member (Embed IO) r) => InterpreterFor ConversationStore r
withCassandra action = do
  cstate <- input
  interpretConversationStoreToCassandra cstate action
