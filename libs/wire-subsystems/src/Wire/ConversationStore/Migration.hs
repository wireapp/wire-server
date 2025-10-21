{-# LANGUAGE RecordWildCards #-}

module Wire.ConversationStore.Migration where

import Cassandra
import Cassandra.Settings hiding (pageSize)
import Control.Error (lastMay)
import Data.Conduit
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as C
import Data.Domain
import Data.Id
import Data.IntMap qualified as IntMap
import Data.Map qualified as Map
import Data.Qualified
import Data.Time
import Data.Tuple.Extra
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Pool qualified as Hasql
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
import Polysemy.State
import Polysemy.Time
import Polysemy.TinyLog
import System.Logger qualified as Log
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
import Wire.ConversationStore.Migration.Cleanup
import Wire.ConversationStore.Migration.Types
import Wire.ConversationStore.MigrationLock
import Wire.ConversationStore.Postgres
import Wire.Postgres
import Wire.Sem.Logger.TinyLog (loggerToTinyLog)
import Wire.Sem.Paging.Cassandra
import Wire.StoredConversation
import Wire.Util

-- * Top level logic

type EffectStack = [State Int, Input ClientState, Input Hasql.Pool, Async, Race, TinyLog, Embed IO, Final IO]

migrateConvsLoop :: ClientState -> Hasql.Pool -> Log.Logger -> IO ()
migrateConvsLoop cassClient pgPool logger =
  migrationLoop cassClient pgPool logger "conversations" migrateAllConversations

migrateUsersLoop :: ClientState -> Hasql.Pool -> Log.Logger -> IO ()
migrateUsersLoop cassClient pgPool logger =
  migrationLoop cassClient pgPool logger "users" migrateAllUsers

migrationLoop :: ClientState -> Hasql.Pool -> Log.Logger -> ByteString -> ConduitT () Void (Sem EffectStack) () -> IO ()
migrationLoop cassClient pgPool logger name migration = go 0
  where
    go :: Int -> IO ()
    go nIter = do
      runMigration >>= \case
        0 ->
          Log.info logger $
            Log.msg (Log.val "finished migration")
              . Log.field "attempt" nIter
        n -> do
          Log.info logger $
            Log.msg (Log.val "finished migration with errors")
              . Log.field "migration" name
              . Log.field "errors" n
              . Log.field "attempt" nIter
          go (nIter + 1)

    runMigration :: IO Int
    runMigration =
      fmap fst
        . interpreter cassClient pgPool logger
        $ runConduit migration

interpreter :: ClientState -> Hasql.Pool -> Log.Logger -> Sem EffectStack a -> IO (Int, a)
interpreter cassClient pgPool logger =
  runFinal
    . embedToFinal
    . loggerToTinyLog logger
    . interpretRace
    . asyncToIOFinal
    . runInputConst pgPool
    . runInputConst cassClient
    . runState 0

-- * Paginated Migration

pageSize :: Int32
pageSize = 10000

migrateAllConversations ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member Async r,
    Member Race r,
    Member (State Int) r
  ) =>
  ConduitM () Void (Sem r) ()
migrateAllConversations =
  withCount (paginateSem select (paramsP LocalQuorum () pageSize) x5)
    .| logRetrievedPage
    .| C.mapM_ (mapM_ (handleErrors migrateConversation "conv"))
    .| C.sinkNull
  where
    select :: PrepQuery R () (Identity ConvId)
    select = "select conv from conversation"

migrateAllUsers ::
  ( Member (Input Hasql.Pool) r,
    Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member Async r,
    Member Race r,
    Member (State Int) r
  ) =>
  ConduitM () Void (Sem r) ()
migrateAllUsers =
  withCount
    (paginateSem select (paramsP LocalQuorum () pageSize) x5)
    .| logRetrievedPage
    .| C.mapM_ (mapM_ (handleErrors migrateUser "user"))
    .| C.sinkNull
  where
    select :: PrepQuery R () (Identity UserId)
    select = "select distinct user from user_remote_conv"

logRetrievedPage :: (Member TinyLog r) => ConduitM (Int32, [Identity (Id a)]) [Id a] (Sem r) ()
logRetrievedPage =
  C.mapM
    ( \(i, rows) -> do
        let estimatedRowsSoFar = (i - 1) * pageSize + fromIntegral (length rows)
        info $ Log.msg (Log.val "retrieved page") . Log.field "estimatedRowsSoFar" estimatedRowsSoFar
        pure $ map runIdentity rows
    )

withCount :: (Monad m) => ConduitM () [a] m () -> ConduitM () (Int32, [a]) m ()
withCount = zipSources (C.sourceList [1 ..])

handleErrors :: (Member (State Int) r, Member TinyLog r) => (Id a -> Sem (Error MigrationLockError : Error Hasql.UsageError : r) b) -> ByteString -> Id a -> Sem r (Maybe b)
handleErrors action lockType id_ =
  join <$> handleError (handleError action lockType) lockType id_

handleError :: (Member (State Int) r, Member TinyLog r, Show e) => (Id a -> Sem (Error e : r) b) -> ByteString -> Id a -> Sem r (Maybe b)
handleError action lockType id_ = do
  eithErr <- runError (action id_)
  case eithErr of
    Right x -> pure $ Just x
    Left e -> do
      warn $
        Log.msg (Log.val "error occurred during migration")
          . Log.field lockType (idToText id_)
          . Log.field "error" (show e)
      modify (+ 1)
      pure Nothing

-- * Conversations

migrateConversation ::
  ( PGConstraints r,
    Member (Input ClientState) r,
    Member TinyLog r,
    Member Async r,
    Member (Error MigrationLockError) r,
    Member Race r
  ) =>
  ConvId ->
  Sem r ()
migrateConversation cid = do
  void . withMigrationLocks LockExclusive (Seconds 10) [Left cid] $ do
    mConvData <- withCassandra $ getAllConvData cid
    for_ mConvData $ \convData -> do
      saveConvToPostgres convData
      withCassandra $ deleteConv convData
    markDeletionComplete DeleteConv cid

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
    Transaction.statement (DeleteConv, storedConv.id_) markDeletionPendingStmt
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

-- * Users

migrateUser :: (PGConstraints r, Member (Input ClientState) r, Member TinyLog r, Member Async r, Member (Error MigrationLockError) r, Member Race r) => UserId -> Sem r ()
migrateUser uid = do
  withMigrationLocks LockExclusive (Seconds 10) [Right uid] $ do
    statusses <- getRemoteMemberStatusFromCassandra uid
    saveRemoteMemberStatusToPostgres uid statusses
    deleteRemoteMemberStatusesFromCassandra uid
  markDeletionComplete DeleteUser uid

getRemoteMemberStatusFromCassandra :: forall r. (Member (Input ClientState) r, Member TinyLog r, Member (Embed IO) r) => UserId -> Sem r (Map (Remote ConvId) MemberStatus)
getRemoteMemberStatusFromCassandra uid = withCassandra $ do
  convIds <- getAllRemoteConvIds [] Nothing
  getRemoteConversationStatus uid convIds
  where
    getAllRemoteConvIds :: [Remote ConvId] -> Maybe (Remote ConvId) -> Sem (ConversationStore ': r) [Remote ConvId]
    getAllRemoteConvIds acc mLastId = do
      res <- getRemoteConverastionIds uid mLastId maxBound
      let newAcc = res.resultSetResult <> acc
      case (res.resultSetResult, res.resultSetType) of
        ([], _) -> pure newAcc
        (_, ResultSetTruncated) -> getAllRemoteConvIds newAcc (lastMay res.resultSetResult)
        (_, ResultSetComplete) -> pure newAcc

saveRemoteMemberStatusToPostgres :: (PGConstraints r) => UserId -> Map (Remote ConvId) MemberStatus -> Sem r ()
saveRemoteMemberStatusToPostgres uid statusses =
  runTransaction ReadCommitted Write $ do
    Transaction.statement statusColumns insertStatuses
    Transaction.statement (DeleteUser, uid) markDeletionPendingStmt
  where
    insertStatuses :: Hasql.Statement ([UserId], [Domain], [ConvId], [Maybe MutedStatus], [Maybe Text], [Bool], [Maybe Text], [Bool], [Maybe Text]) ()
    insertStatuses =
      lmapPG @_ @(Vector _, Vector _, Vector _, Vector _, Vector _, Vector _, Vector _, Vector _, Vector _)
        [resultlessStatement|INSERT INTO remote_conversation_local_member
                             ("user", conv_remote_domain, conv_remote_id, otr_muted_status, otr_muted_ref, otr_archived, otr_archived_ref, hidden, hidden_ref)
                             SELECT *
                             FROM UNNEST ($1 :: uuid[], $2 :: text[], $3 :: uuid[],
                                          $4 :: integer?[], $5 :: text?[],
                                          $6 :: bool[], $7 :: text?[],
                                          $8 :: bool[], $9 :: text?[]
                                         )
                            |]

    statusColumns = unzip9 statusRows

    statusRows :: [(UserId, Domain, ConvId, Maybe MutedStatus, Maybe Text, Bool, Maybe Text, Bool, Maybe Text)]
    statusRows =
      Map.foldrWithKey (\rcid status -> (statusRow rcid status :)) [] statusses

    statusRow :: Remote ConvId -> MemberStatus -> (UserId, Domain, ConvId, Maybe MutedStatus, Maybe Text, Bool, Maybe Text, Bool, Maybe Text)
    statusRow (tUntagged -> Qualified cid dom) MemberStatus {..} =
      (uid, dom, cid, msOtrMutedStatus, msOtrMutedRef, msOtrArchived, msOtrArchivedRef, msHidden, msHiddenRef)

-- * Other helpers

withCassandra :: (Member (Input ClientState) r, Member TinyLog r, Member (Embed IO) r) => InterpreterFor ConversationStore r
withCassandra action = do
  cstate <- input
  interpretConversationStoreToCassandra cstate action

unzip9 :: [(a, b, c, d, e, f, g, h, i)] -> ([a], [b], [c], [d], [e], [f], [g], [h], [i])
unzip9 [] = ([], [], [], [], [], [], [], [], [])
unzip9 ((y1, y2, y3, y4, y5, y6, y7, y8, y9) : ys) =
  let (l1, l2, l3, l4, l5, l6, l7, l8, l9) = unzip9 ys
   in (y1 : l1, y2 : l2, y3 : l3, y4 : l4, y5 : l5, y6 : l6, y7 : l7, y8 : l8, y9 : l9)

paginateSem :: forall a b q r. (Tuple a, Tuple b, RunQ q, Member (Input ClientState) r, Member (Embed IO) r) => q R a b -> QueryParams a -> RetrySettings -> ConduitT () [b] (Sem r) ()
paginateSem q p r = go =<< lift getFirstPage
  where
    go page = do
      unless (null (result page)) $
        yield (result page)
      when (hasMore page) $
        go =<< lift (getNextPage page)

    getFirstPage :: Sem r (Page b)
    getFirstPage = do
      client <- input
      embedClient client $ retry r (paginate q p)

    getNextPage :: Page b -> Sem r (Page b)
    getNextPage page = do
      client <- input
      embedClient client $ retry r (nextPage page)
