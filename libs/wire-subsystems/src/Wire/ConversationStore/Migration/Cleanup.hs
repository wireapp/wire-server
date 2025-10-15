{-# LANGUAGE RecordWildCards #-}

module Wire.ConversationStore.Migration.Cleanup where

import Cassandra
import Data.Id
import Data.Map qualified as Map
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Protocol
import Wire.API.PostgresMarshall
import Wire.ConversationStore
import Wire.ConversationStore.MLS.Types
import Wire.ConversationStore.Migration.Types
import Wire.ConversationStore.Postgres
import Wire.Postgres
import Wire.StoredConversation
import Wire.Util

data DeletionType = DeleteConv | DeleteUser

instance PostgresMarshall DeletionType Text where
  postgresMarshall = \case
    DeleteConv -> "conv"
    DeleteUser -> "user"

markDeletionPendingStmt :: Hasql.Statement (DeletionType, Id a) ()
markDeletionPendingStmt =
  lmapPG
    [resultlessStatement|INSERT INTO conversation_migration_pending_deletes
                         (typ, id)
                         VALUES ($1 :: text, $2 :: uuid)
                         ON CONFLICT DO NOTHING
                        |]

markDeletionComplete :: (PGConstraints r) => DeletionType -> Id a -> Sem r ()
markDeletionComplete typ id_ = runStatement (typ, id_) delete
  where
    delete :: Hasql.Statement (DeletionType, Id a) ()
    delete =
      lmapPG
        [resultlessStatement|DELETE FROM conversation_migration_pending_deletes
                             WHERE typ = $1 :: text AND id = $2 :: uuid
                            |]

getAllConvData :: (Member ConversationStore r) => ConvId -> Sem r (Maybe AllConvData)
getAllConvData cid = do
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

deleteConv :: (Member ConversationStore r) => AllConvData -> Sem r ()
deleteConv allConvData = do
  for_ allConvData.subConvs $ \subConvData -> do
    removeAllMLSClients subConvData.subConv.scMLSData.cnvmlsGroupId
    deleteSubConversation allConvData.conv.id_ subConvData.subConv.scSubConvId

  for_ (getMLSData allConvData.conv.protocol) $ \mlsData ->
    removeAllMLSClients mlsData.cnvmlsGroupId

  case allConvData.conv.metadata.cnvmTeam of
    Nothing -> deleteConversation allConvData.conv.id_
    Just tid -> deleteTeamConversation tid allConvData.conv.id_

deleteRemoteMemberStatusesFromCassandra :: (Member (Input ClientState) r, Member (Embed IO) r) => UserId -> Sem r ()
deleteRemoteMemberStatusesFromCassandra uid = do
  cstate <- input
  embedClient cstate $
    retry x5 $
      write delete (params LocalQuorum (Identity uid))
  where
    delete :: PrepQuery W (Identity UserId) ()
    delete = "delete from user_remote_conv where user = ?"

cleanupIfNecessary :: (PGConstraints r, Member (Input ClientState) r, Member ConversationStore r) => [Either ConvId UserId] -> Sem r ()
cleanupIfNecessary = mapM_ (either cleanupConvIfNecessary cleanupUserIfNecesasry)

cleanupUserIfNecesasry :: (PGConstraints r, Member (Input ClientState) r) => UserId -> Sem r ()
cleanupUserIfNecesasry uid =
  whenM (isPendingDelete DeleteUser uid) $ do
    deleteRemoteMemberStatusesFromCassandra uid
    markDeletionComplete DeleteUser uid

cleanupConvIfNecessary :: (PGConstraints r, Member ConversationStore r) => ConvId -> Sem r ()
cleanupConvIfNecessary cid =
  whenM (isPendingDelete DeleteConv cid) $ do
    maybe (pure ()) deleteConv =<< getAllConvData cid
    markDeletionComplete DeleteConv cid

isPendingDelete :: (PGConstraints r) => DeletionType -> Id a -> Sem r Bool
isPendingDelete typ id_ = runStatement (typ, id_) select
  where
    select =
      lmapPG
        [singletonStatement|SELECT EXISTS (SELECT 1
                                           FROM conversation_migration_pending_deletes
                                           WHERE typ = $1 :: text AND id = $2 :: uuid
                                          ) :: boolean
                           |]
