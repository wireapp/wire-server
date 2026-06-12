{-# LANGUAGE RecordWildCards #-}

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

module Wire.ConversationStore.Migration.Cleanup where

import Cassandra
import Data.Id
import Data.Map qualified as Map
import Data.Vector (Vector)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
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
import Wire.Postgres
import Wire.StoredConversation
import Wire.Util

data DeletionType = DeleteConv | DeleteUser

instance PostgresMarshall Text DeletionType where
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
      mlsDetails <- case mlsMetadata conv of
        Nothing -> pure Nothing
        Just (mlsData, _) -> do
          (cm, im) <- lookupMLSClientLeafIndices mlsData.cnvmlsGroupId
          pure $ ConvMLSDetails <$> mGroupInfo <*> pure cm <*> pure im
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

cleanupIfNecessary :: forall r. (PGConstraints r, Member (Input ClientState) r, Member ConversationStore r) => [Either ConvId UserId] -> Sem r ()
cleanupIfNecessary ids = do
  (pendingConvIds, pendingUserIds) <- runSessionWithRetry $ do
    let (convIds, userIds) = partitionEithers ids
    pendingConvIds <- Session.statement (DeleteConv, convIds) filterPendingDeletes
    pendingUserIds <- Session.statement (DeleteUser, userIds) filterPendingDeletes
    pure (pendingConvIds, pendingUserIds)

  unless (null pendingConvIds) $ do
    cleanupConvs pendingConvIds
    runStatement (DeleteConv, pendingConvIds) markDeletionsComplete

  unless (null pendingUserIds) $ do
    cleanupUsers pendingUserIds
    runStatement (DeleteUser, pendingUserIds) markDeletionsComplete
  where
    markDeletionsComplete :: Statement (DeletionType, [Id a]) ()
    markDeletionsComplete =
      lmapPG @(_, Vector _)
        [resultlessStatement|DELETE FROM conversation_migration_pending_deletes
                             WHERE typ = $1 :: text AND id = ANY($2 :: uuid[])|]

    filterPendingDeletes :: Statement (DeletionType, [Id a]) [Id a]
    filterPendingDeletes =
      dimapPG @(_, Vector _) @_ @(Vector _) @[_]
        [vectorStatement|SELECT id :: uuid
                         FROM conversation_migration_pending_deletes
                         WHERE typ = $1 :: text AND id = ANY($2 :: uuid[])
                        |]
    cleanupConvs :: [ConvId] -> Sem r ()
    cleanupConvs =
      mapM_ $ \cid -> do
        mConvData <- getAllConvData cid
        forM_ mConvData deleteConv

    cleanupUsers :: [UserId] -> Sem r ()
    cleanupUsers =
      mapM_ deleteRemoteMemberStatusesFromCassandra
