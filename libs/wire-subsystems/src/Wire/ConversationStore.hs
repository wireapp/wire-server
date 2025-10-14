{-# LANGUAGE TemplateHaskell #-}

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

module Wire.ConversationStore where

import Control.Error (lastMay)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Id
import Data.Misc
import Data.Qualified
import Data.Range
import Data.Time.Clock
import Imports
import Polysemy
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Pagination
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Credential
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.LeafNode
import Wire.API.MLS.SubConversation
import Wire.API.Pagination
import Wire.API.Provider.Service
import Wire.API.Routes.MultiTablePaging
import Wire.ConversationStore.MLS.Types
import Wire.Sem.Paging.Cassandra
import Wire.StoredConversation
import Wire.UserList

data LockAcquired
  = Acquired
  | NotAcquired
  deriving (Show, Eq)

data MLSCommitLockStore m a where
  AcquireCommitLock :: GroupId -> Epoch -> NominalDiffTime -> MLSCommitLockStore m LockAcquired
  ReleaseCommitLock :: GroupId -> Epoch -> MLSCommitLockStore m ()

data ConversationSearch = ConversationSearch
  { team :: TeamId,
    searchString :: Maybe Text,
    pageSize :: PageSize,
    sortOrder :: SortOrder,
    lastName :: Maybe Text,
    lastId :: Maybe ConvId,
    discoverable :: Bool
  }
  deriving (Show)

makeSem ''MLSCommitLockStore

data ConversationStore m a where
  UpsertConversation :: Local ConvId -> NewConversation -> ConversationStore m StoredConversation
  DeleteConversation :: ConvId -> ConversationStore m ()
  GetConversation :: ConvId -> ConversationStore m (Maybe StoredConversation)
  GetConversationEpoch :: ConvId -> ConversationStore m (Maybe Epoch)
  GetConversations :: [ConvId] -> ConversationStore m [StoredConversation]
  GetLocalConversationIds :: UserId -> Maybe ConvId -> Range 1 1000 Int32 -> ConversationStore m (ResultSet ConvId)
  GetRemoteConverastionIds :: UserId -> Maybe (Remote ConvId) -> Range 1 1000 Int32 -> ConversationStore m (ResultSet (Remote ConvId))
  GetConversationMetadata :: ConvId -> ConversationStore m (Maybe ConversationMetadata)
  GetGroupInfo :: ConvId -> ConversationStore m (Maybe GroupInfoData)
  -- FUTUREWORK: This is only relevant for Convs in Cassandra, we can delete it
  -- once we delete the Cassandra interpreter
  IsConversationAlive :: ConvId -> ConversationStore m Bool
  GetRemoteConversationStatus ::
    UserId ->
    [Remote ConvId] ->
    ConversationStore m (Map (Remote ConvId) MemberStatus)
  SelectConversations :: UserId -> [ConvId] -> ConversationStore m [ConvId]
  SetConversationType :: ConvId -> ConvType -> ConversationStore m ()
  SetConversationName :: ConvId -> Range 1 256 Text -> ConversationStore m ()
  SetConversationAccess :: ConvId -> ConversationAccessData -> ConversationStore m ()
  SetConversationReceiptMode :: ConvId -> ReceiptMode -> ConversationStore m ()
  SetConversationMessageTimer :: ConvId -> Maybe Milliseconds -> ConversationStore m ()
  SetConversationEpoch :: ConvId -> Epoch -> ConversationStore m ()
  SetConversationCipherSuite :: ConvId -> CipherSuiteTag -> ConversationStore m ()
  SetConversationCellsState :: ConvId -> CellsState -> ConversationStore m ()
  ResetConversation :: ConvId -> GroupId -> ConversationStore m ()
  SetGroupInfo :: ConvId -> GroupInfoData -> ConversationStore m ()
  UpdateChannelAddPermissions :: ConvId -> AddPermission -> ConversationStore m ()
  UpdateToMixedProtocol :: ConvId -> GroupId -> Epoch -> ConversationStore m ()
  UpdateToMLSProtocol :: ConvId -> ConversationStore m ()
  -- This function only exists to ensure that the cassandra row about team ->
  -- conv relationshop is deleted from cassanrda. This action should be deleted
  -- when we drop support for Cassandra.
  DeleteTeamConversation :: TeamId -> ConvId -> ConversationStore m ()
  GetTeamConversation :: TeamId -> ConvId -> ConversationStore m (Maybe ConvId)
  GetTeamConversations :: TeamId -> ConversationStore m [ConvId]
  DeleteTeamConversations :: TeamId -> ConversationStore m ()
  -- MEMBER OPERATIONS
  UpsertMembers :: ConvId -> UserList (UserId, RoleName) -> ConversationStore m ([LocalMember], [RemoteMember])
  UpsertMembersInRemoteConversation :: Remote ConvId -> [UserId] -> ConversationStore m ()
  CreateBotMember :: ServiceRef -> BotId -> ConvId -> ConversationStore m BotMember
  GetLocalMember :: ConvId -> UserId -> ConversationStore m (Maybe LocalMember)
  GetLocalMembers :: ConvId -> ConversationStore m [LocalMember]
  GetRemoteMember :: ConvId -> Remote UserId -> ConversationStore m (Maybe RemoteMember)
  GetRemoteMembers :: ConvId -> ConversationStore m [RemoteMember]
  CheckLocalMemberRemoteConv :: UserId -> Remote ConvId -> ConversationStore m Bool
  SelectRemoteMembers :: [UserId] -> Remote ConvId -> ConversationStore m ([UserId], Bool)
  SetSelfMember :: Qualified ConvId -> Local UserId -> MemberUpdate -> ConversationStore m ()
  SetOtherMember :: Local ConvId -> Qualified UserId -> OtherMemberUpdate -> ConversationStore m ()
  DeleteMembers :: ConvId -> UserList UserId -> ConversationStore m ()
  DeleteMembersInRemoteConversation :: Remote ConvId -> [UserId] -> ConversationStore m ()
  AddMLSClients :: GroupId -> Qualified UserId -> Set (ClientId, LeafIndex) -> ConversationStore m ()
  PlanClientRemoval :: (Foldable f) => GroupId -> f ClientIdentity -> ConversationStore m ()
  RemoveMLSClients :: GroupId -> Qualified UserId -> Set ClientId -> ConversationStore m ()
  RemoveAllMLSClients :: GroupId -> ConversationStore m ()
  LookupMLSClients :: GroupId -> ConversationStore m (ClientMap LeafIndex)
  LookupMLSClientLeafIndices :: GroupId -> ConversationStore m (ClientMap LeafIndex, IndexMap)
  -- SUB CONVERSATION OPERATIONS
  UpsertSubConversation :: ConvId -> SubConvId -> GroupId -> ConversationStore m SubConversation
  GetSubConversation :: ConvId -> SubConvId -> ConversationStore m (Maybe SubConversation)
  GetSubConversationGroupInfo :: ConvId -> SubConvId -> ConversationStore m (Maybe GroupInfoData)
  GetSubConversationEpoch :: ConvId -> SubConvId -> ConversationStore m (Maybe Epoch)
  SetSubConversationGroupInfo :: ConvId -> SubConvId -> Maybe GroupInfoData -> ConversationStore m ()
  SetSubConversationEpoch :: ConvId -> SubConvId -> Epoch -> ConversationStore m ()
  SetSubConversationCipherSuite :: ConvId -> SubConvId -> CipherSuiteTag -> ConversationStore m ()
  ListSubConversations :: ConvId -> ConversationStore m (Map SubConvId ConversationMLSData)
  DeleteSubConversation :: ConvId -> SubConvId -> ConversationStore m ()
  SearchConversations :: ConversationSearch -> ConversationStore m [ConversationSearchResult]
  -- FOR MIGRATION
  HaveRemoteConvs :: [UserId] -> ConversationStore m [UserId]

makeSem ''ConversationStore

acceptConnectConversation :: (Member ConversationStore r) => ConvId -> Sem r ()
acceptConnectConversation cid = setConversationType cid One2OneConv

-- | Add a member to a local conversation, as an admin.
upsertMember :: (Member ConversationStore r) => Local ConvId -> Local UserId -> Sem r [LocalMember]
upsertMember c u = fst <$> upsertMembers (tUnqualified c) (UserList [(tUnqualified u, roleNameWireAdmin)] [])

getConversationIdsResultSet :: forall r. (Member ConversationStore r) => Local UserId -> Range 1 1000 Int32 -> Maybe (Qualified ConvId) -> Sem r (ResultSet (Qualified ConvId))
getConversationIdsResultSet lusr maxIds mLastId = do
  case fmap (flip relativeTo lusr) mLastId of
    Nothing -> getLocals Nothing
    Just (Local (tUnqualified -> lastId)) -> getLocals (Just lastId)
    Just (Remote lastId) -> getRemotes (Just lastId) maxIds
  where
    localDomain = tDomain lusr
    usr = tUnqualified lusr

    getLocals :: Maybe ConvId -> Sem r (ResultSet (Qualified ConvId))
    getLocals lastId = do
      localPage <- flip Qualified localDomain <$$> getLocalConversationIds usr lastId maxIds
      let remainingSize = fromRange maxIds - fromIntegral (length localPage.resultSetResult)
      case checked remainingSize of
        Nothing -> pure localPage {resultSetType = ResultSetTruncated}
        Just checkedRemaining -> do
          remotePage <- getRemotes Nothing checkedRemaining
          pure
            remotePage
              { resultSetResult = localPage.resultSetResult <> remotePage.resultSetResult
              }

    getRemotes :: Maybe (Remote ConvId) -> Range 1 1000 Int32 -> Sem r (ResultSet (Qualified ConvId))
    getRemotes lastRemote maxRemotes = tUntagged <$$> getRemoteConverastionIds usr lastRemote maxRemotes

-- | This function only exists because we use the 'MultiTablePage' type for the
-- endpoint. Since now the pagination is based on the qualified ids, we can
-- remove the use of this type in future API versions.
getConversationIds :: forall r. (Member ConversationStore r) => Local UserId -> Range 1 1000 Int32 -> Maybe ConversationPagingState -> Sem r ConvIdsPage
getConversationIds lusr maxIds pagingState = do
  let mLastId = Aeson.decode . BS.fromStrict =<< (.mtpsState) =<< pagingState
  resultSet <- getConversationIdsResultSet lusr maxIds mLastId
  let mLastResult = lastMay resultSet.resultSetResult
  pure
    MultiTablePage
      { mtpResults = resultSet.resultSetResult,
        mtpHasMore = case resultSet.resultSetType of
          ResultSetTruncated -> True
          ResultSetComplete -> False,
        mtpPagingState =
          MultiTablePagingState
            { mtpsTable = case fmap (flip relativeTo lusr) mLastResult of
                Just (Local _) -> PagingLocals
                Just (Remote _) -> PagingRemotes
                Nothing -> PagingRemotes,
              mtpsState = BS.toStrict . Aeson.encode <$> mLastResult
            }
      }
