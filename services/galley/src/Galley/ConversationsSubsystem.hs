module Galley.ConversationsSubsystem
  ( interpretConversationsSubsystemCassandra,
  )
where

import Cassandra
import Cassandra.Exec
import Conduit
import Data.Bifoldable
import Data.Id
import Galley.Cassandra.Store (embedClient)
import Galley.Cassandra.Util (logEffect)
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import UnliftIO.Async (pooledForConcurrentlyN, pooledMapConcurrentlyN_)
import Wire.API.Conversation (ConvType (..))
import Wire.BrigAPIAccess qualified as E
import Wire.ConversationStore.Cassandra (deleteConversation, members, removeMembersFromLocalConv)
import Wire.ConversationStore.Cassandra.Queries (selectConv, selectUserConvs)
import Wire.ConversationsSubsystem
import Wire.StoredConversation qualified
import Wire.UserList (UserList (UserList))

interpretConversationsSubsystemCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member E.BrigAPIAccess r,
    Member TinyLog r
  ) =>
  InterpreterFor ConversationsSubsystem r
interpretConversationsSubsystemCassandra =
  interpret $
    \case
      InternalCloseConversationsFrom tid uid -> do
        logEffect "ConversationsSubsystem.internalCloseConversationsFrom"
        contacts <- E.getContactList uid
        embedClient $ closeConversationsFromImpl tid uid contacts

closeConversationsFromImpl :: TeamId -> UserId -> [UserId] -> Client ()
closeConversationsFromImpl tid uid contacts = do
  runConduit $
    paginateWithStateC listConversationsIds
      .| mapMC performFilter
      .| mapM_C (bimapM_ (mapM_ deleteConversation) (pooledMapConcurrentlyN_ 16 performConversationRemoveUser))
  where
    listConversationsIds pagingState =
      fmap runIdentity <$> paginateWithState selectUserConvs (paramsPagingState LocalQuorum (Identity uid) 32 pagingState)
    performFilter :: [ConvId] -> Client ([ConvId], [ConvId])
    performFilter convIds = do
      filteredConvIds <-
        concat <$> pooledForConcurrentlyN 16 convIds performConversationsFilter
      let filteredTeamConvIds = filter (\(_convId, team, _convType) -> team == Just tid) filteredConvIds
          extractConv = map (\(convId, _team, _convType) -> convId)
          (o2os, mlss) =
            bimap extractConv extractConv $
              partition (\(_convId, _team, convType) -> convType == One2OneConv) filteredTeamConvIds
          isNotConnectedToMember convId = do
            localMembers <- members convId
            pure $ any (flip notElem (uid : contacts) . (.id_)) localMembers
      o2osUnconnected <- filterM isNotConnectedToMember o2os
      pure (o2osUnconnected, mlss)
    performConversationsFilter :: ConvId -> Client [(ConvId, Maybe TeamId, ConvType)]
    performConversationsFilter convId = do
      results <- retry x1 $ query selectConv $ params LocalQuorum (Identity convId)
      pure $
        flip map results $
          \(convType, _mUserId, _mAccesses, _mRole, _mRoles, _mName, mTeam, _mDeleted, _mTimer, _mMode, _mProtocol, _mGroupId, _mEpoch, _mWriteEpoch, _mCiher, _mGroupConvType, _mChannelPerms, _mCellState, _mParentConvId) ->
            (convId, mTeam, convType)
    performConversationRemoveUser :: ConvId -> Client ()
    performConversationRemoveUser convId =
      removeMembersFromLocalConv convId (UserList [uid] [])
