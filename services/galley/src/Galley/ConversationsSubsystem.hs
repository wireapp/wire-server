module Galley.ConversationsSubsystem
  ( interpretConversationsSubsystemCassandra,
  )
where

import Cassandra
import Cassandra.Exec
import Conduit
import Data.Id
import Galley.Cassandra.Store (embedClient)
import Galley.Cassandra.Util (logEffect)
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import UnliftIO.Async (pooledForConcurrentlyN)
import Wire.ConversationStore.Cassandra (members)
import Wire.StoredConversation qualified
import Wire.API.Conversation (ConvType (..))
import Wire.API.Team.Conversation (LeavingConversations, newLeavingConversations)
import Wire.BrigAPIAccess qualified as E
import Wire.ConversationStore.Cassandra.Queries (selectConv, selectUserConvs)
import Wire.ConversationsSubsystem

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
      InternalLeavingConversationsFrom tid uid -> do
        logEffect "ConversationsSubsystem.internalLeavingConversationsFrom"
        contacts <- E.getContactList uid
        embedClient $ leavingConversationsFromImpl tid uid contacts

leavingConversationsFromImpl :: TeamId -> UserId -> [UserId] -> Client LeavingConversations
leavingConversationsFromImpl tid uid contacts =
  fmap (uncurry newLeavingConversations) $
    runConduit $
      paginateWithStateC listConversationsIds
        .| mapMC performFilter
        .| foldC
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
      pure (mlss, o2osUnconnected)
    performConversationsFilter :: ConvId -> Client [(ConvId, Maybe TeamId, ConvType)]
    performConversationsFilter convId = do
      results <- retry x1 $ query selectConv $ params LocalQuorum (Identity convId)
      pure $
        flip map results $
          \(convType, _mUserId, _mAccesses, _mRole, _mRoles, _mName, mTeam, _mDeleted, _mTimer, _mMode, _mProtocol, _mGroupId, _mEpoch, _mWriteEpoch, _mCiher, _mGroupConvType, _mChannelPerms, _mCellState, _mParentConvId) ->
            (convId, mTeam, convType)
