module Galley.ConversationsSubsystem
  ( interpretConversationsSubsystemCassandra,
  )
where

import Cassandra
import Cassandra.Exec
import Conduit
import Data.Bifoldable
import Data.Id
import Galley.Cassandra.Conversation (deleteConversation)
import Galley.Cassandra.Conversation.Members (removeMembersFromLocalConv)
import Galley.Cassandra.Queries (selectConv)
import Galley.Cassandra.Queries qualified as Cql
import Galley.Cassandra.Store (embedClient)
import Galley.Cassandra.Util (logEffect)
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import UnliftIO.Async (pooledForConcurrentlyN, pooledMapConcurrentlyN_)
import Wire.API.Conversation (ConvType (..))
import Wire.ConversationsSubsystem
import Wire.UserList (UserList (UserList))

interpretConversationsSubsystemCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member TinyLog r
  ) =>
  InterpreterFor ConversationsSubsystem r
interpretConversationsSubsystemCassandra =
  interpret $
    \case
      InternalCloseConversationsFrom tid uid -> do
        logEffect "ConversationsSubsystem.internalCloseConversationsFrom"
        embedClient $ closeConversationsFromImpl tid uid

closeConversationsFromImpl :: TeamId -> UserId -> Client ()
closeConversationsFromImpl tid uid =
  runConduit $
    paginateWithStateC listConversationsIds
      .| mapMC performFilter
      .| mapM_C (bimapM_ (mapM_ deleteConversation) (pooledMapConcurrentlyN_ 16 performConversationRemoveUser))
  where
    listConversationsIds pagingState =
      fmap runIdentity <$> paginateWithState Cql.selectUserConvs (paramsPagingState LocalQuorum (Identity uid) 32 pagingState)
    performFilter :: [ConvId] -> Client ([ConvId], [ConvId])
    performFilter convIds = do
      filteredConvIds <-
        concat <$> pooledForConcurrentlyN 16 convIds performConversationsFilter
      let extractConv = map (\(convId, _team, _convType) -> convId)
      pure $
        bimap extractConv extractConv $
          partition (\(_convId, _team, convType) -> convType == One2OneConv) $
            filter (\(_convId, team, _convType) -> team == Just tid) filteredConvIds
    performConversationsFilter :: ConvId -> Client [(ConvId, Maybe TeamId, ConvType)]
    performConversationsFilter convId = do
      results <- retry x1 $ query selectConv $ params LocalQuorum (Identity convId)
      pure $
        flip map results $
          \(convType, _mUserId, _mAccesses, _mRole, _mRoles, _mName, mTeam, _mDeleted, _mTimer, _mMode, _mProtocol, _mGroupId, _mEpoch, _mWriteEpoch, _mCiher, _mGroupConvType, _mChannelPerms, _mCellState) ->
            (convId, mTeam, convType)
    performConversationRemoveUser :: ConvId -> Client ()
    performConversationRemoveUser convId =
      removeMembersFromLocalConv convId (UserList [uid] [])
