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
import Galley.Cassandra.Queries qualified as Cql
import Galley.Cassandra.Store (embedClient)
import Galley.Cassandra.Util (logEffect)
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Wire.API.Conversation (ConvType (..))
import Wire.ConversationsSubsystem
import Wire.Sem.Paging.Cassandra (InternalPage (InternalPage), mkInternalPage)

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
      .| mapM_C (bimapM_ (mapM_ deleteConversation) performConversationsRemoveUser)
  where
    listConversationsIds pagingState =
      fmap runIdentity <$> paginateWithState Cql.selectUserConvs (paramsPagingState LocalQuorum (Identity uid) 32 pagingState)
    performFilter :: [ConvId] -> Client ([ConvId], [ConvId])
    performFilter convIds = do
      page <- paginate conversationsFilter (params LocalQuorum (Identity convIds))
      InternalPage (_, _, filteredConvIds) <- mkInternalPage page pure
      let extractConv = map (\(convId, _team, _convType) -> convId)
      pure $
        bimap extractConv extractConv $
          partition (\(_convId, _team, convType) -> convType == One2OneConv) $
            filter (\(_convId, team, _convType) -> team == Just tid) filteredConvIds
    conversationsFilter :: PrepQuery R (Identity [ConvId]) (ConvId, Maybe TeamId, ConvType)
    conversationsFilter = "select conv, team, type from conversation where conv in ?"
    performConversationsRemoveUser convIds = retry x5 $ write conversationsRemoveUser (params LocalQuorum (uid, convIds))
    conversationsRemoveUser :: PrepQuery W (UserId, [ConvId]) ()
    conversationsRemoveUser = "delete from user where user = ? and conv in ?"
