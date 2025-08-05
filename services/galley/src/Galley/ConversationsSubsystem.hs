module Galley.ConversationsSubsystem
  ( interpretConversationsSubsystemCassandra,
  )
where

import Cassandra
import Cassandra.Exec
import Conduit
import Data.Id
import Galley.Cassandra.Queries qualified as Cql
import Galley.Cassandra.Store (embedClient)
import Galley.Cassandra.Util (logEffect)
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
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
      .| mapM_C performUpdate
  where
    listConversationsIds pagingState =
      fmap runIdentity <$> paginateWithState Cql.selectUserConvs (paramsPagingState LocalQuorum (Identity uid) 32 pagingState)
    performFilter :: [ConvId] -> Client [ConvId]
    performFilter convIds = do
      page <- paginate conversationsFilter (params LocalQuorum (Identity convIds))
      InternalPage (_, _, filteredConvIds) <- mkInternalPage page pure
      pure $
        map (\(convId, _team, _mCreator) -> convId) $
          filter (\(_convId, team, mCreator) -> team == Just tid && mCreator == Just uid) filteredConvIds
    conversationsFilter :: PrepQuery R (Identity [ConvId]) (ConvId, Maybe TeamId, Maybe UserId)
    conversationsFilter = "select conv, team, creator from conversation where conv in ?"
    performUpdate convIds = retry x5 $ write conversationsUpdate (params LocalQuorum (Identity convIds))
    conversationsUpdate :: PrepQuery W (Identity [ConvId]) ()
    conversationsUpdate = "update conversation set deleted = true where conv in ?"
