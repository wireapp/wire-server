module Wire.ConversationsStore.Cassandra
  ( interpretConversationsStoreCassandraOn,
    interpretConversationsStoreCassandra,
  )
where

import Cassandra
import Data.Id
import Imports
import Polysemy
import Polysemy.Input
import Wire.ConversationsStore

interpretConversationsStoreCassandraOn ::
  (Member (Embed IO) r) =>
  ClientState ->
  InterpreterFor ConversationsStore r
interpretConversationsStoreCassandraOn casClient =
  runInputConst casClient
    . interpretConversationsStoreCassandra
    . raiseUnder

interpretConversationsStoreCassandra ::
  ( Member (Embed IO) r,
    Member (Input ClientState) r
  ) =>
  InterpreterFor ConversationsStore r
interpretConversationsStoreCassandra =
  interpret $
    \case
      CloseConversationsFrom tid uid -> do
        cs <- input
        embed @IO $ runClient cs $ closeConversationsFromImpl tid uid

closeConversationsFromImpl :: TeamId -> UserId -> Client ()
closeConversationsFromImpl t u = retry x5 $ write conversationsUpdate (params LocalQuorum (t, Just u))
  where
    conversationsUpdate :: PrepQuery W (TeamId, Maybe UserId) ()
    conversationsUpdate = "update conversation set deleted = true where conv = ? and author = ?"

_closeConversationsFromImpl :: TeamId -> UserId -> Client ()
_closeConversationsFromImpl t u = retry x5 $ write conversationsUpdate (params LocalQuorum (t, Just u))
  where
    conversationsUpdate :: PrepQuery W (TeamId, Maybe UserId) ()
    conversationsUpdate = "update conversation set deleted = true where team = ? and author = ?"
