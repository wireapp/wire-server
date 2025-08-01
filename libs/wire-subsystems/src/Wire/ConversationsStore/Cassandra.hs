module Wire.ConversationsStore.Cassandra (interpretConversationsStoreCassandra) where

import Cassandra
import Data.Id
import Imports
import Polysemy
import Polysemy.Embed
import Wire.ConversationsStore

interpretConversationsStoreCassandra :: (Member (Embed IO) r) => ClientState -> InterpreterFor ConversationsStore r
interpretConversationsStoreCassandra casClient =
  interpret $
    runEmbedded (runClient casClient) . embed . \case
      CloseConversationsFrom tid uid -> closeConversationsFromImpl tid uid

closeConversationsFromImpl :: TeamId -> UserId -> Client ()
closeConversationsFromImpl t u = retry x5 $ write conversationsUpdate (params LocalQuorum (t, Just u))
  where
    conversationsUpdate :: PrepQuery W (TeamId, Maybe UserId) ()
    conversationsUpdate = "UPDATE conversation SET deleted = true WHERE team = ? AND author = ?"
