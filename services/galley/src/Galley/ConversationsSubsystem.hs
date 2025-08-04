module Galley.ConversationsSubsystem
  ( interpretConversationsSubsystemCassandra,
  )
where

import Cassandra
import Data.Id
import Galley.Cassandra.Store (embedClient)
import Galley.Cassandra.Util (logEffect)
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Wire.ConversationsSubsystem

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
closeConversationsFromImpl t u = retry x5 $ write conversationsUpdate (params LocalQuorum (t, Just u))
  where
    conversationsUpdate :: PrepQuery W (TeamId, Maybe UserId) ()
    conversationsUpdate = "update conversation set deleted = true where team = ? and creator = ?"
