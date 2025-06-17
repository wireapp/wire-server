module Galley.API.Action.Notify where

import Data.Id
import Data.Qualified
import Data.Singletons
import Data.Time.Clock
import Galley.API.Util
import Galley.Data.Conversation
import Galley.Effects
import Galley.Effects.BackendNotificationQueueAccess
import Imports hiding ((\\))
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Conversation.Action
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.NotificationSubsystem

data LocalConversationUpdate = LocalConversationUpdate
  { lcuEvent :: Event,
    lcuUpdate :: ConversationUpdate
  }
  deriving (Show)

notifyConversationAction ::
  forall tag r.
  ( Member BackendNotificationQueueAccess r,
    Member ExternalAccess r,
    Member (Error FederationError) r,
    Member NotificationSubsystem r,
    Member (Input UTCTime) r
  ) =>
  Sing tag ->
  Qualified UserId ->
  Bool ->
  Maybe ConnId ->
  Local Conversation ->
  BotsAndMembers ->
  Maybe TeamId ->
  ConversationAction (tag :: ConversationActionTag) ->
  Sem r LocalConversationUpdate
notifyConversationAction tag quid notifyOrigDomain con lconv targets tid action = do
  now <- input
  let lcnv = fmap (.convId) lconv
      conv = tUnqualified lconv
      e = conversationActionToEvent tag now quid (tUntagged lcnv) Nothing tid action
      mkUpdate uids =
        ConversationUpdate
          now
          quid
          (tUnqualified lcnv)
          uids
          (SomeConversationAction tag action)
  update <-
    fmap (fromMaybe (mkUpdate []) . asum . map tUnqualified) $
      enqueueNotificationsConcurrently Q.Persistent (toList (bmRemotes targets)) $
        \ruids -> do
          let update = mkUpdate (tUnqualified ruids)
          -- if notifyOrigDomain is false, filter out user from quid's domain,
          -- because quid's backend will update local state and notify its users
          -- itself using the ConversationUpdate returned by this function
          if notifyOrigDomain || tDomain ruids /= qDomain quid
            then do
              makeConversationUpdateBundle update >>= sendBundle
              pure Nothing
            else pure (Just update)

  -- notify local participants and bots
  pushConversationEvent con conv e (qualifyAs lcnv (bmLocals targets)) (bmBots targets)

  -- return both the event and the 'ConversationUpdate' structure corresponding
  -- to the originating domain (if it is remote)
  pure $ LocalConversationUpdate e update
