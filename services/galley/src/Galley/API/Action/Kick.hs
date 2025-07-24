module Galley.API.Action.Kick where

import Data.Default
import Data.Id
import Data.Qualified
import Data.Singletons
import Galley.API.Action.Leave
import Galley.API.Action.Notify
import Galley.API.Util
import Galley.Data.Conversation
import Galley.Effects
import Galley.Env (Env)
import Imports hiding ((\\))
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation.Action
import Wire.API.Event.LeaveReason
import Wire.API.Federation.Error
import Wire.NotificationSubsystem
import Wire.Sem.Now (Now)

-- | Kick a user from a conversation and send notifications.
--
-- This function removes the given victim from the conversation by making them
-- leave, but then sends notifications as if the user was removed by someone
-- else.
kickMember ::
  ( Member BackendNotificationQueueAccess r,
    Member (Error FederationError) r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member ProposalStore r,
    Member Now r,
    Member (Input Env) r,
    Member MemberStore r,
    Member SubConversationStore r,
    Member TinyLog r,
    Member Random r
  ) =>
  Qualified UserId ->
  Local Conversation ->
  BotsAndMembers ->
  Qualified UserId ->
  Sem r ()
kickMember qusr lconv targets victim = void . runError @NoChanges $ do
  leaveConversation victim lconv
  notifyConversationAction
    (sing @'ConversationRemoveMembersTag)
    qusr
    True
    Nothing
    lconv
    targets
    (ConversationRemoveMembers (pure victim) EdReasonRemoved)
    def
