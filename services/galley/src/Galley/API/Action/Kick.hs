module Galley.API.Action.Kick where

import Data.Id
import Data.Qualified
import Data.Singletons
import Data.Time.Clock
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
    Member (Input UTCTime) r,
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
    Nothing
    (ConversationRemoveMembers (pure victim) EdReasonRemoved)
