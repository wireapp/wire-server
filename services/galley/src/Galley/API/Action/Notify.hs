module Galley.API.Action.Notify where

import Data.Id
import Data.Qualified
import Data.Singletons
import Galley.API.Util
import Galley.Effects
import Imports hiding ((\\))
import Polysemy
import Wire.API.Conversation hiding (Conversation, Member)
import Wire.API.Conversation.Action
import Wire.ConversationSubsystem
import Wire.NotificationSubsystem
import Wire.StoredConversation

sendConversationActionNotifications ::
  forall tag r.
  ( Member ConversationSubsystem r
  ) =>
  Sing tag ->
  Qualified UserId ->
  Bool ->
  Maybe ConnId ->
  Local StoredConversation ->
  BotsAndMembers ->
  ConversationAction (tag :: ConversationActionTag) ->
  ExtraConversationData ->
  Sem r LocalConversationUpdate
sendConversationActionNotifications tag quid notifyOrigDomain con lconv targets action extraData = do
  notifyConversationAction
    tag
    quid
    notifyOrigDomain
    con
    lconv
    (bmLocals targets)
    (bmRemotes targets)
    (bmBots targets)
    action
    extraData
