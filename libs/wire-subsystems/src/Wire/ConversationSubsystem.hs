{-# LANGUAGE TemplateHaskell #-}

module Wire.ConversationSubsystem where

import Data.Id
import Data.Qualified
import Data.Singletons (Sing)
import Imports
import Polysemy
import Wire.API.Conversation (ExtraConversationData)
import Wire.API.Conversation.Action
import Wire.NotificationSubsystem (LocalConversationUpdate)
import Wire.StoredConversation

data ConversationSubsystem m a where
  NotifyConversationAction ::
    Sing tag ->
    Qualified UserId ->
    Bool ->
    Maybe ConnId ->
    Local StoredConversation ->
    Set UserId ->
    Set (Remote UserId) ->
    Set BotMember ->
    ConversationAction (tag :: ConversationActionTag) ->
    ExtraConversationData ->
    ConversationSubsystem r LocalConversationUpdate

makeSem ''ConversationSubsystem
