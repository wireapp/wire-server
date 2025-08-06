{-# LANGUAGE TemplateHaskell #-}

module Wire.ConversationSubsystem where

import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.Conversation

data ConversationSubsystem m a where
  CreateGroupConversation :: Local UserId -> Maybe ConnId -> NewConv -> ConversationSubsystem m CreateGroupConversation
  GetConversation :: Local UserId -> Qualified ConvId -> ConversationSubsystem m Conversation

makeSem ''ConversationSubsystem
