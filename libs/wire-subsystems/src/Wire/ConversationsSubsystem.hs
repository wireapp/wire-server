{-# LANGUAGE TemplateHaskell #-}

module Wire.ConversationsSubsystem where

import Data.Id
import Polysemy
import Wire.API.Team.Conversation (LeftConversations)

data ConversationsSubsystem m a where
  InternalLeaveConversationsFrom :: TeamId -> UserId -> ConversationsSubsystem m LeftConversations

makeSem ''ConversationsSubsystem
