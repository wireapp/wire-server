{-# LANGUAGE TemplateHaskell #-}

module Wire.ConversationsSubsystem where

import Data.Id
import Polysemy
import Wire.API.Team.Conversation (LeavingConversations)

data ConversationsSubsystem m a where
  InternalLeavingConversationsFrom :: TeamId -> UserId -> ConversationsSubsystem m LeavingConversations

makeSem ''ConversationsSubsystem
