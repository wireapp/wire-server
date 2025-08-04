{-# LANGUAGE TemplateHaskell #-}

module Wire.ConversationsSubsystem where

import Data.Id
import Polysemy

data ConversationsSubsystem m a where
  InternalCloseConversationsFrom :: TeamId -> UserId -> ConversationsSubsystem m ()

makeSem ''ConversationsSubsystem
