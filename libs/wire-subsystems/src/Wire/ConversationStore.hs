{-# LANGUAGE TemplateHaskell #-}

module Wire.ConversationStore where

import Data.Id
import Polysemy
import Wire.StoredConversation (StoredConversation)

data ConversationStore m a where
  InsertConversation :: StoredConversation -> ConversationStore m ()
  GetConversation :: ConvId -> ConversationStore m StoredConversation

makeSem ''ConversationStore
