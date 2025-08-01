{-# LANGUAGE TemplateHaskell #-}

module Wire.ConversationsStore where

import Data.Id
import Polysemy

data ConversationsStore m a where
  CloseConversationsFrom :: TeamId -> UserId -> ConversationsStore m ()

makeSem ''ConversationsStore
