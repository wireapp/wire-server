{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.SubConversationStore where

import Data.Id
import Imports
import Polysemy
import Wire.API.Conversation.Protocol
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Group
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.SubConversation
import Wire.ConversationStore.MLS.Types

data SubConversationStore m a where
  CreateSubConversation :: ConvId -> SubConvId -> GroupId -> SubConversationStore m SubConversation
  GetSubConversation :: ConvId -> SubConvId -> SubConversationStore m (Maybe SubConversation)
  GetSubConversationGroupInfo :: ConvId -> SubConvId -> SubConversationStore m (Maybe GroupInfoData)
  GetSubConversationEpoch :: ConvId -> SubConvId -> SubConversationStore m (Maybe Epoch)
  SetSubConversationGroupInfo :: ConvId -> SubConvId -> Maybe GroupInfoData -> SubConversationStore m ()
  SetSubConversationEpoch :: ConvId -> SubConvId -> Epoch -> SubConversationStore m ()
  SetSubConversationCipherSuite :: ConvId -> SubConvId -> CipherSuiteTag -> SubConversationStore m ()
  ListSubConversations :: ConvId -> SubConversationStore m (Map SubConvId ConversationMLSData)
  DeleteSubConversation :: ConvId -> SubConvId -> SubConversationStore m ()

makeSem ''SubConversationStore
