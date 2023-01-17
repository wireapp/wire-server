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

module Galley.Effects.SubConversationStore where

import Data.Id
import Data.Qualified
import Galley.API.MLS.Types
import Imports
import Polysemy
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Epoch
import Wire.API.MLS.Group
import Wire.API.MLS.PublicGroupState
import Wire.API.MLS.SubConversation

data SubConversationStore m a where
  CreateSubConversation :: ConvId -> SubConvId -> CipherSuiteTag -> Epoch -> GroupId -> Maybe OpaquePublicGroupState -> SubConversationStore m ()
  GetSubConversation :: ConvId -> SubConvId -> SubConversationStore m (Maybe SubConversation)
  GetSubConversationPublicGroupState :: ConvId -> SubConvId -> SubConversationStore m (Maybe OpaquePublicGroupState)
  SetSubConversationPublicGroupState :: ConvId -> SubConvId -> Maybe OpaquePublicGroupState -> SubConversationStore m ()
  SetGroupIdForSubConversation :: GroupId -> Qualified ConvId -> SubConvId -> SubConversationStore m ()
  SetSubConversationEpoch :: ConvId -> SubConvId -> Epoch -> SubConversationStore m ()
  DeleteGroupIdForSubConversation :: GroupId -> SubConversationStore m ()

makeSem ''SubConversationStore
