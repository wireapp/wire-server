-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.API.Pydio
  ( HasPydioState (..),
    shouldPushToPydio,
  )
where

import Data.Default
import Galley.Data.Conversation qualified as Data
import Imports
import Wire.API.Conversation
import Wire.API.Conversation qualified as Public
import Wire.API.Conversation.PydioState
import Wire.API.Event.Conversation

class HasPydioState a where
  getPydioState :: a -> PydioState

instance HasPydioState PydioState where
  getPydioState = id

instance HasPydioState Data.Conversation where
  getPydioState = getPydioState . Data.convMetadata

instance HasPydioState Public.Conversation where
  getPydioState = getPydioState . Public.cnvMetadata

instance HasPydioState ConversationMetadata where
  getPydioState = cnvmPydioState

instance HasPydioState () where
  getPydioState = def

shouldPushToPydio :: (HasPydioState a) => a -> EventType -> Bool
shouldPushToPydio st et =
  isPydioConversationEvent et && getPydioState st == PydioReady
