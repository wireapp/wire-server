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

module Galley.API.Cells
  ( HasCellsState (..),
    shouldPushToCells,
  )
where

import Data.Default
import Galley.Data.Conversation qualified as Data
import Imports
import Wire.API.Conversation
import Wire.API.Conversation qualified as Public
import Wire.API.Conversation.CellsState
import Wire.API.Event.Conversation

class HasCellsState a where
  getCellsState :: a -> CellsState

instance HasCellsState CellsState where
  getCellsState = id

instance HasCellsState Data.Conversation where
  getCellsState = getCellsState . Data.convMetadata

instance HasCellsState Public.ConversationV8 where
  getCellsState = getCellsState . Public.cnvMetadata

instance HasCellsState ConversationMetadata where
  getCellsState = cnvmCellsState

instance HasCellsState () where
  getCellsState = def

shouldPushToCells :: (HasCellsState a) => a -> Event -> Bool
shouldPushToCells st e =
  isCellsConversationEvent e && case getCellsState st of
    CellsDisabled -> False
    CellsPending -> True
    CellsReady -> True
