-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU Affero General Public License as published by the
-- Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.MockInterpreters.ConversationStore where

import Data.Id (ConvId, UserId)
import Data.Map qualified as Map
import Data.Qualified (Qualified)
import Imports
import Polysemy
import Polysemy.State
import Wire.ConversationStore (ConversationStore (..))
import Wire.StoredConversation (StoredConversation)

inMemoryConversationStoreInterpreter ::
  (Member (State [Qualified UserId]) r) =>
  Map.Map ConvId StoredConversation ->
  InterpreterFor ConversationStore r
inMemoryConversationStoreInterpreter store =
  interpret $ \case
    GetConversation cid -> pure (Map.lookup cid store)
    SetOtherMember _ target _ -> modify @[(Qualified UserId)] (<> [target])
    _ -> error "ConversationStore: not implemented in mock"
