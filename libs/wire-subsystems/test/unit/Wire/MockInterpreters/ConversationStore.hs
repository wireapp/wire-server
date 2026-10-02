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
import Wire.API.MLS.Group (GroupId)
import Wire.API.MLS.LeafNode (LeafIndex)
import Wire.ConversationStore (ConversationStore (..))
import Wire.ConversationStore.MLS.Types (ClientMap, IndexMap)
import Wire.StoredConversation (LocalMember (..), StoredConversation (..))

inMemoryConversationStoreInterpreter ::
  (Member (State [Qualified UserId]) r) =>
  Map.Map ConvId StoredConversation ->
  InterpreterFor ConversationStore r
inMemoryConversationStoreInterpreter store =
  inMemoryConversationStoreInterpreterWithMLS store mempty

inMemoryConversationStoreInterpreterWithMLS ::
  (Member (State [Qualified UserId]) r) =>
  Map.Map ConvId StoredConversation ->
  Map.Map GroupId (ClientMap LeafIndex, IndexMap) ->
  InterpreterFor ConversationStore r
inMemoryConversationStoreInterpreterWithMLS store mlsClients =
  interpret $ \case
    GetConversation cid -> pure (Map.lookup cid store)
    GetLocalMember cid uid ->
      pure $ do
        conv <- Map.lookup cid store
        find ((== uid) . (.id_)) conv.localMembers
    LookupMLSClientLeafIndices gid -> pure (fromMaybe (mempty, mempty) (Map.lookup gid mlsClients))
    SetOtherMember _ target _ -> modify @[(Qualified UserId)] (<> [target])
    _ -> error "ConversationStore: not implemented in mock"
