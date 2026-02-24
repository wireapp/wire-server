-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MockInterpreters.ConversationSubsystem where

import Data.Default (def)
import Data.Id
import Data.Map qualified as Map
import Data.Qualified (tUnqualified)
import Data.Range (fromRange)
import Data.Set qualified as Set
import Imports
import Polysemy
import Polysemy.State
import Wire.API.Conversation qualified as Public
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Protocol (ConversationMLSData (..), Protocol (..))
import Wire.API.MLS.Group (GroupId (..))
import Wire.API.User (BaseProtocolTag (..))
import Wire.ConversationSubsystem
import Wire.Sem.Random (Random)
import Wire.Sem.Random qualified as Random
import Wire.StoredConversation

type ConversationMembers = Map ConvId (Set UserId)

inMemoryConversationSubsystemInterpreter ::
  (Member (State (Map ConvId StoredConversation)) r, Member (State ConversationMembers) r, Member Random r) =>
  InterpreterFor ConversationSubsystem r
inMemoryConversationSubsystemInterpreter = interpret $ \case
  CreateGroupConversation lusr _mconn newConv -> do
    cid <- Random.newId
    let conv =
          StoredConversation
            { id_ = cid,
              localMembers = [newMember (tUnqualified lusr)],
              remoteMembers = [],
              metadata =
                Public.ConversationMetadata
                  { cnvmType = Public.RegularConv,
                    cnvmCreator = Just (tUnqualified lusr),
                    cnvmAccess = [],
                    cnvmAccessRoles = def,
                    cnvmName = fromRange <$> newConv.newConvName,
                    cnvmMessageTimer = newConv.newConvMessageTimer,
                    cnvmReceiptMode = newConv.newConvReceiptMode,
                    cnvmTeam = Public.cnvTeamId <$> newConv.newConvTeam,
                    cnvmGroupConvType = Just newConv.newConvGroupConvType,
                    cnvmChannelAddPermission = newConv.newConvChannelAddPermission,
                    cnvmCellsState = if newConv.newConvCells then CellsPending else CellsDisabled,
                    cnvmParent = newConv.newConvParent,
                    cnvmHistory = newConv.newConvHistory
                  },
              protocol = case newConv.newConvProtocol of
                BaseProtocolProteusTag -> ProtocolProteus
                BaseProtocolMLSTag -> ProtocolMLS (ConversationMLSData (GroupId "mock-group-id") Nothing)
            }
    modify (Map.insert cid conv)
    modify (Map.insert cid (Set.singleton (tUnqualified lusr)))
    pure conv
  InternalGetLocalMember cid uid -> do
    members <- gets (Map.lookup cid)
    pure $ if Set.member uid (fromMaybe Set.empty members) then Just (newMember uid) else Nothing
  _ -> error "ConversationSubsystem: not implemented in mock"
