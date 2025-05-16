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

module Galley.API.MLS.One2One
  ( localMLSOne2OneConversation,
    localMLSOne2OneConversationAsRemote,
    localMLSOne2OneConversationMetadata,
    remoteMLSOne2OneConversation,
    createMLSOne2OneConversation,
  )
where

import Data.Id as Id
import Data.Qualified
import Galley.API.MLS.Types
import Galley.Data.Conversation.Types qualified as Data
import Galley.Effects.ConversationStore
import Galley.Types.UserList
import Imports
import Polysemy
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Federation.API.Galley
import Wire.API.MLS.Group.Serialisation
import Wire.API.MLS.Keys
import Wire.API.MLS.SubConversation
import Wire.API.User

-- | Construct a local MLS 1-1 'Conversation' between a local user and another
-- (possibly remote) user.
localMLSOne2OneConversation ::
  Local UserId ->
  Local ConvId ->
  ConversationV8
localMLSOne2OneConversation lself (tUntagged -> convId) =
  let members =
        ConvMembers
          { cmSelf = defMember (tUntagged lself),
            cmOthers = []
          }
      (metadata, mlsData) = localMLSOne2OneConversationMetadata convId
   in ConversationV8
        { cnvQualifiedId = convId,
          cnvMetadata = metadata,
          cnvMembers = members,
          cnvProtocol = ProtocolMLS mlsData
        }

-- | Construct a 'RemoteConversation' structure for a local MLS 1-1
-- conversation to be returned to a remote backend.
localMLSOne2OneConversationAsRemote ::
  Local ConvId ->
  RemoteConversationV2
localMLSOne2OneConversationAsRemote lcnv =
  let members =
        RemoteConvMembers
          { selfRole = roleNameWireMember,
            others = []
          }
      (metadata, mlsData) = localMLSOne2OneConversationMetadata (tUntagged lcnv)
   in RemoteConversationV2
        { id = tUnqualified lcnv,
          metadata = metadata,
          members = members,
          protocol = ProtocolMLS mlsData
        }

localMLSOne2OneConversationMetadata ::
  Qualified ConvId ->
  (ConversationMetadata, ConversationMLSData)
localMLSOne2OneConversationMetadata convId =
  let metadata =
        (defConversationMetadata Nothing)
          { cnvmType = One2OneConv
          }
      groupId = convToGroupId $ groupIdParts One2OneConv (fmap Conv convId)
      mlsData =
        ConversationMLSData
          { cnvmlsGroupId = groupId,
            cnvmlsActiveData = Nothing
          }
   in (metadata, mlsData)

-- | Convert an MLS 1-1 conversation returned by a remote backend into a
-- 'Conversation' to be returned to the client.
remoteMLSOne2OneConversation ::
  Local UserId ->
  Remote UserId ->
  RemoteMLSOne2OneConversation ->
  (MLSOne2OneConversation MLSPublicKey)
remoteMLSOne2OneConversation lself rother rc =
  let members =
        ConvMembers
          { cmSelf = defMember (tUntagged lself),
            cmOthers = rc.conversation.members.others
          }
      conv =
        ConversationV8
          { cnvQualifiedId = tUntagged (qualifyAs rother rc.conversation.id),
            cnvMetadata = rc.conversation.metadata,
            cnvMembers = members,
            cnvProtocol = rc.conversation.protocol
          }
   in MLSOne2OneConversation
        { conversation = conv,
          publicKeys = rc.publicKeys
        }

-- | Create a new record for an MLS 1-1 conversation in the database and add
-- the two members to it.
createMLSOne2OneConversation ::
  (Member ConversationStore r) =>
  Qualified UserId ->
  Qualified UserId ->
  Local MLSConversation ->
  Sem r Data.Conversation
createMLSOne2OneConversation self other lconv = do
  createConversation
    (fmap mcId lconv)
    Data.NewConversation
      { ncMetadata = mcMetadata (tUnqualified lconv),
        ncUsers = fmap (,roleNameWireMember) (toUserList lconv [self, other]),
        ncProtocol = BaseProtocolMLSTag
      }
