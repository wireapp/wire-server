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
    remoteMLSOne2OneConversation,
  )
where

import Data.Id as Id
import Data.Qualified
import Imports hiding (cs)
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Federation.API.Galley
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.Group.Serialisation
import Wire.API.MLS.SubConversation

-- | Construct a local MLS 1-1 'Conversation' between a local user and another
-- (possibly remote) user.
localMLSOne2OneConversation ::
  Local UserId ->
  Qualified UserId ->
  Local ConvId ->
  Conversation
localMLSOne2OneConversation lself qother (tUntagged -> convId) =
  let members =
        ConvMembers
          { cmSelf = defMember (tUntagged lself),
            cmOthers = [defOtherMember qother]
          }
      (metadata, protocol) = localMLSOne2OneConversationMetadata convId
   in Conversation
        { cnvQualifiedId = convId,
          cnvMetadata = metadata,
          cnvMembers = members,
          cnvProtocol = protocol
        }

-- | Construct a 'RemoteConversation' structure for a local MLS 1-1
-- conversation to be returned to a remote backend.
localMLSOne2OneConversationAsRemote ::
  Local UserId ->
  Local ConvId ->
  RemoteConversation
localMLSOne2OneConversationAsRemote lother lcnv =
  let members =
        RemoteConvMembers
          { rcmSelfRole = roleNameWireMember,
            rcmOthers = [defOtherMember (tUntagged lother)]
          }
      (metadata, protocol) = localMLSOne2OneConversationMetadata (tUntagged lcnv)
   in RemoteConversation
        { rcnvId = tUnqualified lcnv,
          rcnvMetadata = metadata,
          rcnvMembers = members,
          rcnvProtocol = protocol
        }

localMLSOne2OneConversationMetadata ::
  Qualified ConvId ->
  (ConversationMetadata, Protocol)
localMLSOne2OneConversationMetadata convId =
  let metadata =
        (defConversationMetadata Nothing)
          { cnvmType = One2OneConv
          }
      groupId = convToGroupId $ groupIdParts One2OneConv (fmap Conv convId)
      mlsData =
        ConversationMLSData
          { cnvmlsGroupId = groupId,
            cnvmlsEpoch = Epoch 0,
            cnvmlsEpochTimestamp = Nothing,
            cnvmlsCipherSuite = MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
          }
   in (metadata, ProtocolMLS mlsData)

-- | Convert an MLS 1-1 conversation returned by a remote backend into a
-- 'Conversation' to be returned to the client.
remoteMLSOne2OneConversation ::
  Local UserId ->
  Remote UserId ->
  RemoteConversation ->
  Conversation
remoteMLSOne2OneConversation lself rother rc =
  let members =
        ConvMembers
          { cmSelf = defMember (tUntagged lself),
            cmOthers = [defOtherMember (tUntagged rother)]
          }
   in Conversation
        { cnvQualifiedId = tUntagged (qualifyAs rother (rcnvId rc)),
          cnvMetadata = rcnvMetadata rc,
          cnvMembers = members,
          cnvProtocol = rcnvProtocol rc
        }
