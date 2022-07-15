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

module Test.Wire.API.Federation.Golden.ConversationCreated where

import Data.Domain
import Data.Id
import Data.Misc
import Data.Qualified
import qualified Data.Set as Set
import qualified Data.UUID as UUID
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Federation.API.Galley
import Wire.API.MLS.CipherSuite
import Wire.API.Provider.Service

testObject_ConversationCreated1 :: ConversationCreated ConvId
testObject_ConversationCreated1 =
  ConversationCreated
    { ccTime = read "1864-04-12 12:22:43.673 UTC",
      ccOrigUserId = Id (fromJust (UUID.fromString "eed9dea3-5468-45f8-b562-7ad5de2587d0")),
      ccCnvId = Id (fromJust (UUID.fromString "d13dbe58-d4e3-450f-9c0c-1e632f548740")),
      ccCnvType = RegularConv,
      ccCnvAccess = [InviteAccess, CodeAccess],
      ccCnvAccessRoles = Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole],
      ccCnvName = Just "gossip",
      ccNonCreatorMembers =
        Set.fromList
          [ OtherMember
              { omQualifiedId =
                  Qualified
                    (read "50e6fff1-ffbd-4235-bc73-19c093433beb")
                    (Domain "golden.example.com"),
                omService = Nothing,
                omConvRoleName = roleNameWireAdmin
              },
            OtherMember
              { omQualifiedId =
                  Qualified
                    (read "6801e49b-918c-4eef-baed-f18522152fca")
                    (Domain "golden.example.com"),
                omService =
                  Just
                    ( ServiceRef
                        { _serviceRefId = read "abfe2452-ed22-4f94-b4d4-765b989d7dbb",
                          _serviceRefProvider = read "11b91f61-917e-489b-a268-60b881d08f06"
                        }
                    ),
                omConvRoleName = roleNameWireMember
              }
          ],
      ccMessageTimer = Just (Ms 1000),
      ccReceiptMode = Just (ReceiptMode 42),
      ccProtocol = ProtocolProteus
    }

testObject_ConversationCreated2 :: ConversationCreated ConvId
testObject_ConversationCreated2 =
  ConversationCreated
    { ccTime = read "1864-04-12 12:22:43.673 UTC",
      ccOrigUserId = Id (fromJust (UUID.fromString "eed9dea3-5468-45f8-b562-7ad5de2587d0")),
      ccCnvId = Id (fromJust (UUID.fromString "d13dbe58-d4e3-450f-9c0c-1e632f548740")),
      ccCnvType = One2OneConv,
      ccCnvAccess = [],
      ccCnvAccessRoles = Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole],
      ccCnvName = Nothing,
      ccNonCreatorMembers = Set.fromList [],
      ccMessageTimer = Nothing,
      ccReceiptMode = Nothing,
      ccProtocol = ProtocolMLS (ConversationMLSData (GroupId "group") (Epoch 3) MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519)
    }
