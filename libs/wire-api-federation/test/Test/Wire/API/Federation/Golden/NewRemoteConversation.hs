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

module Test.Wire.API.Federation.Golden.NewRemoteConversation where

import Data.Domain
import Data.Id
import Data.Misc
import Data.Qualified
import qualified Data.Set as Set
import qualified Data.UUID as UUID
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Role
import Wire.API.Federation.API.Galley
import Wire.API.Provider.Service

testObject_NewRemoteConversation1 :: NewRemoteConversation ConvId
testObject_NewRemoteConversation1 =
  NewRemoteConversation
    { rcTime = read "1864-04-12 12:22:43.673 UTC",
      rcOrigUserId = Id (fromJust (UUID.fromString "eed9dea3-5468-45f8-b562-7ad5de2587d0")),
      rcCnvId = Id (fromJust (UUID.fromString "d13dbe58-d4e3-450f-9c0c-1e632f548740")),
      rcCnvType = RegularConv,
      rcCnvAccess = [InviteAccess, CodeAccess],
      rcCnvAccessRoles = Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole],
      rcCnvName = Just "gossip",
      rcNonCreatorMembers =
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
      rcMessageTimer = Just (Ms 1000),
      rcReceiptMode = Just (ReceiptMode 42),
      rcProtocol = Just ProtocolProteus,
      rcGroupId = Nothing
    }

testObject_NewRemoteConversation2 :: NewRemoteConversation ConvId
testObject_NewRemoteConversation2 =
  NewRemoteConversation
    { rcTime = read "1864-04-12 12:22:43.673 UTC",
      rcOrigUserId = Id (fromJust (UUID.fromString "eed9dea3-5468-45f8-b562-7ad5de2587d0")),
      rcCnvId = Id (fromJust (UUID.fromString "d13dbe58-d4e3-450f-9c0c-1e632f548740")),
      rcCnvType = One2OneConv,
      rcCnvAccess = [],
      rcCnvAccessRoles = Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole],
      rcCnvName = Nothing,
      rcNonCreatorMembers = Set.fromList [],
      rcMessageTimer = Nothing,
      rcReceiptMode = Nothing,
      rcProtocol = Just ProtocolMLS,
      rcGroupId = Just groupId
    }
  where
    groupId :: GroupId
    groupId =
      convIdToGroupId $
        Qualified
          (Id (fromJust (UUID.fromString "d13dbe58-d4e3-450f-9c0c-1e632f548740")))
          (Domain "mydomain.com")
