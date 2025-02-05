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
import Data.Set qualified as Set
import Data.Time.Calendar
import Data.Time.Clock
import Data.UUID qualified as UUID
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Federation.API.Galley
import Wire.API.MLS.CipherSuite
import Wire.API.Provider.Service

testObject_ConversationCreated1 :: ConversationCreated ConvId
testObject_ConversationCreated1 =
  ConversationCreated
    { time = read "1864-04-12 12:22:43.673 UTC",
      origUserId = Id (fromJust (UUID.fromString "eed9dea3-5468-45f8-b562-7ad5de2587d0")),
      cnvId = Id (fromJust (UUID.fromString "d13dbe58-d4e3-450f-9c0c-1e632f548740")),
      cnvType = RegularConv,
      cnvAccess = [InviteAccess, CodeAccess],
      cnvAccessRoles = Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole],
      cnvName = Just "gossip",
      nonCreatorMembers =
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
      messageTimer = Just (Ms 1000),
      receiptMode = Just (ReceiptMode 42),
      protocol = ProtocolProteus,
      groupConvType = Just GroupConversation,
      cellsState = Nothing
    }

testObject_ConversationCreated2 :: ConversationCreated ConvId
testObject_ConversationCreated2 =
  ConversationCreated
    { time = read "1864-04-12 12:22:43.673 UTC",
      origUserId = Id (fromJust (UUID.fromString "eed9dea3-5468-45f8-b562-7ad5de2587d0")),
      cnvId = Id (fromJust (UUID.fromString "d13dbe58-d4e3-450f-9c0c-1e632f548740")),
      cnvType = One2OneConv,
      cnvAccess = [],
      cnvAccessRoles = Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole],
      cnvName = Nothing,
      nonCreatorMembers = Set.fromList [],
      messageTimer = Nothing,
      receiptMode = Nothing,
      protocol =
        ProtocolMLS
          ( ConversationMLSData
              (GroupId "group")
              ( Just
                  ( ActiveMLSConversationData
                      (Epoch 3)
                      (UTCTime (fromGregorian 2020 8 29) 0)
                      MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
                  )
              )
          ),
      groupConvType = Nothing,
      cellsState = Just CellsPending
    }
