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

module Test.Wire.API.Golden.Manual.ConversationsResponse
  ( testObject_ConversationsResponse_1,
  )
where

import Data.Domain
import Data.Id (Id (Id))
import Data.Misc
import Data.Qualified
import qualified Data.UUID as UUID
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Role

domain :: Domain
domain = Domain "golden.example.com"

testObject_ConversationsResponse_1 :: ConversationsResponse
testObject_ConversationsResponse_1 =
  ConversationsResponse
    { crFound = [conv1, conv2],
      crNotFound =
        [ Qualified (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000002"))) domain,
          Qualified (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-111111111112"))) (Domain "golden2.example.com")
        ],
      crFailed =
        [ Qualified (Id (fromJust (UUID.fromString "00000018-4444-0020-0000-000e00000002"))) domain,
          Qualified (Id (fromJust (UUID.fromString "99999999-0000-0020-0000-111111111112"))) (Domain "golden3.example.com")
        ]
    }

conv1 :: Conversation
conv1 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) domain,
      cnvMetadata =
        ConversationMetadata
          { cnvmType = One2OneConv,
            cnvmCreator = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001")),
            cnvmAccess = [],
            cnvmAccessRole = PrivateAccessRole,
            cnvmName = Just " 0",
            cnvmTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))),
            cnvmMessageTimer = Nothing,
            cnvmReceiptMode = Just (ReceiptMode {unReceiptMode = -2})
          },
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) domain,
                  memService = Nothing,
                  memOtrMutedStatus = Nothing,
                  memOtrMutedRef = Nothing,
                  memOtrArchived = False,
                  memOtrArchivedRef = Just "",
                  memHidden = False,
                  memHiddenRef = Just "",
                  memConvRoleName = fromJust (parseRoleName "rhhdzf0j0njilixx0g0vzrp06b_5us")
                },
            cmOthers = []
          }
    }

conv2 :: Conversation
conv2 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))) domain,
      cnvMetadata =
        ConversationMetadata
          { cnvmType = SelfConv,
            cnvmCreator = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001")),
            cnvmAccess =
              [ InviteAccess,
                InviteAccess,
                CodeAccess,
                LinkAccess,
                InviteAccess,
                PrivateAccess,
                LinkAccess,
                CodeAccess,
                CodeAccess,
                LinkAccess,
                PrivateAccess,
                InviteAccess
              ],
            cnvmAccessRole = NonActivatedAccessRole,
            cnvmName = Just "",
            cnvmTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))),
            cnvmMessageTimer = Just (Ms {ms = 1319272593797015}),
            cnvmReceiptMode = Just (ReceiptMode {unReceiptMode = 2})
          },
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
                  memService = Nothing,
                  memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                  memOtrMutedRef = Nothing,
                  memOtrArchived = False,
                  memOtrArchivedRef = Nothing,
                  memHidden = True,
                  memHiddenRef = Just "",
                  memConvRoleName =
                    fromJust (parseRoleName "9b2d3thyqh4ptkwtq2n2v9qsni_ln1ca66et_z8dlhfs9oamp328knl3rj9kcj")
                },
            cmOthers = []
          }
    }
