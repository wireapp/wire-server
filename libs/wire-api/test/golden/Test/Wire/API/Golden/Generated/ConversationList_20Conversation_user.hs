{-# LANGUAGE OverloadedLists #-}

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

module Test.Wire.API.Golden.Generated.ConversationList_20Conversation_user where

import Data.Domain (Domain (..))
import Data.Id (Id (Id))
import Data.Misc (Milliseconds (Ms, ms))
import Data.Qualified (Qualified (..))
import Data.Set qualified as Set
import Data.UUID qualified as UUID (fromString)
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role (parseRoleName)

domain :: Domain
domain = Domain "golden.example.com"

testObject_ConversationList_20Conversation_user_1 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_1 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) (Domain "golden.example.com"),
              cnvMetadata =
                ConversationMetadata
                  { cnvmType = RegularConv,
                    cnvmCreator = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                    cnvmAccess = [],
                    cnvmAccessRoles = Set.empty,
                    cnvmName = Just "",
                    cnvmTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                    cnvmMessageTimer = Just (Ms {ms = 4760386328981119}),
                    cnvmReceiptMode = Just (ReceiptMode {unReceiptMode = 0}),
                    cnvmGroupConvType = Just GroupConversation
                  },
              cnvProtocol = ProtocolProteus,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) domain,
                          memService = Nothing,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Nothing,
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            fromJust (parseRoleName "71xuphsrwfoktrpiv4d08dxj6_1umizg67iisctw87gemvi114mtu")
                        },
                    cmOthers = []
                  }
            }
        ],
      convHasMore = False
    }

testObject_ConversationList_20Conversation_user_2 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_2 =
  ConversationList
    { convList = [],
      convHasMore = False
    }
