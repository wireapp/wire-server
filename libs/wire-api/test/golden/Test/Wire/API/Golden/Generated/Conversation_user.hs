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

module Test.Wire.API.Golden.Generated.Conversation_user
  ( testObject_Conversation_user_1,
    testObject_Conversation_user_2,
    testObject_Conversation_user_3,
    testObject_Conversation_user_4,
    testObject_Conversation_user_5,
  )
where

import Data.Domain
import Data.Id (Id (Id))
import Data.Misc (Milliseconds (Ms, ms))
import Data.Qualified
import Data.Set qualified as Set
import Data.Time.Calendar
import Data.Time.Clock
import Data.UUID qualified as UUID (fromString)
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.CellsState
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.MLS.CipherSuite
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))

domain :: Domain
domain = Domain "golden.example.com"

testObject_Conversation_user_1 :: ConversationV8
testObject_Conversation_user_1 =
  ConversationV8
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) domain,
      cnvMetadata =
        ConversationMetadata
          { cnvmType = One2OneConv,
            cnvmCreator = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))),
            cnvmAccess = [],
            cnvmAccessRoles = Set.empty,
            cnvmName = Just " 0",
            cnvmTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))),
            cnvmMessageTimer = Nothing,
            cnvmReceiptMode = Just (ReceiptMode {unReceiptMode = -2}),
            cnvmGroupConvType = Nothing,
            cnvmChannelAddPermission = Nothing,
            cnvmCellsState = CellsReady
          },
      cnvProtocol = ProtocolProteus,
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

testObject_Conversation_user_2 :: ConversationV8
testObject_Conversation_user_2 =
  ConversationV8
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))) domain,
      cnvMetadata =
        ConversationMetadata
          { cnvmType = SelfConv,
            cnvmCreator = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),
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
            cnvmAccessRoles = Set.fromList [TeamMemberAccessRole, GuestAccessRole, ServiceAccessRole],
            cnvmName = Just "",
            cnvmTeam = Nothing,
            cnvmMessageTimer = Just (Ms {ms = 1319272593797015}),
            cnvmReceiptMode = Nothing,
            cnvmGroupConvType = Nothing,
            cnvmChannelAddPermission = Nothing,
            cnvmCellsState = CellsPending
          },
      cnvProtocol = ProtocolProteus,
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
            cmOthers =
              [ OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
                              _serviceRefProvider =
                                Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))
                            }
                        ),
                    omConvRoleName =
                      fromJust
                        ( parseRoleName
                            "r1rg526serx51g15n99y1bw_9q0qrcwck3jxl7ocjsjqcoux7d1zbkz9nnczy92t2oyogxrx3cyh_b8yv44l61mx9uzdnv6"
                        )
                  }
              ]
          }
    }

testObject_Conversation_user_3 :: ConversationV8
testObject_Conversation_user_3 =
  ConversationV8
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))) domain,
      cnvMetadata =
        ConversationMetadata
          { cnvmType = SelfConv,
            cnvmCreator = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),
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
            cnvmAccessRoles = Set.fromList [TeamMemberAccessRole, GuestAccessRole, ServiceAccessRole],
            cnvmName = Just "",
            cnvmTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))),
            cnvmMessageTimer = Just (Ms {ms = 1319272593797015}),
            cnvmReceiptMode = Just (ReceiptMode {unReceiptMode = 2}),
            cnvmGroupConvType = Nothing,
            cnvmChannelAddPermission = Nothing,
            cnvmCellsState = CellsDisabled
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
          },
      cnvProtocol =
        ProtocolMLS
          ( ConversationMLSData
              (GroupId "test_group")
              ( Just
                  ( ActiveMLSConversationData
                      (Epoch 42)
                      timestamp
                      MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
                  )
              )
          )
    }
  where
    timestamp :: UTCTime
    timestamp = UTCTime (fromGregorian 2023 1 17) (secondsToDiffTime 42)

testObject_Conversation_user_4 :: ConversationV8
testObject_Conversation_user_4 =
  ConversationV8
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))) domain,
      cnvMetadata =
        ConversationMetadata
          { cnvmType = SelfConv,
            cnvmCreator = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),
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
            cnvmAccessRoles = Set.fromList [TeamMemberAccessRole, GuestAccessRole, ServiceAccessRole],
            cnvmName = Just "",
            cnvmTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))),
            cnvmMessageTimer = Just (Ms {ms = 1319272593797015}),
            cnvmReceiptMode = Just (ReceiptMode {unReceiptMode = 2}),
            cnvmGroupConvType = Nothing,
            cnvmChannelAddPermission = Nothing,
            cnvmCellsState = CellsDisabled
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
          },
      cnvProtocol =
        ProtocolMLS
          ( ConversationMLSData
              (GroupId "test_group")
              Nothing
          )
    }

testObject_Conversation_user_5 :: ConversationV8
testObject_Conversation_user_5 =
  ConversationV8
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) domain,
      cnvMetadata =
        ConversationMetadata
          { cnvmType = One2OneConv,
            cnvmCreator = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))),
            cnvmAccess = [],
            cnvmAccessRoles = Set.empty,
            cnvmName = Just " 0",
            cnvmTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))),
            cnvmMessageTimer = Nothing,
            cnvmReceiptMode = Just (ReceiptMode {unReceiptMode = -2}),
            cnvmGroupConvType = Nothing,
            cnvmChannelAddPermission = Nothing,
            cnvmCellsState = CellsDisabled
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
          },
      cnvProtocol = ProtocolProteus
    }
