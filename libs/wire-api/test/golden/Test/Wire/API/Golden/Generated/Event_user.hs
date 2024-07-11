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

module Test.Wire.API.Golden.Generated.Event_user where

import Data.Domain
import Data.Id
import Data.Misc (Milliseconds (Ms, ms))
import Data.Qualified
import Data.Range (unsafeRange)
import Data.Set qualified as Set
import Data.Text.Ascii (validate)
import Data.UUID qualified as UUID (fromString)
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Code
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Conversation.Typing
import Wire.API.Event.Conversation
import Wire.API.Event.LeaveReason
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))

domain :: Domain
domain = Domain "golden.example.com"

testObject_Event_user_1 :: Event
testObject_Event_user_1 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "00005d81-0000-0d71-0000-1d8f00007d32"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "00003b8b-0000-3395-0000-076a00007830"))) (Domain "faraway.example.com"))
    (read "1864-05-22 09:51:07.104 UTC")
    EdConvDelete

testObject_Event_user_2 :: Event
testObject_Event_user_2 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "0000064d-0000-7a7f-0000-5749000029e1"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "00006a88-0000-2acb-0000-6aa0000061b2"))) (Domain "faraway.example.com"))
    (read "1864-06-05 23:01:18.769 UTC")
    ( EdConvAccessUpdate
        ( ConversationAccessData
            { cupAccess = [InviteAccess, LinkAccess, PrivateAccess, InviteAccess, InviteAccess],
              cupAccessRoles = Set.fromList [TeamMemberAccessRole, GuestAccessRole]
            }
        )
    )

testObject_Event_user_3 :: Event
testObject_Event_user_3 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "00006f8c-0000-00d6-0000-1568000001e9"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "00004b11-0000-5504-0000-55d800002188"))) (Domain "faraway.example.com"))
    (read "1864-04-27 15:44:23.844 UTC")
    ( EdOtrMessage
        ( OtrMessage
            { otrSender = ClientId 0xc,
              otrRecipient = ClientId 0xf,
              otrCiphertext = "",
              otrData = Just ">\33032\SI\30584"
            }
        )
    )

testObject_Event_user_4 :: Event
testObject_Event_user_4 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "00004f04-0000-3939-0000-472d0000316b"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "00007c90-0000-766a-0000-01b700002ab7"))) (Domain "faraway.example.com"))
    (read "1864-05-12 00:59:09.2 UTC")
    EdConvCodeDelete

testObject_Event_user_5 :: Event
testObject_Event_user_5 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "00003c8c-0000-6394-0000-294b0000098b"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "00002a12-0000-73e1-0000-71f700002ec9"))) (Domain "faraway.example.com"))
    (read "1864-04-12 03:04:00.298 UTC")
    ( EdMemberUpdate
        ( MemberUpdateData
            { misTarget =
                Qualified
                  (Id (fromJust (UUID.fromString "afb0e5b1-c554-4ce4-98f5-3e1671f22485")))
                  (Domain "target.example.com"),
              misOtrMutedStatus = Nothing,
              misOtrMutedRef = Just "\94957",
              misOtrArchived = Just False,
              misOtrArchivedRef = Just "\SOHJ",
              misHidden = Nothing,
              misHiddenRef = Just "\b\t\CAN",
              misConvRoleName =
                Just
                  ( fromJust
                      (parseRoleName "_smrwzjjyq92t3t9u1pettcfiga699uz98rpzdt4lviu8x9iv1di4uiebz2gmrxor2_g0mfzzsfonqvc")
                  )
            }
        )
    )

testObject_Event_user_6 :: Event
testObject_Event_user_6 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "00001fdb-0000-3127-0000-23ef00007183"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "0000705a-0000-0b62-0000-425c000049c8"))) (Domain "faraway.example.com"))
    (read "1864-05-09 05:44:41.382 UTC")
    (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 5029817038083912})}))

testObject_Event_user_7 :: Event
testObject_Event_user_7 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "00006ac1-0000-543e-0000-7c8f00000be7"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "0000355a-0000-2979-0000-083000002d5e"))) (Domain "faraway.example.com"))
    (read "1864-04-18 05:01:13.761 UTC")
    (EdTyping StoppedTyping)

testObject_Event_user_8 :: Event
testObject_Event_user_8 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "000019e1-0000-1dc6-0000-68de0000246d"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "00000457-0000-0689-0000-77a00000021c"))) (Domain "faraway.example.com"))
    (read "1864-05-29 19:31:31.226 UTC")
    ( EdConversation
        ( Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) (Domain "golden.example.com"),
              cnvMetadata =
                ConversationMetadata
                  { cnvmType = RegularConv,
                    cnvmCreator = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),
                    cnvmAccess =
                      [InviteAccess, PrivateAccess, LinkAccess, InviteAccess, InviteAccess, InviteAccess, LinkAccess],
                    cnvmAccessRoles = Set.fromList [TeamMemberAccessRole, GuestAccessRole, ServiceAccessRole],
                    cnvmName = Just "\a\SO\r",
                    cnvmTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))),
                    cnvmMessageTimer = Just (Ms {ms = 283898987885780}),
                    cnvmReceiptMode = Just (ReceiptMode {unReceiptMode = -1}),
                    cnvmGroupColor = Nothing,
                    cnvmGroupIcon = Nothing
                  },
              cnvProtocol = ProtocolProteus,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) (Domain "golden.example.com"),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
                                    _serviceRefProvider =
                                      Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))
                                  }
                              ),
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            fromJust
                              (parseRoleName "kf_7rcnb2oilvdmd9nelmwf52gikr4aqkhktyn5vjzg7lq1dnzym812q1innmegmx9a")
                        },
                    cmOthers =
                      [ OtherMember
                          { omQualifiedId =
                              Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
                            omService = Nothing,
                            omConvRoleName =
                              fromJust
                                ( parseRoleName
                                    "4190csbyn6n7ooa8w4d7y9na9_a4m5hgvvmfnowu9zib_29nepamxsxl0gvq2hrfzp7obu_mtj43j0rd38jyd9r5j7xvf2ujge7s0pnt43g9cyal_ak2alwyf8uda"
                                )
                          }
                      ]
                  }
            }
        )
    )

testObject_Event_user_9 :: Event
testObject_Event_user_9 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "00000b98-0000-618d-0000-19e200004651"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "00004bee-0000-45a0-0000-2c0300005726"))) (Domain "faraway.example.com"))
    (read "1864-05-01 11:57:35.123 UTC")
    (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -10505}}))

testObject_Event_user_10 :: Event
testObject_Event_user_10 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "00005e43-0000-3b56-0000-7c270000538c"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "00007f28-0000-40b1-0000-56ab0000748d"))) (Domain "faraway.example.com"))
    (read "1864-05-25 01:31:49.802 UTC")
    ( EdConnect
        ( Connect
            { cRecipient =
                Qualified
                  (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000600000001")))
                  (Domain "faraway.example.com"),
              cMessage = Just "L",
              cName = Just "fq",
              cEmail = Just "\992986"
            }
        )
    )

testObject_Event_user_11 :: Event
testObject_Event_user_11 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "0000303b-0000-23a9-0000-25de00002f80"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "000043a6-0000-1627-0000-490300002017"))) (Domain "faraway.example.com"))
    (read "1864-04-12 01:28:25.705 UTC")
    ( EdMembersLeave
        EdReasonLeft
        ( QualifiedUserIdList
            { qualifiedUserIdList =
                [ Qualified (Id (fromJust (UUID.fromString "00003fab-0000-40b8-0000-3b0c000014ef"))) (Domain "faraway.example.com"),
                  Qualified (Id (fromJust (UUID.fromString "00001c48-0000-29ae-0000-62fc00001479"))) (Domain "faraway.example.com")
                ]
            }
        )
    )

testObject_Event_user_12 :: Event
testObject_Event_user_12 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "00000838-0000-1bc6-0000-686d00003565"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "0000114a-0000-7da8-0000-40cb00007fcf"))) (Domain "faraway.example.com"))
    (read "1864-05-12 20:29:47.483 UTC")
    ( EdMembersJoin
        ( SimpleMembers
            { mMembers =
                [ SimpleMember
                    { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000055-0000-004d-0000-005100000037"))) (Domain "faraway.example.com"),
                      smConvRoleName = fromJust (parseRoleName "dlkagbmicz0f95d")
                    },
                  SimpleMember
                    { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-001f-0000-001500000009"))) (Domain "faraway.example.com"),
                      smConvRoleName = fromJust (parseRoleName "2e")
                    }
                ]
            }
        )
    )

testObject_Event_user_13 :: Event
testObject_Event_user_13 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "00000838-0000-1bc6-0000-686d00003565"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "0000114a-0000-7da8-0000-40cb00007fcf"))) (Domain "faraway.example.com"))
    (read "1864-05-12 20:29:47.483 UTC")
    (EdConvRename (ConversationRename "New conversation name"))

testObject_Event_user_14 :: Event
testObject_Event_user_14 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "00000838-0000-1bc6-0000-686d00003565"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "0000114a-0000-7da8-0000-40cb00007fcf"))) (Domain "faraway.example.com"))
    (read "1864-05-12 20:29:47.483 UTC")
    (EdConvCodeUpdate cc)
  where
    cc =
      ConversationCodeInfo
        ( ConversationCode
            { conversationKey = Key {asciiKey = unsafeRange (fromRight undefined (validate "NEN=eLUWHXclTp=_2Nap"))},
              conversationCode = Value {asciiValue = unsafeRange (fromRight undefined (validate "lLz-9vR8ENum0kI-xWJs"))},
              conversationUri = Nothing
            }
        )
        False

testObject_Event_user_15 :: Event
testObject_Event_user_15 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "7cd50991-3cdd-40ec-bb0f-63ae17b2309d"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "04e68c50-027e-4e84-a33a-e2e28a7b8ea3"))) (Domain "faraway.example.com"))
    (read "2021-11-10 05:39:44.297 UTC")
    (EdMLSMessage "hello world")

testObject_Event_user_16 :: Event
testObject_Event_user_16 =
  Event
    (Qualified (Id (fromJust (UUID.fromString "6ec1c834-9ae6-4825-8809-61dde80be5ea"))) (Domain "faraway.example.com"))
    Nothing
    (Qualified (Id (fromJust (UUID.fromString "e8f48b8f-fad3-4f60-98e3-a6df082c328d"))) (Domain "faraway.example.com"))
    (read "2021-05-12 13:12:01.005 UTC")
    (EdMLSWelcome "welcome message content")
