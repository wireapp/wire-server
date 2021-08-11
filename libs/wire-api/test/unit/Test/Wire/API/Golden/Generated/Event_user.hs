{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
import Data.Id (ClientId (ClientId, client), Id (Id))
import Data.Misc (Milliseconds (Ms, ms))
import Data.Qualified
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust, read)
import Wire.API.Conversation
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Conversation.Typing (TypingData (TypingData, tdStatus), TypingStatus (StoppedTyping))
import Wire.API.Event.Conversation
  ( Connect (Connect, cEmail, cMessage, cName, cRecipient),
    Event (Event),
    EventData (..),
    EventType
      ( ConvAccessUpdate,
        ConvCodeDelete,
        ConvConnect,
        ConvCreate,
        ConvDelete,
        ConvMessageTimerUpdate,
        ConvReceiptModeUpdate,
        MemberJoin,
        MemberLeave,
        MemberStateUpdate,
        OtrMessageAdd,
        Typing
      ),
    MemberUpdateData
      ( MemberUpdateData,
        misConvRoleName,
        misHidden,
        misHiddenRef,
        misOtrArchived,
        misOtrArchivedRef,
        misOtrMuted,
        misOtrMutedRef,
        misOtrMutedStatus,
        misTarget
      ),
    OtrMessage
      ( OtrMessage,
        otrCiphertext,
        otrData,
        otrRecipient,
        otrSender
      ),
    SimpleMember (..),
    SimpleMembers (SimpleMembers, mMembers),
    UserIdList (UserIdList, mUsers),
  )
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))

domain :: Domain
domain = Domain "golden.example.com"

testObject_Event_user_1 :: Event
testObject_Event_user_1 =
  ( Event
      (ConvDelete)
      (Qualified (Id (fromJust (UUID.fromString "00005d81-0000-0d71-0000-1d8f00007d32"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "00003b8b-0000-3395-0000-076a00007830"))) (Domain "faraway.example.com"))
      (read "1864-05-22 09:51:07.104 UTC")
      (EdConvDelete)
  )

testObject_Event_user_2 :: Event
testObject_Event_user_2 =
  ( Event
      (ConvAccessUpdate)
      (Qualified (Id (fromJust (UUID.fromString "0000064d-0000-7a7f-0000-5749000029e1"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "00006a88-0000-2acb-0000-6aa0000061b2"))) (Domain "faraway.example.com"))
      (read "1864-06-05 23:01:18.769 UTC")
      ( ( EdConvAccessUpdate
            ( ConversationAccessUpdate
                { cupAccess = [InviteAccess, LinkAccess, PrivateAccess, InviteAccess, InviteAccess],
                  cupAccessRole = ActivatedAccessRole
                }
            )
        )
      )
  )

testObject_Event_user_3 :: Event
testObject_Event_user_3 =
  ( Event
      (OtrMessageAdd)
      (Qualified (Id (fromJust (UUID.fromString "00006f8c-0000-00d6-0000-1568000001e9"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "00004b11-0000-5504-0000-55d800002188"))) (Domain "faraway.example.com"))
      (read "1864-04-27 15:44:23.844 UTC")
      ( ( EdOtrMessage
            ( OtrMessage
                { otrSender = ClientId {client = "c"},
                  otrRecipient = ClientId {client = "f"},
                  otrCiphertext = "",
                  otrData = Just ">\33032\SI\30584"
                }
            )
        )
      )
  )

testObject_Event_user_4 :: Event
testObject_Event_user_4 =
  ( Event
      (ConvCodeDelete)
      (Qualified (Id (fromJust (UUID.fromString "00004f04-0000-3939-0000-472d0000316b"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "00007c90-0000-766a-0000-01b700002ab7"))) (Domain "faraway.example.com"))
      (read "1864-05-12 00:59:09.2 UTC")
      (EdConvCodeDelete)
  )

testObject_Event_user_5 :: Event
testObject_Event_user_5 =
  ( Event
      (MemberStateUpdate)
      (Qualified (Id (fromJust (UUID.fromString "00003c8c-0000-6394-0000-294b0000098b"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "00002a12-0000-73e1-0000-71f700002ec9"))) (Domain "faraway.example.com"))
      (read "1864-04-12 03:04:00.298 UTC")
      ( ( EdMemberUpdate
            ( MemberUpdateData
                { misTarget = Nothing,
                  misOtrMuted = Just False,
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
      )
  )

testObject_Event_user_6 :: Event
testObject_Event_user_6 =
  ( Event
      (ConvMessageTimerUpdate)
      (Qualified (Id (fromJust (UUID.fromString "00001fdb-0000-3127-0000-23ef00007183"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "0000705a-0000-0b62-0000-425c000049c8"))) (Domain "faraway.example.com"))
      (read "1864-05-09 05:44:41.382 UTC")
      ((EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 5029817038083912})})))
  )

testObject_Event_user_7 :: Event
testObject_Event_user_7 =
  ( Event
      (Typing)
      (Qualified (Id (fromJust (UUID.fromString "00006ac1-0000-543e-0000-7c8f00000be7"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "0000355a-0000-2979-0000-083000002d5e"))) (Domain "faraway.example.com"))
      (read "1864-04-18 05:01:13.761 UTC")
      ((EdTyping (TypingData {tdStatus = StoppedTyping})))
  )

testObject_Event_user_8 :: Event
testObject_Event_user_8 =
  ( Event
      (ConvCodeDelete)
      (Qualified (Id (fromJust (UUID.fromString "00000892-0000-53c7-0000-0c870000027a"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "000008e8-0000-43fa-0000-4dd1000034cc"))) (Domain "faraway.example.com"))
      (read "1864-06-08 15:19:01.916 UTC")
      (EdConvCodeDelete)
  )

testObject_Event_user_9 :: Event
testObject_Event_user_9 =
  ( Event
      (ConvAccessUpdate)
      (Qualified (Id (fromJust (UUID.fromString "00004847-0000-1eb9-0000-2973000039ca"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "000044e3-0000-1c36-0000-42fd00006e01"))) (Domain "faraway.example.com"))
      (read "1864-05-21 16:22:14.886 UTC")
      ( ( EdConvAccessUpdate
            ( ConversationAccessUpdate
                { cupAccess =
                    [PrivateAccess, PrivateAccess, PrivateAccess, LinkAccess, InviteAccess, LinkAccess, CodeAccess],
                  cupAccessRole = NonActivatedAccessRole
                }
            )
        )
      )
  )

testObject_Event_user_10 :: Event
testObject_Event_user_10 =
  ( Event
      (ConvCreate)
      (Qualified (Id (fromJust (UUID.fromString "000019e1-0000-1dc6-0000-68de0000246d"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "00000457-0000-0689-0000-77a00000021c"))) (Domain "faraway.example.com"))
      (read "1864-05-29 19:31:31.226 UTC")
      ( ( EdConversation
            ( Conversation
                { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) (Domain "golden.example.com"),
                  cnvType = RegularConv,
                  cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),
                  cnvAccess =
                    [InviteAccess, PrivateAccess, LinkAccess, InviteAccess, InviteAccess, InviteAccess, LinkAccess],
                  cnvAccessRole = NonActivatedAccessRole,
                  cnvName = Just "\a\SO\r",
                  cnvMembers =
                    ConvMembers
                      { cmSelf =
                          Member
                            { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                              memService =
                                Just
                                  ( ServiceRef
                                      { _serviceRefId =
                                          (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                                        _serviceRefProvider =
                                          (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
                                      }
                                  ),
                              memOtrMuted = False,
                              memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                              memOtrMutedRef = Just "",
                              memOtrArchived = True,
                              memOtrArchivedRef = Just "",
                              memHidden = True,
                              memHiddenRef = Just "",
                              memConvRoleName =
                                ( fromJust
                                    (parseRoleName "kf_7rcnb2oilvdmd9nelmwf52gikr4aqkhktyn5vjzg7lq1dnzym812q1innmegmx9a")
                                )
                            },
                        cmOthers =
                          [ OtherMember
                              { omQualifiedId =
                                  Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
                                omService = Nothing,
                                omConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "4190csbyn6n7ooa8w4d7y9na9_a4m5hgvvmfnowu9zib_29nepamxsxl0gvq2hrfzp7obu_mtj43j0rd38jyd9r5j7xvf2ujge7s0pnt43g9cyal_ak2alwyf8uda"
                                      )
                                  )
                              },
                            OtherMember
                              { omQualifiedId =
                                  Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) domain,
                                omService =
                                  Just
                                    ( ServiceRef
                                        { _serviceRefId =
                                            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                                          _serviceRefProvider =
                                            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
                                        }
                                    ),
                                omConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "yv7zy3tkxrvz7aj3vvdv3e57pdi8euyuiatpvj48yl8ecw2xskacp737wl269wnts4rgbn1f93zbrkxs5oltt61e099wwzgztqpat4laqk6rqafvb_9aku2w"
                                      )
                                  )
                              },
                            OtherMember
                              { omQualifiedId =
                                  Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) domain,
                                omService =
                                  Just
                                    ( ServiceRef
                                        { _serviceRefId =
                                            (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                                          _serviceRefProvider =
                                            (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
                                        }
                                    ),
                                omConvRoleName = (fromJust (parseRoleName "zsltc_f04kycbem134adefbzjuyd7"))
                              },
                            OtherMember
                              { omQualifiedId =
                                  Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))) domain,
                                omService =
                                  Just
                                    ( ServiceRef
                                        { _serviceRefId =
                                            (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                                          _serviceRefProvider =
                                            (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
                                        }
                                    ),
                                omConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "nm1gzd7dfqwcf_u3zfq991ylfmjavcs0s0gm6kjq532pjjflua6u5f_xk8dxm1t1g4s3mc2piv631phv19qvtix62s4q6_rc4xj5dh3wmoer_"
                                      )
                                  )
                              }
                          ]
                      },
                  cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))),
                  cnvMessageTimer = Just (Ms {ms = 283898987885780}),
                  cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
                }
            )
        )
      )
  )

testObject_Event_user_11 :: Event
testObject_Event_user_11 =
  ( Event
      (MemberStateUpdate)
      (Qualified (Id (fromJust (UUID.fromString "000031c2-0000-108c-0000-10a500000882"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "00005335-0000-2983-0000-46460000082f"))) (Domain "faraway.example.com"))
      (read "1864-05-03 06:49:41.178 UTC")
      ( ( EdMemberUpdate
            ( MemberUpdateData
                { misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))),
                  misOtrMuted = Nothing,
                  misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                  misOtrMutedRef = Just "v\1034354",
                  misOtrArchived = Just True,
                  misOtrArchivedRef = Just "v6",
                  misHidden = Just False,
                  misHiddenRef = Just "D",
                  misConvRoleName = Just (fromJust (parseRoleName "spkf0ayk4c4obgc_l2lj54cljtj25ph"))
                }
            )
        )
      )
  )

testObject_Event_user_12 :: Event
testObject_Event_user_12 =
  ( Event
      (ConvDelete)
      (Qualified (Id (fromJust (UUID.fromString "00007474-0000-2a7b-0000-125900006ac9"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "00000795-0000-709d-0000-11270000007a"))) (Domain "faraway.example.com"))
      (read "1864-05-23 17:16:29.326 UTC")
      (EdConvDelete)
  )

testObject_Event_user_13 :: Event
testObject_Event_user_13 =
  ( Event
      (OtrMessageAdd)
      (Qualified (Id (fromJust (UUID.fromString "00006355-0000-5f6e-0000-592c0000680c"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "000029eb-0000-06f8-0000-514100000a84"))) (Domain "faraway.example.com"))
      (read "1864-05-21 03:22:42.926 UTC")
      ( ( EdOtrMessage
            ( OtrMessage
                { otrSender = ClientId {client = "1f"},
                  otrRecipient = ClientId {client = "4"},
                  otrCiphertext = "\1016351\FS!kO5",
                  otrData = Just "sz"
                }
            )
        )
      )
  )

testObject_Event_user_14 :: Event
testObject_Event_user_14 =
  ( Event
      (ConvReceiptModeUpdate)
      (Qualified (Id (fromJust (UUID.fromString "00000b98-0000-618d-0000-19e200004651"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "00004bee-0000-45a0-0000-2c0300005726"))) (Domain "faraway.example.com"))
      (read "1864-05-01 11:57:35.123 UTC")
      ((EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -10505}})))
  )

testObject_Event_user_15 :: Event
testObject_Event_user_15 =
  ( Event
      (ConvConnect)
      (Qualified (Id (fromJust (UUID.fromString "00005e43-0000-3b56-0000-7c270000538c"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "00007f28-0000-40b1-0000-56ab0000748d"))) (Domain "faraway.example.com"))
      (read "1864-05-25 01:31:49.802 UTC")
      ( ( EdConnect
            ( Connect
                { cRecipient = (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000600000001"))),
                  cMessage = Just "L",
                  cName = Just "fq",
                  cEmail = Just "\992986"
                }
            )
        )
      )
  )

testObject_Event_user_16 :: Event
testObject_Event_user_16 =
  ( Event
      (ConvAccessUpdate)
      (Qualified (Id (fromJust (UUID.fromString "00004b59-0000-55d6-0000-5aad00007373"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "0000211e-0000-0b37-0000-563100003a5d"))) (Domain "faraway.example.com"))
      (read "1864-05-24 00:49:37.413 UTC")
      ((EdConvAccessUpdate (ConversationAccessUpdate {cupAccess = [], cupAccessRole = ActivatedAccessRole})))
  )

testObject_Event_user_17 :: Event
testObject_Event_user_17 =
  ( Event
      (Typing)
      (Qualified (Id (fromJust (UUID.fromString "00006ac8-0000-1342-0000-76880000021d"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "0000145f-0000-2ce0-0000-4ca800006c72"))) (Domain "faraway.example.com"))
      (read "1864-04-17 07:39:54.846 UTC")
      ((EdTyping (TypingData {tdStatus = StoppedTyping})))
  )

testObject_Event_user_18 :: Event
testObject_Event_user_18 =
  ( Event
      (MemberLeave)
      (Qualified (Id (fromJust (UUID.fromString "0000303b-0000-23a9-0000-25de00002f80"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "000043a6-0000-1627-0000-490300002017"))) (Domain "faraway.example.com"))
      (read "1864-04-12 01:28:25.705 UTC")
      ( ( EdMembersLeave
            ( UserIdList
                { mUsers =
                    [ (Qualified (Id (fromJust (UUID.fromString "00003fab-0000-40b8-0000-3b0c000014ef"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00001c48-0000-29ae-0000-62fc00001479"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00003254-0000-4f74-0000-6fc400003a01"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "000051f3-0000-077d-0000-1b3d00003745"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "000073a6-0000-7dec-0000-673c00005911"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "0000535c-0000-3949-0000-14aa000076cb"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "0000095f-0000-696f-0000-5ee200000ace"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00003861-0000-132e-0000-502500005207"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00007be5-0000-251a-0000-469400006f8d"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "000078f6-0000-7e08-0000-56d10000390e"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "0000517f-0000-26ef-0000-24c100002ae0"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "000001c6-0000-16c9-0000-58ea00005d5e"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "0000485b-0000-208e-0000-272200005214"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00004d24-0000-439c-0000-618c00001e77"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "000077b4-0000-74a4-0000-26570000353e"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "0000332a-0000-430c-0000-5fbc00001ca8"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "000059c9-0000-6597-0000-667a00005744"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00005777-0000-7a37-0000-6e22000052d2"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "0000430d-0000-4970-0000-0a9c00007b88"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "0000530a-0000-305f-0000-71a0000035d4"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "000005b8-0000-2691-0000-3a6000007dfb"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00003c9c-0000-0780-0000-7ad500001db8"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "0000679a-0000-59cf-0000-279100003e58"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00005aba-0000-14f5-0000-5c2e0000642f"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "000016b2-0000-56e8-0000-584600006914"))) (Domain "faraway.example.com"))
                    ]
                }
            )
        )
      )
  )

testObject_Event_user_19 :: Event
testObject_Event_user_19 =
  ( Event
      (MemberJoin)
      (Qualified (Id (fromJust (UUID.fromString "00000838-0000-1bc6-0000-686d00003565"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "0000114a-0000-7da8-0000-40cb00007fcf"))) (Domain "faraway.example.com"))
      (read "1864-05-12 20:29:47.483 UTC")
      ( ( EdMembersJoin
            ( SimpleMembers
                { mMembers =
                    [ SimpleMember
                        { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000055-0000-004d-0000-005100000037"))) (Domain "faraway.example.com"),
                          smConvRoleName = (fromJust (parseRoleName "dlkagbmicz0f95d"))
                        },
                      SimpleMember
                        { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000004c-0000-0051-0000-00220000005c"))) (Domain "faraway.example.com"),
                          smConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "1me2in15nttjib_zx_qqx_c_mw4rw9bys2w4y78e6qhziu_85wj8vbnk6igkzld9unfvnl0oosp25i4btj6yehlq7q9em_mxsxodvq7nj_f5hqx"
                                )
                            )
                        },
                      SimpleMember
                        { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000014-0000-0027-0000-003400000023"))) (Domain "faraway.example.com"),
                          smConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "31664ffg5sx2690yu2059f7hij_m5vmb80kig21u4h3fe8uwfbshhgkdydiv_nwjm3mo4fprgxkizazcvax0vvxwcvdax"
                                )
                            )
                        },
                      SimpleMember
                        { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-001f-0000-001500000009"))) (Domain "faraway.example.com"),
                          smConvRoleName = (fromJust (parseRoleName "2e"))
                        },
                      SimpleMember
                        { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000005d-0000-0064-0000-00590000007d"))) (Domain "faraway.example.com"),
                          smConvRoleName = (fromJust (parseRoleName "f3nxp18px4kup3nrarx5wsp1o_eh69"))
                        },
                      SimpleMember
                        { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000068-0000-007a-0000-005a0000006c"))) (Domain "faraway.example.com"),
                          smConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "fixso00nq4580z4ax9zs0sk3rej11c09rcj2ikbvnrg_io84n0eamqvwlz2icdo2u5jzzovta5j64kp0vg7e_21vs4r0hzv9"
                                )
                            )
                        },
                      SimpleMember
                        { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000074-0000-0036-0000-00780000007d"))) (Domain "faraway.example.com"),
                          smConvRoleName =
                            (fromJust (parseRoleName "f9i5d2wd01ijp53en5bq8lch__jlnu8_v2xsgkctpin98byh1009f_v63"))
                        },
                      SimpleMember
                        { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000001c-0000-000a-0000-004800000063"))) (Domain "faraway.example.com"),
                          smConvRoleName = (fromJust (parseRoleName "o_oqigzovv9oc2uxckvk5eofmc"))
                        },
                      SimpleMember
                        { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000056-0000-0028-0000-004f00000079"))) (Domain "faraway.example.com"),
                          smConvRoleName =
                            ( fromJust
                                (parseRoleName "5snj8s5t7nicihwspcp4sg4ny1pa1yb2s6601vjyxhksbciotoi_rvivybk1iviuz8buw")
                            )
                        },
                      SimpleMember
                        { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000001e-0000-0054-0000-002300000053"))) (Domain "faraway.example.com"),
                          smConvRoleName = (fromJust (parseRoleName "73e9u2hpffjb5ids29tbtcceg0i9v2"))
                        },
                      SimpleMember
                        { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000004d-0000-0027-0000-007500000042"))) (Domain "faraway.example.com"),
                          smConvRoleName =
                            (fromJust (parseRoleName "d2s4mc_qt1cc2rox8c9gak_qivlha7q259lsz7y5bz6dxsv8igx9r"))
                        },
                      SimpleMember
                        { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000050-0000-006e-0000-007000000057"))) (Domain "faraway.example.com"),
                          smConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "7d84htzo4bc9250rer4r8p47ykbesgatuz8wwkoe1m2xnfljpwoi01025ti548frbvdmtykqq4pn1qsoc3s"
                                )
                            )
                        },
                      SimpleMember
                        { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000007a-0000-003c-0000-005300000013"))) (Domain "faraway.example.com"),
                          smConvRoleName = (fromJust (parseRoleName "v7ldb8mov4an62t6"))
                        },
                      SimpleMember
                        { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0064-0000-005e00000072"))) (Domain "faraway.example.com"),
                          smConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "k7uigpk1wwfc0mffoafjqf3dejctneh21zilaup19435zntvwu8kqd3l0k7s938ex2hf_n7_7dld5z604_if5z88f3u2w28qarfdcw5rkczk4jb4n"
                                )
                            )
                        },
                      SimpleMember
                        { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000020-0000-0012-0000-000500000036"))) (Domain "faraway.example.com"),
                          smConvRoleName = (fromJust (parseRoleName "s6creybsl300lqkhu0wv_ikgattm3bd1r"))
                        }
                    ]
                }
            )
        )
      )
  )

testObject_Event_user_20 :: Event
testObject_Event_user_20 =
  ( Event
      (MemberLeave)
      (Qualified (Id (fromJust (UUID.fromString "00000c88-0000-433f-0000-669100006374"))) (Domain "faraway.example.com"))
      (Qualified (Id (fromJust (UUID.fromString "00007547-0000-26d8-0000-52280000157c"))) (Domain "faraway.example.com"))
      (read "1864-04-21 23:40:54.462 UTC")
      ( ( EdMembersLeave
            ( UserIdList
                { mUsers =
                    [ (Qualified (Id (fromJust (UUID.fromString "00002e78-0000-23d9-0000-1cba00005025"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00003293-0000-6991-0000-533700000e73"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "000075b1-0000-2e89-0000-6262000067a9"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00007f94-0000-39fc-0000-28c5000028ed"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "000041f3-0000-3886-0000-735900007499"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00004014-0000-675c-0000-688600003ed7"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00002e75-0000-74cd-0000-529a000008c7"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00000cea-0000-4b67-0000-4a2600007dae"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00006b72-0000-1fae-0000-6647000025d0"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00003c64-0000-4b1f-0000-7bc900001c31"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00002cd3-0000-4520-0000-0d8c00004a16"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00003e8f-0000-66a2-0000-067600002d8f"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00004544-0000-0ce2-0000-1c2300007fbc"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "000071ef-0000-44f4-0000-7dc500002e5f"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00007e40-0000-7f3a-0000-45a300002aee"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00006eec-0000-4bb0-0000-271000001e9f"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00001893-0000-272e-0000-5ccc0000561f"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00004d81-0000-2d5f-0000-43ec00005771"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00002521-0000-1a18-0000-3bc200005ce2"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "000005f2-0000-3b01-0000-070000005296"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "0000411b-0000-224b-0000-32650000061a"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00004880-0000-3a0b-0000-56b10000398a"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00002d6b-0000-4f28-0000-11110000309a"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "0000357d-0000-2963-0000-7bb000002734"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00000f40-0000-657c-0000-7d25000019df"))) (Domain "faraway.example.com")),
                      (Qualified (Id (fromJust (UUID.fromString "00006350-0000-630b-0000-5f560000503e"))) (Domain "faraway.example.com"))
                    ]
                }
            )
        )
      )
  )
