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
module Test.Wire.API.Golden.Generated.RemoveBotResponse_user where

import Data.Code (Key (Key, asciiKey), Value (Value, asciiValue))
import Data.Coerce (coerce)
import Data.Domain
import Data.Id (ClientId (ClientId, client), Id (Id))
import Data.Misc (HttpsUrl (HttpsUrl), Milliseconds (Ms, ms))
import Data.Qualified
import Data.Range (unsafeRange)
import Data.Text.Ascii (AsciiChars (validate))
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust, fromRight, read, undefined)
import URI.ByteString
  ( Authority
      ( Authority,
        authorityHost,
        authorityPort,
        authorityUserInfo
      ),
    Host (Host, hostBS),
    Query (Query, queryPairs),
    Scheme (Scheme, schemeBS),
    URIRef
      ( URI,
        uriAuthority,
        uriFragment,
        uriPath,
        uriQuery,
        uriScheme
      ),
  )
import Wire.API.Conversation
  ( Access (InviteAccess, PrivateAccess),
    AccessRole (ActivatedAccessRole),
    ConversationAccessUpdate
      ( ConversationAccessUpdate,
        cupAccess,
        cupAccessRole
      ),
    ConversationMessageTimerUpdate
      ( ConversationMessageTimerUpdate,
        cupMessageTimer
      ),
    ConversationRename (ConversationRename, cupName),
  )
import Wire.API.Conversation.Bot (RemoveBotResponse (..))
import Wire.API.Conversation.Code
  ( ConversationCode
      ( ConversationCode,
        conversationCode,
        conversationKey,
        conversationUri
      ),
  )
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Conversation.Typing (TypingData (TypingData, tdStatus), TypingStatus (StartedTyping))
import Wire.API.Event.Conversation
  ( Connect (Connect, cEmail, cMessage, cName, cRecipient),
    Event (Event),
    EventData (..),
    EventType
      ( ConvAccessUpdate,
        ConvCodeDelete,
        ConvCodeUpdate,
        ConvConnect,
        ConvDelete,
        ConvMessageTimerUpdate,
        ConvRename,
        MemberJoin,
        MemberLeave,
        OtrMessageAdd,
        Typing
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

testObject_RemoveBotResponse_user_1 :: RemoveBotResponse
testObject_RemoveBotResponse_user_1 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (MemberLeave)
            (Qualified (Id (fromJust (UUID.fromString "00003ab8-0000-0cff-0000-427f000000df"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00004166-0000-1e32-0000-52cb0000428d"))) (Domain "faraway.example.com"))
            (read "1864-05-07 01:13:35.741 UTC")
            ( ( EdMembersLeave
                  ( UserIdList
                      { mUsers =
                          [ (Qualified (Id (fromJust (UUID.fromString "000038c1-0000-4a9c-0000-511300004c8b"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00003111-0000-2620-0000-1c8800000ea0"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00000de2-0000-6a83-0000-094b00007b02"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00001203-0000-7200-0000-7f8600001824"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "0000412f-0000-6e53-0000-6fde00001ffa"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "000035d8-0000-190b-0000-3f6a00004698"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00004a5d-0000-1532-0000-7c0f000057a8"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00001eda-0000-7b4f-0000-35d800001e6f"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "000079aa-0000-1359-0000-42b8000036a9"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00001b31-0000-356b-0000-379b000048ef"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "0000649d-0000-04a0-0000-6dac00001c6d"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00003a75-0000-6289-0000-274d00001220"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00003ffb-0000-1dcc-0000-3ad40000209c"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00007243-0000-40bf-0000-6cd1000079ca"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "000003ef-0000-0ac8-0000-1a060000698d"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00005a61-0000-3900-0000-4b5d00007ea6"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00001ebb-0000-22ef-0000-4df700007541"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00005dc2-0000-68ba-0000-2bd0000010a8"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00001e9c-0000-24ba-0000-0f8e000016b6"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "0000480d-0000-0b25-0000-6f8700001bcf"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00006d2e-0000-7890-0000-77e600007c77"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00005702-0000-2392-0000-643e00000389"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "000041a6-0000-52a9-0000-41ce00003ead"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "000026a1-0000-0fd3-0000-4aa2000012e7"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00000820-0000-54c4-0000-48490000065b"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "000026ea-0000-4310-0000-7c61000078ea"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00005134-0000-19cc-0000-32fe00006ccb"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00006c9f-0000-5750-0000-3d5c00000149"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "00004772-0000-793d-0000-0b4d0000087f"))) (Domain "faraway.example.com")),
                            (Qualified (Id (fromJust (UUID.fromString "000074ee-0000-5b53-0000-640000005536"))) (Domain "faraway.example.com"))
                          ]
                      }
                  )
              )
            )
        )
    }

testObject_RemoveBotResponse_user_2 :: RemoveBotResponse
testObject_RemoveBotResponse_user_2 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (ConvDelete)
            (Qualified (Id (fromJust (UUID.fromString "00005a06-0000-10ab-0000-4999000058de"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00004247-0000-0560-0000-07df00005850"))) (Domain "faraway.example.com"))
            (read "1864-04-23 16:56:18.982 UTC")
            (EdConvDelete)
        )
    }

testObject_RemoveBotResponse_user_3 :: RemoveBotResponse
testObject_RemoveBotResponse_user_3 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (MemberJoin)
            (Qualified (Id (fromJust (UUID.fromString "000031b6-0000-7f2c-0000-22ca000012a0"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00005a35-0000-3751-0000-76fe000044c2"))) (Domain "faraway.example.com"))
            (read "1864-04-23 02:07:23.62 UTC")
            ( ( EdMembersJoin
                  ( SimpleMembers
                      { mMembers =
                          [ SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000042-0000-0046-0000-005e0000001f"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "3jqe4rv30oxjs05p0vjx_gv"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000073-0000-003c-0000-005800000069"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "gv66owx6jn8"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000003a-0000-0056-0000-000e00000038"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "zx5yjj62r6x5vzvdekehjc6syfkollz3j5ztxjsu1ffrjvolkynevvykqe6dyyntx3t4p7ph_axwmb_9puw2h2i5qrnvkuwx1a7d23ln9q30h_vulfs1x8iiya"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000007c-0000-0075-0000-006500000036"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      (parseRoleName "_6hbn84l_4xly84ic0hrz_m4unx_i2_5sfotmu2xjmylyly_qilavdw54n1reep")
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000031-0000-0004-0000-005e00000060"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "u8r_c9n84lvf4v9i8c6tzre_e3jhp327b2vvubky8_25tf6x6cszt770uuuikdpofyu5oa7lyd"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000007b-0000-0019-0000-002a00000000"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  (fromJust (parseRoleName "4agujelz62r_o96qfxja1h60hqmsbuowdhmqb1zvrlhtru6b66vl1lu5oc1"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000000c-0000-0069-0000-006600000032"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "6o_85q3e0hn13mkqzstg29b3r29ezb52cl6a_1hhzpx1wtdkav8z8nhc8uk5jj3wsp16rn0wx0dbj9rqt"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000001d-0000-0006-0000-00540000005a"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "ii7eljki45zqe819xzx16tkvbgb85"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000007f-0000-005d-0000-000700000035"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "8fg3lg3rtnjamcshonl6ailheepmslbc_c3vgdhofs2hwbr84duunkatfkotiq246euejqre_sa4ygly"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000006b-0000-005e-0000-003d0000007c"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "5moz9hri8wj07ilkxfcsubwzelf8bkv0vpyssxthz7nnwbthym1ux33bn682ddcbv91aq7oquc9osjow75iu75kjp0prd2zam_o_zixgv3"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000005e-0000-000e-0000-00300000005f"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "_xq9rxj1fopahja5o9av3g18y4ko17fzdjunr84k0_txycx3sd1sqn2k5_usv0l_007wdzjrnxcss4b32w4c1qe"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000060-0000-0000-0000-001e0000003b"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "h0q7fe607q9oaiw53dbfunmrlposh47fvaoe5mfg8rth7dzl8r0y759kclqbbqzt7zlbu090lkdberm0u78tb"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000030-0000-000e-0000-006f0000001d"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "rw50gu92raxvq87hqpf7r_xyl"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000003f-0000-005e-0000-003200000062"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  (fromJust (parseRoleName "5bizt8d567yjavituolq2unxfh0qyih7_9dep7cpix5bucbevifs2m0"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000002e-0000-007d-0000-005e0000004d"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "1kit803b528tmtyvlkespy"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000007c-0000-0013-0000-007100000049"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "74l03am2b"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000012-0000-0058-0000-00500000004d"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "8ghe34e3xwi0i1e7cfe8ivltslpzuf15xadc7x5741tzeh1ne_v3m_xzjouowchqe5ubn0jptjorvxoksxwqowgp7oey9ptzpe2cegkplw3445q2z390sf1zy_09ngm"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000060-0000-007f-0000-003c0000001d"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "ui1_axn4co_y0u6a8yrmwsam6zar72jdpdorz8xyvxa1_gfd50r4gu47detfx0rgm6s9iqy2"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000034-0000-005a-0000-003600000063"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "32y4b84gygtg3xscfds0vu69bbsir8cbfh0_gmnh6hnbdr6md8807tuoi8ijtsfr2bkfd8d1vlacwytk55gr__t9f48uyd9p1fz07j20"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000006a-0000-007f-0000-006700000068"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "wf0v8gr2oqqdm"))
                              }
                          ]
                      }
                  )
              )
            )
        )
    }

testObject_RemoveBotResponse_user_4 :: RemoveBotResponse
testObject_RemoveBotResponse_user_4 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (ConvAccessUpdate)
            (Qualified (Id (fromJust (UUID.fromString "000057d8-0000-4ce9-0000-2a9a00001ced"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00005b30-0000-0805-0000-116700000485"))) (Domain "faraway.example.com"))
            (read "1864-05-21 00:12:51.49 UTC")
            ((EdConvAccessUpdate (ConversationAccessUpdate {cupAccess = [], cupAccessRole = ActivatedAccessRole})))
        )
    }

testObject_RemoveBotResponse_user_5 :: RemoveBotResponse
testObject_RemoveBotResponse_user_5 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (ConvAccessUpdate)
            (Qualified (Id (fromJust (UUID.fromString "00004615-0000-2e80-0000-552b0000353c"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "0000134e-0000-6a75-0000-470a00006537"))) (Domain "faraway.example.com"))
            (read "1864-04-14 01:56:55.057 UTC")
            ( ( EdConvAccessUpdate
                  ( ConversationAccessUpdate
                      { cupAccess = [InviteAccess, PrivateAccess, PrivateAccess],
                        cupAccessRole = ActivatedAccessRole
                      }
                  )
              )
            )
        )
    }

testObject_RemoveBotResponse_user_6 :: RemoveBotResponse
testObject_RemoveBotResponse_user_6 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (MemberJoin)
            (Qualified (Id (fromJust (UUID.fromString "00002aa8-0000-7a99-0000-660700000bd3"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "000036f7-0000-6d15-0000-0ff200006a4c"))) (Domain "faraway.example.com"))
            (read "1864-05-31 11:11:10.792 UTC")
            ( ( EdMembersJoin
                  ( SimpleMembers
                      { mMembers =
                          [ SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000041-0000-004b-0000-002300000030"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "htshpkwocsefoqvjbzonewymi1zn8fpmdi1o8bwmm7fj161iortxvrz23lrjzabdmh6a55bb8cvq09xv6rq4qdtff95hkuqw4u8tj5ez9xx9cd7pvc_r67s2vw4m"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000055-0000-0065-0000-005000000065"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "j2dtw20p_p7_v96xvpsjwe9ww3eyi4zdq8xx2_cabuv0w21u_vz5l09abprf1hue25srgwrlgeszd1ce3mtgz5w"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000000e-0000-0042-0000-00580000000a"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "o7hm7tvk1opilxu1kc5chxj25scof183t5mdwhdkj0zjg7re3vbt5g8988z6gyu4p8sspu8fto0sko9e_m8pzk54zzvwz7vod927_jjcp3wg5jj9n2egwvi8"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000006b-0000-0080-0000-00360000000c"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      (parseRoleName "kf14jpkab__n0g0ssfw21_3q52t2op841s0zl8edy11acgb218rr4nmkodozdim")
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000031-0000-0006-0000-003f00000069"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "06i5vil75hof_mqn8_7cuglrizks"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000006f-0000-0026-0000-006000000045"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "cux_igluvokgr7z7ikcqcmm9dhskcimfufmsxwb11vfv"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000002a-0000-0080-0000-003e00000014"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "es0p"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000001d-0000-006f-0000-003e0000003d"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "w28vr_ps429op3rmp3sil1wogmfgf1dsxmmsx2u5smde8srbfb11opw0a_b5z9ywbu9q0yivoz2n70m808m6f1vtvcr6oeh05c20va1jh299hk6q950"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000004c-0000-0065-0000-007d00000026"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "4pgdip19fs0"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000028-0000-005b-0000-007d00000042"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "x76ykqupchbjeozez7aqxynobvjd38xuqb"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000011-0000-0031-0000-001e00000055"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "fsttup8l4pwse5n72k34u_swxpalpgzl4gjnko0l7c3gxmu0x6l4nzbyzdcaxstr2iiuxb061f9"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000039-0000-003b-0000-002200000013"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "73c37ry1xfsx"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000007c-0000-0065-0000-001b00000046"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "51a4e2v57yge9xa_cc6mg67bix0exndp7swn_dppzuk8n5i19xsqaoqlkyv_x2hhv8h4uzkng185o5y_77189zvwk_y8sy1ynp5y8vo0e5p__kwlcl0yztuvtiyyr1"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000034-0000-004c-0000-001c0000007c"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "3qy1onol9hu2g4hql7ak8gyleg9a2dh0poq72b8opgm3140xjmrvlj0jtovjt3fpbar4x1i08lzdqndo7nhtczrrp9dahulq9fbuhfdrhu7n7kl6tkvu"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000004-0000-0028-0000-002300000045"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "lc4kukb759glnd3j1a5cd141a7a0h8pze2c78n8x3h_9mzn7v8jtfpsgqrvt9lca6l5f8oqk3yplig1ccl8"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000000f-0000-007d-0000-00650000006b"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "pfirchcrh2lo5pq1msq2x93tawq4v37onjphe9fcssiwfdpysse0dvk3ehupya4axtiq6ewmsjjj9xsaimlk0l70ovinyo5zmgil24ckv_fd2v_h4fx9i2s"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000006c-0000-0028-0000-004000000067"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "2130v11uf_bzjod2p35u_vhotitn"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000059-0000-007e-0000-00580000006c"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      (parseRoleName "6idgmk_1d_g5ii1sfpfcrenr8m2afbe2d71llw8xrlzdhxw_g7vn3foj5_abaul9j71_")
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000007f-0000-0057-0000-004c00000005"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "c9ycux2q_6sj1hecc_cvkz6aupdm4g5rc3gzyw9cnd0wqd0miltcb1i0q6tietu0w7khbhg8fx3z600fgsr2m3rj0mxs1pqwblnhazp1f23t"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000004a-0000-000b-0000-001000000004"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  (fromJust (parseRoleName "57guddz98hnzetk8xjme1h_gtmczis9jv3xt73rtjgz6jsentre2s7d2"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000000a-0000-0039-0000-005c00000048"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "x0qcthwpdmzimnfqh4rd4sf"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000006a-0000-000d-0000-006300000074"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "xdobrq683oi0lbxoy9ociqkouclsen5wu8suhbj75co521ipa89bnc7nh3y41fg58bxlet5u0wg94ueejw05iu15zr1kno_oxiqlhx9s9i9zd8ksyb4"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000003-0000-0078-0000-00310000002b"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "9ble9wkz5sx4fof474zgb"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000006-0000-0042-0000-007e00000013"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "6x00nd8of9_prpikunwo7292vzgp6qivsia735dns1s395syckletc2smrzxezrsn1hgjjvenm"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000002d-0000-0043-0000-004f00000078"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "dd2lcxld259xsjsqz2h130ksyeixe21s87mhwa7tas1k_ttqefg66ga13x7ixlfuuiaj5p8i16nn6pf3sbn25p8s4ld9virn3tf"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000015-0000-0069-0000-005400000018"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  (fromJust (parseRoleName "jiyr52auzomq5ui457z209fcszalvj_wy09_zgc05pfp9x304nwxni"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000064-0000-007d-0000-006b00000055"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "ldesfdsha0z3olxjyjkijtud5z2ns5oxb5h1vbbamtgymlnmjg4ybed_tfhvntcdr1h78ihk5ztwd27vtiy"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000006d-0000-007a-0000-007a00000017"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "hcfut6_dj"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000005-0000-0065-0000-002600000007"))) (Domain "faraway.example.com"),
                                smConvRoleName = (fromJust (parseRoleName "q5_32a257neednc3"))
                              }
                          ]
                      }
                  )
              )
            )
        )
    }

testObject_RemoveBotResponse_user_7 :: RemoveBotResponse
testObject_RemoveBotResponse_user_7 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (OtrMessageAdd)
            (Qualified (Id (fromJust (UUID.fromString "00006a93-0000-005c-0000-361e00000180"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00007bb6-0000-07cc-0000-687c00002703"))) (Domain "faraway.example.com"))
            (read "1864-04-25 18:08:10.735 UTC")
            ( ( EdOtrMessage
                  ( OtrMessage
                      { otrSender = ClientId {client = "b"},
                        otrRecipient = ClientId {client = "1c"},
                        otrCiphertext = "",
                        otrData = Nothing
                      }
                  )
              )
            )
        )
    }

testObject_RemoveBotResponse_user_8 :: RemoveBotResponse
testObject_RemoveBotResponse_user_8 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (ConvConnect)
            (Qualified (Id (fromJust (UUID.fromString "000022d4-0000-6167-0000-519f0000134c"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "0000200d-0000-386f-0000-0de000003b71"))) (Domain "faraway.example.com"))
            (read "1864-05-29 09:46:28.943 UTC")
            ( ( EdConnect
                  ( Connect
                      { cRecipient = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000006"))),
                        cMessage = Just "2\152188Q\EM4\DC3",
                        cName = Just "$\1094087\1072236",
                        cEmail = Just "1\\X"
                      }
                  )
              )
            )
        )
    }

testObject_RemoveBotResponse_user_9 :: RemoveBotResponse
testObject_RemoveBotResponse_user_9 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (OtrMessageAdd)
            (Qualified (Id (fromJust (UUID.fromString "0000324b-0000-23a4-0000-0fbb00006c87"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00006234-0000-7d47-0000-0b95000079f2"))) (Domain "faraway.example.com"))
            (read "1864-05-18 05:11:02.885 UTC")
            ( ( EdOtrMessage
                  ( OtrMessage
                      { otrSender = ClientId {client = "1c"},
                        otrRecipient = ClientId {client = "19"},
                        otrCiphertext = "\STX\1046061\SYN\1945\SYN",
                        otrData = Nothing
                      }
                  )
              )
            )
        )
    }

testObject_RemoveBotResponse_user_10 :: RemoveBotResponse
testObject_RemoveBotResponse_user_10 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (Typing)
            (Qualified (Id (fromJust (UUID.fromString "00005788-0000-327b-0000-7ef80000017e"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "0000588d-0000-6704-0000-153f00001692"))) (Domain "faraway.example.com"))
            (read "1864-04-11 02:49:27.442 UTC")
            ((EdTyping (TypingData {tdStatus = StartedTyping})))
        )
    }

testObject_RemoveBotResponse_user_11 :: RemoveBotResponse
testObject_RemoveBotResponse_user_11 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (ConvRename)
            (Qualified (Id (fromJust (UUID.fromString "00001db4-0000-575c-0000-5b9200002c33"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "000009b3-0000-04dc-0000-310100002b5f"))) (Domain "faraway.example.com"))
            (read "1864-05-25 16:08:53.052 UTC")
            ( ( EdConvRename
                  ( ConversationRename
                      { cupName = "\ETB\157284\160321>P2L\177195x\1075131\1078860\989169T\151842\&0)y\1003901\SYN"
                      }
                  )
              )
            )
        )
    }

testObject_RemoveBotResponse_user_12 :: RemoveBotResponse
testObject_RemoveBotResponse_user_12 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (ConvConnect)
            (Qualified (Id (fromJust (UUID.fromString "00004c29-0000-0214-0000-1d7300001cdc"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00003ba8-0000-448c-0000-769e00004cdf"))) (Domain "faraway.example.com"))
            (read "1864-04-23 00:31:51.842 UTC")
            ( ( EdConnect
                  ( Connect
                      { cRecipient = (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000100000007"))),
                        cMessage = Just "\EM{ze;RY",
                        cName = Nothing,
                        cEmail = Just "}Y\1075650]?\21533o"
                      }
                  )
              )
            )
        )
    }

testObject_RemoveBotResponse_user_13 :: RemoveBotResponse
testObject_RemoveBotResponse_user_13 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (ConvMessageTimerUpdate)
            (Qualified (Id (fromJust (UUID.fromString "000062a2-0000-46ad-0000-0f8100005bbe"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "000065a2-0000-1aaa-0000-311000003d69"))) (Domain "faraway.example.com"))
            (read "1864-05-06 22:47:56.147 UTC")
            ((EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Nothing})))
        )
    }

testObject_RemoveBotResponse_user_14 :: RemoveBotResponse
testObject_RemoveBotResponse_user_14 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (ConvCodeUpdate)
            (Qualified (Id (fromJust (UUID.fromString "0000060f-0000-6d7d-0000-33a800005d07"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00005c4c-0000-226a-0000-04b70000100a"))) (Domain "faraway.example.com"))
            (read "1864-04-21 02:44:02.145 UTC")
            ( ( EdConvCodeUpdate
                  ( ConversationCode
                      { conversationKey =
                          Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("AbM=P0Cv1K3WFwJLU6eg")))))},
                        conversationCode =
                          Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("z3MqXFfMRMlMeTim7025")))))},
                        conversationUri = Nothing
                      }
                  )
              )
            )
        )
    }

testObject_RemoveBotResponse_user_15 :: RemoveBotResponse
testObject_RemoveBotResponse_user_15 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (ConvMessageTimerUpdate)
            (Qualified (Id (fromJust (UUID.fromString "00006421-0000-0363-0000-192100003398"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "000005cd-0000-7897-0000-1fc700002d35"))) (Domain "faraway.example.com"))
            (read "1864-04-30 23:29:02.24 UTC")
            ( ( EdConvMessageTimerUpdate
                  (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 8977358108702637})})
              )
            )
        )
    }

testObject_RemoveBotResponse_user_16 :: RemoveBotResponse
testObject_RemoveBotResponse_user_16 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (ConvCodeUpdate)
            (Qualified (Id (fromJust (UUID.fromString "0000067f-0000-0d9b-0000-039f0000033f"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "0000030b-0000-5943-0000-6cd900006eae"))) (Domain "faraway.example.com"))
            (read "1864-04-27 19:16:49.866 UTC")
            ( ( EdConvCodeUpdate
                  ( ConversationCode
                      { conversationKey =
                          Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("E=ljiXAMvwAYiYxy3jSG")))))},
                        conversationCode =
                          Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("=X8_OGM09")))))},
                        conversationUri =
                          Just
                            ( coerce
                                URI
                                  { uriScheme = Scheme {schemeBS = "https"},
                                    uriAuthority =
                                      Just
                                        ( Authority
                                            { authorityUserInfo = Nothing,
                                              authorityHost = Host {hostBS = "example.com"},
                                              authorityPort = Nothing
                                            }
                                        ),
                                    uriPath = "",
                                    uriQuery = Query {queryPairs = []},
                                    uriFragment = Nothing
                                  }
                            )
                      }
                  )
              )
            )
        )
    }

testObject_RemoveBotResponse_user_17 :: RemoveBotResponse
testObject_RemoveBotResponse_user_17 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (ConvMessageTimerUpdate)
            (Qualified (Id (fromJust (UUID.fromString "00005994-0000-5c94-0000-519300002727"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00003ddd-0000-21a2-0000-6a54000023c3"))) (Domain "faraway.example.com"))
            (read "1864-04-24 18:38:55.053 UTC")
            ( ( EdConvMessageTimerUpdate
                  (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 3685837512701220})})
              )
            )
        )
    }

testObject_RemoveBotResponse_user_18 :: RemoveBotResponse
testObject_RemoveBotResponse_user_18 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (ConvConnect)
            (Qualified (Id (fromJust (UUID.fromString "000005bf-0000-3fdd-0000-089a0000544e"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00003c0a-0000-3d64-0000-7f74000011e9"))) (Domain "faraway.example.com"))
            (read "1864-05-05 05:34:43.386 UTC")
            ( ( EdConnect
                  ( Connect
                      { cRecipient = (Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000400000002"))),
                        cMessage = Just "\1088794\GS\a",
                        cName = Just "P",
                        cEmail = Just "y\EMT"
                      }
                  )
              )
            )
        )
    }

testObject_RemoveBotResponse_user_19 :: RemoveBotResponse
testObject_RemoveBotResponse_user_19 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (ConvCodeDelete)
            (Qualified (Id (fromJust (UUID.fromString "00000c59-0000-51c7-0000-1b6500001384"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00003046-0000-14df-0000-5a5900005ef2"))) (Domain "faraway.example.com"))
            (read "1864-04-19 14:51:39.037 UTC")
            (EdConvCodeDelete)
        )
    }

testObject_RemoveBotResponse_user_20 :: RemoveBotResponse
testObject_RemoveBotResponse_user_20 =
  RemoveBotResponse
    { rsRemoveBotEvent =
        ( Event
            (ConvMessageTimerUpdate)
            (Qualified (Id (fromJust (UUID.fromString "00004e98-0000-2ec5-0000-31870000098c"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00006cb0-0000-6547-0000-1fe500000270"))) (Domain "faraway.example.com"))
            (read "1864-05-18 03:54:11.412 UTC")
            ( ( EdConvMessageTimerUpdate
                  (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 5776200192005000})})
              )
            )
        )
    }
