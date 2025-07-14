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

module Test.Wire.API.Golden.Generated.Event_conversation where

import Data.Code
import Data.Domain (Domain (..))
import Data.Id
import Data.Misc (HttpsUrl (HttpsUrl))
import Data.Qualified (Qualified (..))
import Data.Range
import Data.Time
import Data.UUID qualified as UUID
import GHC.Exts (IsList (fromList))
import Imports
import URI.ByteString (Authority (..), Host (..), Query (..), Scheme (..), URIRef (..))
import Wire.API.Conversation (Access (..), MutedStatus (..))
import Wire.API.Conversation.Code
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.Event.Conversation
import Wire.API.Event.LeaveReason

testObject_Event_conversation_1 :: Event
testObject_Event_conversation_1 =
  Event
    { evtConv = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "oym59-06.i423w"}},
      evtSubConv = Nothing,
      evtFrom = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "n8nl6tp.h5"}},
      evtTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      evtData = EdConvCodeDelete,
      evtTeam = Nothing
    }

testObject_Event_conversation_2 :: Event
testObject_Event_conversation_2 =
  Event
    { evtConv = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "2m99----34.id7u09"}},
      evtSubConv = Nothing,
      evtFrom = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "b.0-7.0.rg"}},
      evtTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      evtData =
        EdMemberUpdate
          ( MemberUpdateData
              { misTarget = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "oy8yz.f1"}},
                misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                misOtrMutedRef = Nothing,
                misOtrArchived = Nothing,
                misOtrArchivedRef = Nothing,
                misHidden = Nothing,
                misHiddenRef = Nothing,
                misConvRoleName = Just roleNameWireAdmin
              }
          ),
      evtTeam = Nothing
    }

testObject_Event_conversation_3 :: Event
testObject_Event_conversation_3 =
  Event
    { evtConv = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "91.ii9vf.mbwj9k7lmk"}},
      evtSubConv = Nothing,
      evtFrom = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "k3.f.z"}},
      evtTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      evtData =
        EdConvCodeUpdate
          ( ConversationCodeInfo
              ( ConversationCode
                  { conversationKey = Key {asciiKey = unsafeRange "CRdONS7988O2QdyndJs1"},
                    conversationCode = Value {asciiValue = unsafeRange "7d6713"},
                    conversationUri = Just $ HttpsUrl (URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})
                  }
              )
              False
          ),
      evtTeam = Nothing
    }

testObject_Event_conversation_4 :: Event
testObject_Event_conversation_4 =
  Event
    { evtConv = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "ma--6us.i8o--0440"}},
      evtSubConv = Nothing,
      evtFrom = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "97k-u0.b-5c"}},
      evtTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      evtData = EdConvAccessUpdate (ConversationAccessData {cupAccess = fromList [PrivateAccess, CodeAccess], cupAccessRoles = fromList []}),
      evtTeam = Nothing
    }

testObject_Event_conversation_5 :: Event
testObject_Event_conversation_5 =
  Event
    { evtConv = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "q.6lm833.o95.l.y2"}},
      evtSubConv = Nothing,
      evtFrom = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "7.m4f7p.ez4zs61"}},
      evtTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      evtData = EdMLSWelcome "",
      evtTeam = Nothing
    }

testObject_Event_conversation_6 :: Event
testObject_Event_conversation_6 =
  Event
    { evtConv = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "391.r"}},
      evtSubConv = Nothing,
      evtFrom = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "8.0-6.t7pxv"}},
      evtTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      evtData = EdOtrMessage (OtrMessage {otrSender = ClientId 1, otrRecipient = ClientId 1, otrCiphertext = "", otrData = Just "I\68655"}),
      evtTeam = Nothing
    }

testObject_Event_conversation_7 :: Event
testObject_Event_conversation_7 =
  Event
    { evtConv = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "b2.ue4k"}},
      evtSubConv = Nothing,
      evtFrom = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "64b3--h.u"}},
      evtTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      evtData = EdOtrMessage (OtrMessage {otrSender = ClientId 3, otrRecipient = ClientId 3, otrCiphertext = "%\SI", otrData = Nothing}),
      evtTeam = Nothing
    }

testObject_Event_conversation_8 :: Event
testObject_Event_conversation_8 =
  Event
    { evtConv = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "36.e9.s-o-17"}},
      evtSubConv = Nothing,
      evtFrom = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "0--gy.705nsa8.j4m"}},
      evtTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      evtData = EdTyping StartedTyping,
      evtTeam = Nothing
    }

testObject_Event_conversation_9 :: Event
testObject_Event_conversation_9 =
  Event
    { evtConv = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "743846p6-pp33.1.ktb9.0bmn.efm2bly"}},
      evtSubConv = Nothing,
      evtFrom = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "9--5grmn.j39y3--9n"}},
      evtTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      evtData =
        EdMembersLeave
          EdReasonLeft
          ( QualifiedUserIdList
              { qualifiedUserIdList =
                  [ Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "ow8i3fhr.v"}},
                    Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "8kshw-2l.3w44.6c8763a-77r4.gk13zq"}},
                    Qualified
                      { qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")),
                        qDomain = Domain {_domainText = "xk-no.m5--f8b7"}
                      },
                    Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "x02.p.69y-6.8ncr.u"}},
                    Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "1-47tume1e5l32i.v75is-q4-o.u7qc"}},
                    Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "8ay.ec.k-8"}},
                    Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "u-8.m-42ns2c"}},
                    Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "08kh83-8.vu.i24"}},
                    Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "t1t683.o3--2.3k5.it-5.e1"}},
                    Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "q.ajw-5"}},
                    Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "73m4g.c24em3.v"}},
                    Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "i7zn.li"}},
                    Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "42y78.yekf"}},
                    Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "n63-p87m2.dtq"}},
                    Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "ns.v2p"}}
                  ]
              }
          ),
      evtTeam = Nothing
    }

testObject_Event_conversation_10 :: Event
testObject_Event_conversation_10 =
  Event
    { evtConv = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "1852a.o-4"}},
      evtSubConv = Nothing,
      evtFrom = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "4-p.d7b8d3.6.c8--jds3-1acy"}},
      evtTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      evtData = EdMLSMessage "s\b\138w\236\231P(\ESC\216\205",
      evtTeam = Nothing
    }

testObject_Event_conversation_11 :: Event
testObject_Event_conversation_11 =
  Event
    { evtConv = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "wwzw-ly4jk5.6790-y.j04o-21.ltl"}},
      evtSubConv = Nothing,
      evtFrom = Qualified {qUnqualified = Id (fromJust (UUID.fromString "2126ea99-ca79-43ea-ad99-a59616468e8e")), qDomain = Domain {_domainText = "70-o.ncd"}},
      evtTime = UTCTime {utctDay = ModifiedJulianDay 58119, utctDayTime = 0},
      evtData = EdTyping StoppedTyping,
      evtTeam = Nothing
    }
