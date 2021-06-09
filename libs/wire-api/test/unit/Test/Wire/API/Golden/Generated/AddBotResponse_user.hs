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
module Test.Wire.API.Golden.Generated.AddBotResponse_user where

import Data.Code (Key (Key, asciiKey), Value (Value, asciiValue))
import Data.Coerce (coerce)
import Data.Domain
import Data.Id (BotId (BotId), ClientId (ClientId, client), Id (Id))
import Data.Misc (HttpsUrl (HttpsUrl), Milliseconds (Ms, ms))
import Data.Qualified
import Data.Range (unsafeRange)
import Data.Text.Ascii (AsciiChars (validate))
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust, fromRight, read, undefined, (.))
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
  ( Access (CodeAccess, LinkAccess, PrivateAccess),
    AccessRole (ActivatedAccessRole, NonActivatedAccessRole),
    ConvMembers (ConvMembers, cmOthers, cmSelf),
    ConvType (RegularConv),
    Conversation
      ( Conversation,
        cnvAccess,
        cnvAccessRole,
        cnvCreator,
        cnvId,
        cnvMembers,
        cnvMessageTimer,
        cnvName,
        cnvReceiptMode,
        cnvTeam,
        cnvType
      ),
    ConversationAccessUpdate
      ( ConversationAccessUpdate,
        cupAccess,
        cupAccessRole
      ),
    ConversationMessageTimerUpdate
      ( ConversationMessageTimerUpdate,
        cupMessageTimer
      ),
    ConversationReceiptModeUpdate
      ( ConversationReceiptModeUpdate,
        cruReceiptMode
      ),
    ConversationRename (ConversationRename, cupName),
    Member
      ( Member,
        memConvRoleName,
        memHidden,
        memHiddenRef,
        memId,
        memOtrArchived,
        memOtrArchivedRef,
        memOtrMuted,
        memOtrMutedRef,
        memOtrMutedStatus,
        memService
      ),
    MutedStatus (MutedStatus, fromMutedStatus),
    ReceiptMode (ReceiptMode, unReceiptMode),
  )
import Wire.API.Conversation.Bot (AddBotResponse (..))
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
        ConvCreate,
        ConvDelete,
        ConvMessageTimerUpdate,
        ConvReceiptModeUpdate,
        ConvRename,
        MemberJoin,
        MemberLeave,
        MemberStateUpdate,
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
    SimpleMember (..),
    SimpleMembers (SimpleMembers, mMembers),
    UserIdList (UserIdList, mUsers),
  )
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))
import Wire.API.User
  ( Asset (ImageAsset),
    AssetSize (AssetComplete, AssetPreview),
    ColourId (ColourId, fromColourId),
    Name (Name, fromName),
  )

testObject_AddBotResponse_user_1 :: AddBotResponse
testObject_AddBotResponse_user_1 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0004-0000-000300000001"))),
      rsAddBotClient = ClientId {client = "e"},
      rsAddBotName =
        Name
          { fromName =
              "\77844\129468A\1061088\30365\142096\40918\USc\DC3~0g\ENQr\v\29872\f\154305\1077132u\175940.\1018427v\v-/\bi\bJ\ETXE3\ESC8\53613\1073036\&0@\14466\51733;\27113\SYN\153289\b&\ae]\1042471H\1024555k7\EMJ\1083646[;\140668;J^`0,B\STX\95353N.@Z\v\ENQ\r\19858|'w-\b\157432V\STX \GSW|N\1072850\&3=\22550K245\DC1\142803\168718\7168\147365\ETX"
          },
      rsAddBotColour = ColourId {fromColourId = -3},
      rsAddBotAssets = [(ImageAsset "7" (Nothing)), (ImageAsset "" (Just AssetPreview))],
      rsAddBotEvent =
        ( Event
            (ConvRename)
            (Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000003"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000400000004"))) (Domain "faraway.example.com"))
            (read "1864-05-12 19:20:22.286 UTC")
            ((EdConvRename (ConversationRename {cupName = "6"})))
        )
    }

testObject_AddBotResponse_user_2 :: AddBotResponse
testObject_AddBotResponse_user_2 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0003-0000-000200000004"))),
      rsAddBotClient = ClientId {client = "e"},
      rsAddBotName =
        Name
          { fromName =
              "\162949t\DEL\\\DC2\52420Jn\1069034\997789t!\ESC\STX\1009296~jP]}|8\1106819\11112\SYNR\985193\&8H\1056222\ETBL\189886V\99433Q\1013937\133319\EOTM\DC4kc\a V"
          },
      rsAddBotColour = ColourId {fromColourId = 3},
      rsAddBotAssets = [],
      rsAddBotEvent =
        ( Event
            (Typing)
            (Qualified (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000300000001"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000300000001"))) (Domain "faraway.example.com"))
            (read "1864-05-08 19:02:58.6 UTC")
            ((EdTyping (TypingData {tdStatus = StartedTyping})))
        )
    }

testObject_AddBotResponse_user_3 :: AddBotResponse
testObject_AddBotResponse_user_3 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0001-0000-000400000002"))),
      rsAddBotClient = ClientId {client = "d"},
      rsAddBotName =
        Name
          { fromName =
              "%}\SI\188248U?;4a\986786\166069u\ETBy@\b?\".\SOH\"[\144254\154061\&1o)q\SUB<S\DC1\1017804\ETX\DC4\144917VV/D9ec\SOH*\169139oTf&(Q"
          },
      rsAddBotColour = ColourId {fromColourId = 0},
      rsAddBotAssets =
        [ (ImageAsset "" (Just AssetPreview)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Nothing))
        ],
      rsAddBotEvent =
        ( Event
            (ConvCreate)
            (Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000003"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000004"))) (Domain "faraway.example.com"))
            (read "1864-05-10 11:22:13.523 UTC")
            ( ( EdConversation
                  ( Conversation
                      { cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                        cnvType = RegularConv,
                        cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                        cnvAccess = [],
                        cnvAccessRole = ActivatedAccessRole,
                        cnvName = Nothing,
                        cnvMembers =
                          ConvMembers
                            { cmSelf =
                                Member
                                  { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                                    memService = Nothing,
                                    memOtrMuted = True,
                                    memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                                    memOtrMutedRef = Just "",
                                    memOtrArchived = True,
                                    memOtrArchivedRef = Nothing,
                                    memHidden = False,
                                    memHiddenRef = Just "",
                                    memConvRoleName = (fromJust (parseRoleName "fcwhpxsjc"))
                                  },
                              cmOthers = []
                            },
                        cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                        cnvMessageTimer = Just (Ms {ms = 2172415561652216}),
                        cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
                      }
                  )
              )
            )
        )
    }

testObject_AddBotResponse_user_4 :: AddBotResponse
testObject_AddBotResponse_user_4 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000003"))),
      rsAddBotClient = ClientId {client = "9"},
      rsAddBotName =
        Name
          { fromName =
              "M\185247\1113359K2(\DC2\EOT7>\143187\170327\r5w\1041114\RScH\1071165\142028D\ESC\EOT\1090183\1099007PE!\1037076c\153563~"
          },
      rsAddBotColour = ColourId {fromColourId = -4},
      rsAddBotAssets = [],
      rsAddBotEvent =
        ( Event
            (ConvDelete)
            (Qualified (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000300000003"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000300000001"))) (Domain "faraway.example.com"))
            (read "1864-05-06 03:03:10.788 UTC")
            (EdConvDelete)
        )
    }

testObject_AddBotResponse_user_5 :: AddBotResponse
testObject_AddBotResponse_user_5 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0003-0000-000100000000"))),
      rsAddBotClient = ClientId {client = "e"},
      rsAddBotName =
        Name
          { fromName =
              "\1068088\1098987x\1059059\111046\1017502\\MpIc\1083784\1072885y\1088651w\181697\1025970\917804\DLEES5\ENQ\1109557\ACKP\tAx\SYN*k\1034159\1012266M\179955f\ETBMF\135162q\SUB\186144Zf\78416\32790\CAN\37672DB\1085248\171865\DC3[\46265!\1078020V(\FSp\1068585F4Xwp#\1085281TJ2\DC3\12434\1072939\1024286U&\rG\1111811h$R\rsL\998143\nz\1008788\DEL\ETXB"
          },
      rsAddBotColour = ColourId {fromColourId = -4},
      rsAddBotAssets =
        [ (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Just AssetPreview))
        ],
      rsAddBotEvent =
        ( Event
            (ConvCreate)
            (Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000002"))) (Domain "faraway.example.com"))
            (read "1864-05-13 21:19:26.488 UTC")
            ( ( EdConversation
                  ( Conversation
                      { cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                        cnvType = RegularConv,
                        cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                        cnvAccess = [],
                        cnvAccessRole = NonActivatedAccessRole,
                        cnvName = Just "",
                        cnvMembers =
                          ConvMembers
                            { cmSelf =
                                Member
                                  { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                                    memService =
                                      Just
                                        ( ServiceRef
                                            { _serviceRefId =
                                                (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                                              _serviceRefProvider =
                                                (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
                                            }
                                        ),
                                    memOtrMuted = True,
                                    memOtrMutedStatus = Nothing,
                                    memOtrMutedRef = Nothing,
                                    memOtrArchived = True,
                                    memOtrArchivedRef = Nothing,
                                    memHidden = False,
                                    memHiddenRef = Just "",
                                    memConvRoleName =
                                      ( fromJust
                                          ( parseRoleName
                                              "hnsnsqy4arvtd_u4_4_ewxmjtsbbrjsrhg1h2hy2uzwj8552_ql7ds_vo67fqw3wxue4_8ixydv3ao5w91_6_"
                                          )
                                      )
                                  },
                              cmOthers = []
                            },
                        cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                        cnvMessageTimer = Just (Ms {ms = 7052633912967928}),
                        cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
                      }
                  )
              )
            )
        )
    }

testObject_AddBotResponse_user_6 :: AddBotResponse
testObject_AddBotResponse_user_6 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000004"))),
      rsAddBotClient = ClientId {client = "d"},
      rsAddBotName =
        Name
          { fromName =
              "\999775.\NUL\FS{\ENQ\1072079f\40418.u\SUB\DC4-0BK\SI\ETB1k\150963\1074939\27325l13As\35452NY\1068437\SOD\138418A6qS\ETB6\186510\ETB\140762P\SOH=qz:\194647|H\SO+B\1101376\1088189\1107156\1006880\1031768\1005888\a\162351QA\142205&\1058662)\US7\DC3>'Sl.=\1080370\ACK\1021321\fO\993441oI\1001729\tb\1089639Zg\RS\179868\1061825YX\1052945.\1064688Q\181260\1000944-\190613"
          },
      rsAddBotColour = ColourId {fromColourId = -1},
      rsAddBotAssets = [(ImageAsset "i" (Just AssetComplete)), (ImageAsset "" (Just AssetPreview))],
      rsAddBotEvent =
        ( Event
            (ConvCodeUpdate)
            (Qualified (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000003"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000300000001"))) (Domain "faraway.example.com"))
            (read "1864-05-14 23:40:44.551 UTC")
            ( ( EdConvCodeUpdate
                  ( ConversationCode
                      { conversationKey =
                          Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("ljLPNBdwXBNph6csPBYc")))))},
                        conversationCode =
                          Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("3A16c3OyeYqqLqhHKwZ")))))},
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

testObject_AddBotResponse_user_7 :: AddBotResponse
testObject_AddBotResponse_user_7 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0003-0000-000300000000"))),
      rsAddBotClient = ClientId {client = "c"},
      rsAddBotName =
        Name
          { fromName =
              "q\NUL+\1023082{-\1050727h\994893\&3\1090806\151704&@jZqjW\27599\DLE\SI\997397\38806\rZ\1089468\n\CAN\1104095\DC1l\1030879\153742\1062975<`N nX?\987303+h.\SOH\1030244OhO9qy\1105414@\174579\14702\1101980\ftrG\1074806?]\1064538\RS\NULu.AA =9H5H2YJ\136205a"
          },
      rsAddBotColour = ColourId {fromColourId = -4},
      rsAddBotAssets = [],
      rsAddBotEvent =
        ( Event
            (ConvReceiptModeUpdate)
            (Qualified (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000300000004"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000400000001"))) (Domain "faraway.example.com"))
            (read "1864-05-07 22:30:05.775 UTC")
            ((EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -4}})))
        )
    }

testObject_AddBotResponse_user_8 :: AddBotResponse
testObject_AddBotResponse_user_8 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0002-0000-000400000003"))),
      rsAddBotClient = ClientId {client = "f"},
      rsAddBotName =
        Name
          { fromName =
              "as\96525F\t\ETX\SIrPx7\am\SI;\984876\"\142529\&5\SI \DC2=\SO\1096241\179031\92226C\ESC\a4I\STX\166191Kw\SOH*\153298\158630\167768\GS\1019412\&5X\1028292`"
          },
      rsAddBotColour = ColourId {fromColourId = 0},
      rsAddBotAssets = [],
      rsAddBotEvent =
        ( Event
            (ConvAccessUpdate)
            (Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000003"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000400000000"))) (Domain "faraway.example.com"))
            (read "1864-05-05 09:04:05.078 UTC")
            ( ( EdConvAccessUpdate
                  (ConversationAccessUpdate {cupAccess = [LinkAccess, PrivateAccess], cupAccessRole = ActivatedAccessRole})
              )
            )
        )
    }

testObject_AddBotResponse_user_9 :: AddBotResponse
testObject_AddBotResponse_user_9 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000000"))),
      rsAddBotClient = ClientId {client = "8"},
      rsAddBotName =
        Name
          { fromName =
              "1\NAK5\2678\"9Vz\1016163^\188742^\1047071Q.lx\100386\1020807O\a+d=-}j3\1077280\32613w[\137747\178437p\1105250~5\ETX\1010116)\40699T$N\154953v\v{?\NAK\1061899\100656\33764mm\1093194Z\53192\1011057:\987661\&78S\DC4'\SYN\STXg\r}\a\64467q%"
          },
      rsAddBotColour = ColourId {fromColourId = 2},
      rsAddBotAssets =
        [(ImageAsset "" (Just AssetPreview)), (ImageAsset "" (Just AssetPreview)), (ImageAsset "" (Just AssetComplete))],
      rsAddBotEvent =
        ( Event
            (MemberStateUpdate)
            (Qualified (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000000000004"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))) (Domain "faraway.example.com"))
            (read "1864-05-07 17:13:06.966 UTC")
            ( ( EdMemberUpdate
                  ( MemberUpdateData
                      { misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                        misOtrMuted = Just False,
                        misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                        misOtrMutedRef = Just "",
                        misOtrArchived = Just False,
                        misOtrArchivedRef = Just "",
                        misHidden = Just True,
                        misHiddenRef = Just "",
                        misConvRoleName = Just (fromJust (parseRoleName "eivv0nnmraefi0496_5lyptwgu4jl"))
                      }
                  )
              )
            )
        )
    }

testObject_AddBotResponse_user_10 :: AddBotResponse
testObject_AddBotResponse_user_10 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000000"))),
      rsAddBotClient = ClientId {client = "10"},
      rsAddBotName =
        Name
          { fromName =
              "e1\DLE\153961\&3\163202\&2\1103669h!1Tr\96993\f;\DC4\ETBv\v\1029060\bi\155875<j\983215\62340\DC3\1015139\SUB{\1022362\SOH\173354\143782h\32052@8|:#\DLE0\128761P.zl\b@\149099(Z\v\7261\30294\156404\SOI/LLn|\24048\&6X\bfjL\DC3\NUL\97693\1043236G\CAN\94340\DLE\SUB\1031646\1027553\DC4.\1020310\RSUZ\1089060\1073793Nh\ETB\1032954y)\1073938 !\ax\1070491\DC3x\1021222cF#\EM\ETB\f\31882"
          },
      rsAddBotColour = ColourId {fromColourId = 3},
      rsAddBotAssets = [(ImageAsset "^\10571" (Just AssetPreview))],
      rsAddBotEvent =
        ( Event
            (MemberLeave)
            (Qualified (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000400000001"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))) (Domain "faraway.example.com"))
            (read "1864-05-04 10:22:33.842 UTC")
            ((EdMembersLeave (UserIdList {mUsers = []})))
        )
    }

testObject_AddBotResponse_user_11 :: AddBotResponse
testObject_AddBotResponse_user_11 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0000-0000-000200000001"))),
      rsAddBotClient = ClientId {client = "c"},
      rsAddBotName =
        Name
          { fromName =
              "+H$M\DC4'K03\\w\97649\1031181\1084666\48358(e\1046706\n\1080595\SO\NAK-9\63441fu\62634~\DC1\FS\160021\1112396\th^\EOTv\50306#9b\GS\"u\1070054\DLE\1032511y\119655Qn8\169751\1039556\&5Q\1020388\168750\24449\\\1036423\&3l\119314`\DC1@+\RSd!\1035629\DELaxx\155717\1018607\5272\GSo\1066879f\GS\ETXjrC\DC2Pk"
          },
      rsAddBotColour = ColourId {fromColourId = 0},
      rsAddBotAssets = [],
      rsAddBotEvent =
        ( Event
            (ConvAccessUpdate)
            (Qualified (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000300000004"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000100000000"))) (Domain "faraway.example.com"))
            (read "1864-05-04 14:10:34.032 UTC")
            ( ( EdConvAccessUpdate
                  (ConversationAccessUpdate {cupAccess = [CodeAccess], cupAccessRole = ActivatedAccessRole})
              )
            )
        )
    }

testObject_AddBotResponse_user_12 :: AddBotResponse
testObject_AddBotResponse_user_12 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0004-0000-000200000003"))),
      rsAddBotClient = ClientId {client = "5"},
      rsAddBotName = Name {fromName = "\32648Q2\r.+\144332\NUL'Wm)U|X.\183698\EMm\STXu\NAK\168000\&4G3>k"},
      rsAddBotColour = ColourId {fromColourId = -4},
      rsAddBotAssets = [(ImageAsset "\DC1" (Just AssetComplete))],
      rsAddBotEvent =
        ( Event
            (ConvConnect)
            (Qualified (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000200000000"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))) (Domain "faraway.example.com"))
            (read "1864-05-05 01:06:47.245 UTC")
            ( ( EdConnect
                  ( Connect
                      { cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                        cMessage = Just "",
                        cName = Nothing,
                        cEmail = Just "\ETX"
                      }
                  )
              )
            )
        )
    }

testObject_AddBotResponse_user_13 :: AddBotResponse
testObject_AddBotResponse_user_13 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0002-0000-000100000003"))),
      rsAddBotClient = ClientId {client = "9"},
      rsAddBotName =
        Name
          { fromName =
              "k\54132\14593u<svb\GS\FS\1110603aA>\1087127.\ESCOR&@i>\5779\&2\16904F=\DC4N\1085511!8\DC2\27584\EM\GS\SOH\1063251\166794JuD\ACK\SIg]\SYNZ\FS\DC3\1032653\&4&\185903F7f\1106747n\n~X\NUL^A\DLE\\\1044227=\CAN.<\EMzR\n\GSK\1085670\GS\185700\999363\f\STX!\94324^\bh\1092202?\4754\141305\GSF\1012463\1085638{\ESC>$\26360\SOHR\1110563-#U\131789\1068409\&9"
          },
      rsAddBotColour = ColourId {fromColourId = -1},
      rsAddBotAssets =
        [ (ImageAsset "" (Just AssetPreview)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Just AssetPreview)),
          (ImageAsset "" (Just AssetPreview))
        ],
      rsAddBotEvent =
        ( Event
            (ConvReceiptModeUpdate)
            (Qualified (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000100000001"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000400000002"))) (Domain "faraway.example.com"))
            (read "1864-05-13 05:09:37.371 UTC")
            ((EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 3}})))
        )
    }

testObject_AddBotResponse_user_14 :: AddBotResponse
testObject_AddBotResponse_user_14 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000004"))),
      rsAddBotClient = ClientId {client = "8"},
      rsAddBotName =
        Name
          { fromName =
              "\DLEf\35790\&6b\1667\1059776i\59844\ETX\"\r\1071318 \1051731\r\1090000R\29687v\140936fh^'\1105777\RS\ENQ\1088038[e\nG\1002512\1079413\1038391{\ACK^\1063969Gt\150750DUFa\1007520\fn6xm7T\ETB`{ 2\rN0F\1042124y\996716!+\DLE\STX\EM!f~\tu\SYN\1090525i\1075169\DC2k\129582\NULP\63700\n1k\53262\&0HS s}\DC1pG\RS\DC2<\1106323n,\ESC_\15853ta\178665"
          },
      rsAddBotColour = ColourId {fromColourId = 4},
      rsAddBotAssets = [],
      rsAddBotEvent =
        ( Event
            (MemberStateUpdate)
            (Qualified (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000004"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))) (Domain "faraway.example.com"))
            (read "1864-05-13 06:48:06.601 UTC")
            ( ( EdMemberUpdate
                  ( MemberUpdateData
                      { misTarget = Nothing,
                        misOtrMuted = Just True,
                        misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                        misOtrMutedRef = Nothing,
                        misOtrArchived = Just True,
                        misOtrArchivedRef = Just "",
                        misHidden = Just True,
                        misHiddenRef = Just "",
                        misConvRoleName = Just (fromJust (parseRoleName "xlbj5ajmu4ece6fb70ff1wioos7qm8rgg5aenk8eer"))
                      }
                  )
              )
            )
        )
    }

testObject_AddBotResponse_user_15 :: AddBotResponse
testObject_AddBotResponse_user_15 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0004-0000-000100000003"))),
      rsAddBotClient = ClientId {client = "d"},
      rsAddBotName =
        Name
          { fromName =
              "Y\1061604(\DLE<|O\179000ZM0$t~\DC4\986728\1003601Z*A/iOYx\1036037MhVOS\189640\&1>%!\CANnE4\170465@o/\98343\1043770\128919a\1013091.7\1086427\32740\1048482< '!\1056452Z \NULY\fUp\132104\&0\1001544r\157627\1021353<?E0@\9634U-\12295\NAK\EM\127773C#-q>I\1063205"
          },
      rsAddBotColour = ColourId {fromColourId = -4},
      rsAddBotAssets =
        [ (ImageAsset "" (Nothing)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Just AssetPreview))
        ],
      rsAddBotEvent =
        ( Event
            (MemberJoin)
            (Qualified (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000003"))) (Domain "faraway.example.com"))
            (read "1864-05-11 04:21:51.377 UTC")
            ( ( EdMembersJoin
                  ( SimpleMembers
                      { mMembers =
                          [ SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "uk7gwif0crc3wgiak6qae948ny57lwbwbtgbhran16vnewvp10eqialhaq9m38bqbczm_17nl46lhxs3h2cf448_7zcazh1f4ao8gnrzutbhd29j_lvsz"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "il2dfqczvqqvs3vcob7t6t7zi61y4hxgxmmpp19ueznkasq5q1cssn72l5df92b64yuqsizc6up2p1270hu18t97oifzl"
                                      )
                                  )
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  (fromJust (parseRoleName "jf7f75hkum6_zxqiabxu8zix2_1kutsjijedcjckapwmymcxx11"))
                              },
                            SimpleMember
                              { smQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))) (Domain "faraway.example.com"),
                                smConvRoleName =
                                  ( fromJust
                                      ( parseRoleName
                                          "i700417q9qqygs5k5a0zvvnpkvg2jimgi_stuyzfxgokyvy05n3_vgikqr0t5ldsb5fvltb8pylb"
                                      )
                                  )
                              }
                          ]
                      }
                  )
              )
            )
        )
    }

testObject_AddBotResponse_user_16 :: AddBotResponse
testObject_AddBotResponse_user_16 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0002-0000-000300000000"))),
      rsAddBotClient = ClientId {client = "10"},
      rsAddBotName =
        Name
          { fromName =
              "x[rw<J&rN?\3913\33850:\134766\1089867P\183077\FSu\"v\1028695\1085626yh\93054\&1\1089002pt>\71735\SUB\1043617\989645\&9InR <!\US\ENQ\SOnlp\SOH\996730\1069502\4190''\999323wN\a\GS\1034051\\\184462\51800\&0\GS"
          },
      rsAddBotColour = ColourId {fromColourId = 1},
      rsAddBotAssets = [(ImageAsset "" (Just AssetComplete))],
      rsAddBotEvent =
        ( Event
            (ConvRename)
            (Qualified (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000200000000"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))) (Domain "faraway.example.com"))
            (read "1864-05-07 11:54:38.133 UTC")
            ((EdConvRename (ConversationRename {cupName = "\72291)@\16969"})))
        )
    }

testObject_AddBotResponse_user_17 :: AddBotResponse
testObject_AddBotResponse_user_17 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))),
      rsAddBotClient = ClientId {client = "5"},
      rsAddBotName = Name {fromName = "5\1088094\1082372\v\ACKh\49710TMp\141571Q\DLE\r7\SYN\FS\1073186\1011197."},
      rsAddBotColour = ColourId {fromColourId = -2},
      rsAddBotAssets = [],
      rsAddBotEvent =
        ( Event
            (ConvAccessUpdate)
            (Qualified (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000100000002"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000400000001"))) (Domain "faraway.example.com"))
            (read "1864-05-09 16:18:32.395 UTC")
            ( ( EdConvAccessUpdate
                  ( ConversationAccessUpdate
                      { cupAccess = [PrivateAccess, LinkAccess, CodeAccess, LinkAccess],
                        cupAccessRole = NonActivatedAccessRole
                      }
                  )
              )
            )
        )
    }

testObject_AddBotResponse_user_18 :: AddBotResponse
testObject_AddBotResponse_user_18 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0003-0000-000300000003"))),
      rsAddBotClient = ClientId {client = "5"},
      rsAddBotName =
        Name
          { fromName =
              "w\ACK\1102507l\SOHyL1\ACK/\NULJ&\rdp\1056119\6750D[\58993\&2\"\\,{\ESC\1088106\SO\1023674\v0\36489\NULL\1056420u\NAKY\ETX\15250\SI46\151359#\GSl\100201\DC3'v\317\95322\aP`"
          },
      rsAddBotColour = ColourId {fromColourId = 1},
      rsAddBotAssets =
        [ (ImageAsset "" (Nothing)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Nothing)),
          (ImageAsset "" (Just AssetPreview)),
          (ImageAsset "" (Just AssetComplete))
        ],
      rsAddBotEvent =
        ( Event
            (ConvRename)
            (Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000003"))) (Domain "faraway.example.com"))
            (read "1864-05-13 04:14:10.186 UTC")
            ((EdConvRename (ConversationRename {cupName = "+S\994417x1"})))
        )
    }

testObject_AddBotResponse_user_19 :: AddBotResponse
testObject_AddBotResponse_user_19 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0001-0000-000400000002"))),
      rsAddBotClient = ClientId {client = "d"},
      rsAddBotName = Name {fromName = "4\a\FStZ7UW57\50835\&5\b\27742\ETBJ\GSE~>~\FS%^\133042\38979u\EM\US"},
      rsAddBotColour = ColourId {fromColourId = 3},
      rsAddBotAssets = [(ImageAsset "\1082326" (Just AssetComplete)), (ImageAsset "" (Just AssetComplete))],
      rsAddBotEvent =
        ( Event
            (ConvCodeDelete)
            (Qualified (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000000000000"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000400000002"))) (Domain "faraway.example.com"))
            (read "1864-05-14 03:03:50.569 UTC")
            (EdConvCodeDelete)
        )
    }

testObject_AddBotResponse_user_20 :: AddBotResponse
testObject_AddBotResponse_user_20 =
  AddBotResponse
    { rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000001"))),
      rsAddBotClient = ClientId {client = "b"},
      rsAddBotName =
        Name
          { fromName =
              "hn<t\ESCF\1049708\&1\8640\1013871\rHo\42150\&2il+\154829%E\1023597\30168q\70313\&6\DC4-x\ESC\f\rf\63824\38026e6=\1026969x\20152\18285\1014019\1110690\"\51817\DLE\48592\29710Uty+?[#R\1010159<<\23046\&9\983087\STXP\SUBh\DC3\t\a\SI\SI[@\\\".\ESC\153070;\1098598\156987\1063103^D3!dVo:\"wM\ETB"
          },
      rsAddBotColour = ColourId {fromColourId = -1},
      rsAddBotAssets = [(ImageAsset "\12303" (Just AssetComplete))],
      rsAddBotEvent =
        ( Event
            (ConvMessageTimerUpdate)
            (Qualified (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000200000004"))) (Domain "faraway.example.com"))
            (Qualified (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000004"))) (Domain "faraway.example.com"))
            (read "1864-05-08 05:48:34.348 UTC")
            ( ( EdConvMessageTimerUpdate
                  (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 3346692440762670})})
              )
            )
        )
    }
