{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RemoveBotResponse_user where
import Data.Code ( Key(Key, asciiKey), Value(Value, asciiValue) )
import Data.Coerce ( coerce )
import Data.Id ( ClientId(ClientId, client), Id(Id) )
import Data.Misc ( HttpsUrl(HttpsUrl), Milliseconds(Ms, ms) )
import Data.Range ( unsafeRange )
import Data.Text.Ascii ( AsciiChars(validate) )
import Imports
    ( Bool(False, True),
      Maybe(Just, Nothing),
      undefined,
      read,
      fromRight,
      fromJust )
import qualified Data.UUID as UUID ( fromString )
import URI.ByteString
    ( URIRef(URI, uriScheme, uriAuthority, uriPath, uriQuery,
             uriFragment),
      Authority(Authority, authorityUserInfo, authorityHost,
                authorityPort),
      Host(Host, hostBS),
      Query(Query, queryPairs),
      Scheme(Scheme, schemeBS) )
import Wire.API.Conversation
    ( Access(CodeAccess, PrivateAccess, LinkAccess, InviteAccess),
      AccessRole(NonActivatedAccessRole),
      ConversationAccessUpdate(ConversationAccessUpdate, cupAccess,
                               cupAccessRole),
      ConversationMessageTimerUpdate(ConversationMessageTimerUpdate,
                                     cupMessageTimer),
      ConversationReceiptModeUpdate(ConversationReceiptModeUpdate,
                                    cruReceiptMode),
      ConversationRename(ConversationRename, cupName),
      ReceiptMode(ReceiptMode, unReceiptMode) )
import Wire.API.Conversation.Bot ( RemoveBotResponse(..) )
import Wire.API.Conversation.Code
    ( ConversationCode(ConversationCode, conversationKey,
                       conversationCode, conversationUri) )
import Wire.API.Conversation.Role ( parseRoleName )
import Wire.API.Conversation.Typing
    ( TypingData(TypingData, tdStatus),
      TypingStatus(StartedTyping, StoppedTyping) )
import Wire.API.Event.Conversation
    ( Event(Event),
      EventData(EdTyping, EdMembersJoin, EdConvMessageTimerUpdate,
                EdOtrMessage, EdConvAccessUpdate, EdConvReceiptModeUpdate,
                EdMemberUpdate, EdConvRename, EdConvCodeUpdate),
      EventType(Typing, MemberJoin, ConvMessageTimerUpdate,
                OtrMessageAdd, ConvCodeDelete, ConvAccessUpdate,
                ConvReceiptModeUpdate, MemberStateUpdate, ConvRename,
                ConvCodeUpdate),
      MemberUpdateData(MemberUpdateData, misTarget, misOtrMuted,
                       misOtrMutedStatus, misOtrMutedRef, misOtrArchived,
                       misOtrArchivedRef, misHidden, misHiddenRef, misConvRoleName),
      OtrMessage(OtrMessage, otrSender, otrRecipient, otrCiphertext,
                 otrData),
      SimpleMember(SimpleMember, smId, smConvRoleName),
      SimpleMembers(SimpleMembers, mMembers) )

testObject_RemoveBotResponse_user_1 :: RemoveBotResponse
testObject_RemoveBotResponse_user_1 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvRename) ((Id (fromJust (UUID.fromString "00005cc5-0000-50d8-0000-632400002684")))) ((Id (fromJust (UUID.fromString "000036b3-0000-4693-0000-0cb4000043ef")))) (read "1864-06-01 21:53:14.551 UTC") (Just (EdConvRename (ConversationRename {cupName = "\GS\1084645*8E\35060\65153@J"}))))}
testObject_RemoveBotResponse_user_2 :: RemoveBotResponse
testObject_RemoveBotResponse_user_2 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "000041f2-0000-51d8-0000-37ef000066ba")))) ((Id (fromJust (UUID.fromString "00000007-0000-31b5-0000-5ab0000074fa")))) (read "1864-05-15 05:17:31.646 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 3637317759218872})}))))}
testObject_RemoveBotResponse_user_3 :: RemoveBotResponse
testObject_RemoveBotResponse_user_3 = RemoveBotResponse {rsRemoveBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "00006dec-0000-6c06-0000-047a0000234a")))) ((Id (fromJust (UUID.fromString "000053bb-0000-6580-0000-738b00005757")))) (read "1864-06-06 21:20:26.363 UTC") (Just (EdTyping (TypingData {tdStatus = StartedTyping}))))}
testObject_RemoveBotResponse_user_4 :: RemoveBotResponse
testObject_RemoveBotResponse_user_4 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberJoin) ((Id (fromJust (UUID.fromString "00005458-0000-4b80-0000-76f30000209f")))) ((Id (fromJust (UUID.fromString "00002872-0000-1084-0000-382800003364")))) (read "1864-05-20 08:39:21.877 UTC") (Just (EdMembersJoin (SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000054-0000-0071-0000-003600000070"))), smConvRoleName = (fromJust (parseRoleName "h1z8eg3xbf8slj_wmotmw68n_0r4yt9432"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000005a-0000-004f-0000-004300000059"))), smConvRoleName = (fromJust (parseRoleName "a_55e2bxmn1qahiulr8nk8eg0z5feo37lfglq13t02f5gq7705_phj57t0s7m4j6mp6e0kmqxr7voq9685n0il5l975u5axki_sjtwbmqr5ck_i3xhz9_iemj1hfvs3a"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000002-0000-004e-0000-006c00000076"))), smConvRoleName = (fromJust (parseRoleName "_83xymchs405udzty412u25zhfmhuz4zdb0q25s"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000066-0000-0032-0000-000d0000007c"))), smConvRoleName = (fromJust (parseRoleName "yb3bcwn4rivs5lpx43_4gyjweiy9lxvqbmo9d2fmaf2pzmmdbim61_aup6qe8jqcx75jt"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000078-0000-000e-0000-000d00000059"))), smConvRoleName = (fromJust (parseRoleName "2cx90x7fnfjfp8dqgs0s0iwhpmlxr7wx9d4_bgpdilrdado2o5z3_omwb2_hwrwsnsdkhlqu_ywoqi8_vgtvrtjwtlt95fobg7ecl5sn37v9e"))}]}))))}
testObject_RemoveBotResponse_user_5 :: RemoveBotResponse
testObject_RemoveBotResponse_user_5 = RemoveBotResponse {rsRemoveBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "00001509-0000-2d53-0000-1d430000694f")))) ((Id (fromJust (UUID.fromString "0000582d-0000-2130-0000-1e91000076b2")))) (read "1864-04-17 04:49:29.662 UTC") (Just (EdTyping (TypingData {tdStatus = StartedTyping}))))}
testObject_RemoveBotResponse_user_6 :: RemoveBotResponse
testObject_RemoveBotResponse_user_6 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "00003ebb-0000-7e13-0000-4c1f00006a0f")))) ((Id (fromJust (UUID.fromString "00002c4b-0000-396b-0000-5c7f00003fd3")))) (read "1864-04-28 04:25:48.961 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 3593923685021570})}))))}
testObject_RemoveBotResponse_user_7 :: RemoveBotResponse
testObject_RemoveBotResponse_user_7 = RemoveBotResponse {rsRemoveBotEvent = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "00002104-0000-4cae-0000-622f00007791")))) ((Id (fromJust (UUID.fromString "00005d30-0000-344d-0000-1f42000004c0")))) (read "1864-05-11 10:05:46.47 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "4"}, otrRecipient = ClientId {client = "5"}, otrCiphertext = "\RS\ETB\SO\16371Y", otrData = Just "\83218\CAN\USEE"}))))}
testObject_RemoveBotResponse_user_8 :: RemoveBotResponse
testObject_RemoveBotResponse_user_8 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCodeDelete) ((Id (fromJust (UUID.fromString "0000239b-0000-4119-0000-2643000004b9")))) ((Id (fromJust (UUID.fromString "00007a1d-0000-25eb-0000-77fa0000065e")))) (read "1864-06-07 11:17:47.653 UTC") (Nothing))}
testObject_RemoveBotResponse_user_9 :: RemoveBotResponse
testObject_RemoveBotResponse_user_9 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "000077a8-0000-5f6b-0000-368c00005aae")))) ((Id (fromJust (UUID.fromString "00000ac8-0000-65f4-0000-446800006259")))) (read "1864-04-26 09:37:33.785 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -4305}}))))}
testObject_RemoveBotResponse_user_10 :: RemoveBotResponse
testObject_RemoveBotResponse_user_10 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCodeDelete) ((Id (fromJust (UUID.fromString "00005733-0000-2066-0000-6ceb00002633")))) ((Id (fromJust (UUID.fromString "000077d9-0000-6126-0000-412500000062")))) (read "1864-04-09 22:46:47.74 UTC") (Nothing))}
testObject_RemoveBotResponse_user_11 :: RemoveBotResponse
testObject_RemoveBotResponse_user_11 = RemoveBotResponse {rsRemoveBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "00002bdf-0000-2155-0000-492700007119")))) ((Id (fromJust (UUID.fromString "00003f38-0000-71ec-0000-74fa00002113")))) (read "1864-06-03 12:22:43.693 UTC") (Just (EdTyping (TypingData {tdStatus = StoppedTyping}))))}
testObject_RemoveBotResponse_user_12 :: RemoveBotResponse
testObject_RemoveBotResponse_user_12 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvAccessUpdate) ((Id (fromJust (UUID.fromString "000040b1-0000-6f23-0000-40fe00001642")))) ((Id (fromJust (UUID.fromString "00002d0b-0000-3559-0000-34cd000079b5")))) (read "1864-06-01 08:55:31.352 UTC") (Just (EdConvAccessUpdate (ConversationAccessUpdate {cupAccess = [PrivateAccess], cupAccessRole = NonActivatedAccessRole}))))}
testObject_RemoveBotResponse_user_13 :: RemoveBotResponse
testObject_RemoveBotResponse_user_13 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "000073ac-0000-43e6-0000-1e8b00005693")))) ((Id (fromJust (UUID.fromString "00002ef9-0000-6fca-0000-18ee00002f22")))) (read "1864-04-24 00:55:39.58 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("zqPLyqV4SfyC-pzXhZN5")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("Tpi8RzPG")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}))))}
testObject_RemoveBotResponse_user_14 :: RemoveBotResponse
testObject_RemoveBotResponse_user_14 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvAccessUpdate) ((Id (fromJust (UUID.fromString "00005ac7-0000-3003-0000-0a2800000a41")))) ((Id (fromJust (UUID.fromString "00001c2e-0000-4925-0000-141d00004276")))) (read "1864-05-11 23:24:53.366 UTC") (Just (EdConvAccessUpdate (ConversationAccessUpdate {cupAccess = [CodeAccess,InviteAccess,LinkAccess,InviteAccess,CodeAccess,InviteAccess,CodeAccess,CodeAccess], cupAccessRole = NonActivatedAccessRole}))))}
testObject_RemoveBotResponse_user_15 :: RemoveBotResponse
testObject_RemoveBotResponse_user_15 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvRename) ((Id (fromJust (UUID.fromString "00000db5-0000-33cf-0000-2a3e000045bb")))) ((Id (fromJust (UUID.fromString "000046bf-0000-0759-0000-672600004a90")))) (read "1864-04-24 13:40:56.394 UTC") (Just (EdConvRename (ConversationRename {cupName = "e\STXz%\46234\173340D\a6\1036247Oo\ACK\NAKiN\22645L\1023458\US\1050580\&9"}))))}
testObject_RemoveBotResponse_user_16 :: RemoveBotResponse
testObject_RemoveBotResponse_user_16 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "00007c68-0000-5886-0000-557a0000421f")))) ((Id (fromJust (UUID.fromString "00000ceb-0000-6df6-0000-6e8b00000424")))) (read "1864-05-25 10:48:39.878 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -1240}}))))}
testObject_RemoveBotResponse_user_17 :: RemoveBotResponse
testObject_RemoveBotResponse_user_17 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberStateUpdate) ((Id (fromJust (UUID.fromString "00001d37-0000-2069-0000-393400002302")))) ((Id (fromJust (UUID.fromString "00002d2a-0000-0e9c-0000-7c6400007c0c")))) (read "1864-04-14 17:50:21.953 UTC") (Just (EdMemberUpdate (MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), misOtrMuted = Just True, misOtrMutedStatus = Nothing, misOtrMutedRef = Nothing, misOtrArchived = Just True, misOtrArchivedRef = Just "\CAN(", misHidden = Just False, misHiddenRef = Just "f_", misConvRoleName = Just (fromJust (parseRoleName "bf45qco8v_n8go98l8upjc5mok2_1x2dbwlfgc_ggliuv76jrthkee29_oiy9hx2r5s0rchwlm02agb_oz66nm0onwing_6_dtjcxzf2s0hc276yjzbyf9u0o0w"))}))))}
testObject_RemoveBotResponse_user_18 :: RemoveBotResponse
testObject_RemoveBotResponse_user_18 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvRename) ((Id (fromJust (UUID.fromString "000075f7-0000-4d71-0000-4e6b000056cb")))) ((Id (fromJust (UUID.fromString "00003035-0000-1e99-0000-7bbb0000366a")))) (read "1864-05-21 23:20:18.627 UTC") (Just (EdConvRename (ConversationRename {cupName = "{\1072568\1005570b\RS\991585\SOH.A\SUB.x\vl"}))))}
testObject_RemoveBotResponse_user_19 :: RemoveBotResponse
testObject_RemoveBotResponse_user_19 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "000004f9-0000-3c9b-0000-55dc00004dde")))) ((Id (fromJust (UUID.fromString "00002d81-0000-036e-0000-2a9700000bb7")))) (read "1864-05-12 04:25:26.077 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("9_KprcgyULpl5m1D12zR")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("FyXpa78XvDWrk0")))))}, conversationUri = Nothing}))))}
testObject_RemoveBotResponse_user_20 :: RemoveBotResponse
testObject_RemoveBotResponse_user_20 = RemoveBotResponse {rsRemoveBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "000068e0-0000-2494-0000-4a9300000505")))) ((Id (fromJust (UUID.fromString "00000c31-0000-1c1a-0000-23b60000561b")))) (read "1864-04-13 02:10:26.238 UTC") (Just (EdTyping (TypingData {tdStatus = StartedTyping}))))}
