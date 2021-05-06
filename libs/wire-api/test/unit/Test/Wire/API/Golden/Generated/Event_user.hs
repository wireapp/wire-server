{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Event_user where

import Codec.MIME.Type (Type(..))
import qualified Codec.MIME.Type as MIME
import Control.Lens ((.~))
import Data.Code
import Data.Coerce
import Data.Currency
import Data.Domain
import Data.Handle
import Data.Id
import Data.ISO3166_CountryCodes
import Data.Json.Util
import Data.List1
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc
import Data.PEM
import Data.Qualified
import Data.Range (unsafeRange)
import qualified Data.Set as Set
import Data.Text.Ascii
import Data.Time (secondsToNominalDiffTime)
import Imports hiding (LT, GT)
import qualified Data.LanguageCodes
import qualified Data.UUID as UUID
import Test.Tasty (testGroup, TestTree)
import URI.ByteString
import qualified Wire.API.Call.Config as CallConfig
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Bot as Provider
import qualified Wire.API.Provider.External as Provider
import qualified Wire.API.Provider.Service as Provider
import qualified Wire.API.Provider.Service.Tag as Provider
import Data.Aeson
import GHC.Exts
import Wire.API.Asset
import Wire.API.Asset.V3.Resumable
import Wire.API.Call.Config
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Notification (QueuedNotification, queuedNotification, QueuedNotificationList, queuedNotificationList)
import Wire.API.Properties
-- import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
-- import Wire.API.Provider.Service.Tag
import Wire.API.Push.Token hiding (Transport)
import qualified Wire.API.Push.Token as Push.Token
import Wire.API.Team
import Wire.API.Team.Role
-- import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.API.Wrapped
testObject_Event_user_1 :: Event
testObject_Event_user_1 = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "000070df-0000-374e-0000-35fd0000408f")))) ((Id (fromJust (UUID.fromString "0000716d-0000-3c2a-0000-312700004b5a")))) (read "1864-05-04 11:37:17.544 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -433}}))))
testObject_Event_user_2 :: Event
testObject_Event_user_2 = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "00003dc9-0000-58a4-0000-1f7400000248")))) ((Id (fromJust (UUID.fromString "000061f1-0000-6894-0000-24cd00004638")))) (read "1864-04-19 02:27:35.284 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "2"}, otrRecipient = ClientId {client = "1a"}, otrCiphertext = "", otrData = Just "Q\170515O!G"}))))
testObject_Event_user_3 :: Event
testObject_Event_user_3 = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "000035bb-0000-7544-0000-422300002615")))) ((Id (fromJust (UUID.fromString "00003229-0000-27c0-0000-0189000033e9")))) (read "1864-04-15 13:03:37.354 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("04j3Ji80UTv_Xh76vNI9")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("agVVWHbYIW_k")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}))))
testObject_Event_user_4 :: Event
testObject_Event_user_4 = (Event (ConvRename) ((Id (fromJust (UUID.fromString "00004f26-0000-1512-0000-38a400005d9f")))) ((Id (fromJust (UUID.fromString "000032c3-0000-0ad5-0000-23fe00001c6b")))) (read "1864-06-06 01:37:42.851 UTC") (Just (EdConvRename (ConversationRename {cupName = "|G\44390s\120886:"}))))
testObject_Event_user_5 :: Event
testObject_Event_user_5 = (Event (Typing) ((Id (fromJust (UUID.fromString "0000528f-0000-550e-0000-10ff00007d94")))) ((Id (fromJust (UUID.fromString "00007a2d-0000-6815-0000-35b500004d8c")))) (read "1864-05-14 01:32:39.693 UTC") (Just (EdTyping (TypingData {tdStatus = StartedTyping}))))
testObject_Event_user_6 :: Event
testObject_Event_user_6 = (Event (MemberJoin) ((Id (fromJust (UUID.fromString "0000491f-0000-5755-0000-3ff100007335")))) ((Id (fromJust (UUID.fromString "0000391e-0000-2154-0000-5ac500002528")))) (read "1864-04-13 15:37:36.455 UTC") (Just (EdMembersJoin (SimpleMembers {mMembers = []}))))
testObject_Event_user_7 :: Event
testObject_Event_user_7 = (Event (MemberJoin) ((Id (fromJust (UUID.fromString "0000573c-0000-56bd-0000-79bf0000072e")))) ((Id (fromJust (UUID.fromString "000055e5-0000-1fab-0000-463600004ee6")))) (read "1864-04-23 10:05:21.251 UTC") (Just (EdMembersJoin (SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000065-0000-002b-0000-00290000004b"))), smConvRoleName = (fromJust (parseRoleName "egj3qhk4vubhanv5g5j23kfpc4xnm6r8mk9u0mmw5l436ku"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000052-0000-001e-0000-00190000005e"))), smConvRoleName = (fromJust (parseRoleName "l9jwz"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007b-0000-005c-0000-004e00000074"))), smConvRoleName = (fromJust (parseRoleName "30903fyayp3a8x5ql7ndeg4zbx2w_kfl273ybzc"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000000a-0000-0078-0000-00420000005b"))), smConvRoleName = (fromJust (parseRoleName "fws9dz4x5l3o5e9b3q2xxltnbakqnp8y96j"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000018-0000-003d-0000-000e0000003e"))), smConvRoleName = (fromJust (parseRoleName "o2n7smh__ul"))}]}))))
testObject_Event_user_8 :: Event
testObject_Event_user_8 = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "000069f7-0000-21c4-0000-700b000004fc")))) ((Id (fromJust (UUID.fromString "00006030-0000-080f-0000-7f430000606b")))) (read "1864-04-21 20:50:30.776 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000300000003"))), cMessage = Just "\148893", cName = Just "\b)hdiG(", cEmail = Nothing}))))
testObject_Event_user_9 :: Event
testObject_Event_user_9 = (Event (Typing) ((Id (fromJust (UUID.fromString "00004e54-0000-1906-0000-664e000038a4")))) ((Id (fromJust (UUID.fromString "000059bf-0000-11e7-0000-525e00004366")))) (read "1864-04-13 03:49:21.898 UTC") (Just (EdTyping (TypingData {tdStatus = StoppedTyping}))))
testObject_Event_user_10 :: Event
testObject_Event_user_10 = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "00007074-0000-4dfd-0000-12c1000036da")))) ((Id (fromJust (UUID.fromString "000025bb-0000-748a-0000-1a6d0000100c")))) (read "1864-05-07 21:11:45.231 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Nothing}))))
testObject_Event_user_11 :: Event
testObject_Event_user_11 = (Event (MemberLeave) ((Id (fromJust (UUID.fromString "00001fa9-0000-1fa3-0000-0a21000043e2")))) ((Id (fromJust (UUID.fromString "00004218-0000-1238-0000-5ace00005802")))) (read "1864-04-24 00:13:49.017 UTC") (Just (EdMembersLeave (UserIdList {mUsers = [(Id (fromJust (UUID.fromString "0000684a-0000-44fc-0000-249800004e7f"))),(Id (fromJust (UUID.fromString "00006a0e-0000-7a75-0000-5a4700002445"))),(Id (fromJust (UUID.fromString "000056d1-0000-3ef8-0000-671000003db8"))),(Id (fromJust (UUID.fromString "00007671-0000-33fa-0000-615100002e7e"))),(Id (fromJust (UUID.fromString "000048ef-0000-5bce-0000-2d33000063de"))),(Id (fromJust (UUID.fromString "000062a4-0000-5bb3-0000-24f700006f5b"))),(Id (fromJust (UUID.fromString "00000f98-0000-6de3-0000-71b500006048"))),(Id (fromJust (UUID.fromString "00000525-0000-42ce-0000-1d41000072e5"))),(Id (fromJust (UUID.fromString "00003bc2-0000-278b-0000-391e00007406"))),(Id (fromJust (UUID.fromString "0000692c-0000-4958-0000-267900000adb"))),(Id (fromJust (UUID.fromString "00001849-0000-7849-0000-173500004de6"))),(Id (fromJust (UUID.fromString "000030a2-0000-68ff-0000-76ec00002986"))),(Id (fromJust (UUID.fromString "00007ec8-0000-539f-0000-43ea00003074"))),(Id (fromJust (UUID.fromString "000004c2-0000-0bc2-0000-5f2900004f78"))),(Id (fromJust (UUID.fromString "00002a1f-0000-0e0c-0000-5561000052ef"))),(Id (fromJust (UUID.fromString "00006c7e-0000-3b1c-0000-5ff1000001ad"))),(Id (fromJust (UUID.fromString "00004034-0000-2dfc-0000-1edb0000012c"))),(Id (fromJust (UUID.fromString "00002cb1-0000-2ac3-0000-3d3000004835"))),(Id (fromJust (UUID.fromString "00007971-0000-03a2-0000-347800000aea"))),(Id (fromJust (UUID.fromString "00003470-0000-1be6-0000-3f700000144a"))),(Id (fromJust (UUID.fromString "00000007-0000-1c06-0000-4b4e00003c39")))]}))))
testObject_Event_user_12 :: Event
testObject_Event_user_12 = (Event (MemberStateUpdate) ((Id (fromJust (UUID.fromString "00001387-0000-3a71-0000-5cac00001596")))) ((Id (fromJust (UUID.fromString "00004abc-0000-036d-0000-2cf600007ba6")))) (read "1864-05-19 18:54:13.43 UTC") (Just (EdMemberUpdate (MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Just "", misOtrArchived = Just False, misOtrArchivedRef = Just "{,\r", misHidden = Nothing, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "ah7kk7r8soldtxknl38o8xztkuhyb3ssix399q87_wctlo8d7idrnikh8kyh2cs4itqlheh594zs2j9u2by12"))}))))
testObject_Event_user_13 :: Event
testObject_Event_user_13 = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "0000654d-0000-42ea-0000-364b0000079a")))) ((Id (fromJust (UUID.fromString "00007903-0000-5558-0000-0e69000027cd")))) (read "1864-05-31 03:55:41.772 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("NBx3SpzKAhTlLl94OsVV")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("waJAimuC")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}))))
testObject_Event_user_14 :: Event
testObject_Event_user_14 = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "000072a9-0000-647c-0000-78af0000470e")))) ((Id (fromJust (UUID.fromString "00000886-0000-696e-0000-72be0000215f")))) (read "1864-04-18 03:38:38.024 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("V19nR8eAPNTzGumG864=")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("ntOzhvRc43r0")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}))))
testObject_Event_user_15 :: Event
testObject_Event_user_15 = (Event (ConvCodeDelete) ((Id (fromJust (UUID.fromString "000016aa-0000-4e23-0000-01a300003f20")))) ((Id (fromJust (UUID.fromString "000011cb-0000-0751-0000-1fd800001a27")))) (read "1864-06-01 14:44:29.388 UTC") (Nothing))
testObject_Event_user_16 :: Event
testObject_Event_user_16 = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "00002990-0000-7789-0000-0a3800005dfe")))) ((Id (fromJust (UUID.fromString "00004cff-0000-75b3-0000-4c2f000077de")))) (read "1864-05-11 06:22:29.52 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 807928949418820})}))))
testObject_Event_user_17 :: Event
testObject_Event_user_17 = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "00005225-0000-7495-0000-2fbc00003359")))) ((Id (fromJust (UUID.fromString "000051b0-0000-6a68-0000-203a0000091f")))) (read "1864-04-20 15:54:44.881 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 6483917419649568})}))))
testObject_Event_user_18 :: Event
testObject_Event_user_18 = (Event (MemberLeave) ((Id (fromJust (UUID.fromString "000054bc-0000-6026-0000-63760000425f")))) ((Id (fromJust (UUID.fromString "0000419b-0000-298e-0000-420a00002a32")))) (read "1864-04-21 23:38:13.48 UTC") (Just (EdMembersLeave (UserIdList {mUsers = [(Id (fromJust (UUID.fromString "00002ec8-0000-4fbc-0000-457600004031"))),(Id (fromJust (UUID.fromString "00006852-0000-17f2-0000-190f00005618"))),(Id (fromJust (UUID.fromString "000048bf-0000-566c-0000-659e0000674f"))),(Id (fromJust (UUID.fromString "00005f1e-0000-5461-0000-48b5000070bf")))]}))))
testObject_Event_user_19 :: Event
testObject_Event_user_19 = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "0000142b-0000-0927-0000-784c0000103f")))) ((Id (fromJust (UUID.fromString "00005ebe-0000-5f1a-0000-49eb00001e93")))) (read "1864-05-20 23:00:59.751 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 6033769110720914})}))))
testObject_Event_user_20 :: Event
testObject_Event_user_20 = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "00007607-0000-4095-0000-4c5b00003be4")))) ((Id (fromJust (UUID.fromString "00006d19-0000-41f1-0000-1fed00006aec")))) (read "1864-05-04 21:22:51.458 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("4kQVfB_Yexp=X0GE0BdN")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("Vn1hQ5LEg5")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}))))
