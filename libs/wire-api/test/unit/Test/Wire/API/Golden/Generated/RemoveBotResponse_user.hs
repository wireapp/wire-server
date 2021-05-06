{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RemoveBotResponse_user where

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
testObject_RemoveBotResponse_user_1 :: RemoveBotResponse
testObject_RemoveBotResponse_user_1 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "000036b9-0000-4eec-0000-62c9000075a3")))) ((Id (fromJust (UUID.fromString "000017a6-0000-0f74-0000-239d00003919")))) (read "1864-05-28 23:42:26.521 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000006"))), cMessage = Just "", cName = Just "\1026463", cEmail = Nothing}))))}
testObject_RemoveBotResponse_user_2 :: RemoveBotResponse
testObject_RemoveBotResponse_user_2 = RemoveBotResponse {rsRemoveBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "00003b44-0000-0d07-0000-4d6b00007630")))) ((Id (fromJust (UUID.fromString "00001a14-0000-3b4e-0000-525d00002d4d")))) (read "1864-04-30 19:51:26.066 UTC") (Just (EdTyping (TypingData {tdStatus = StartedTyping}))))}
testObject_RemoveBotResponse_user_3 :: RemoveBotResponse
testObject_RemoveBotResponse_user_3 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "00001674-0000-1c69-0000-758700001644")))) ((Id (fromJust (UUID.fromString "000036aa-0000-0cfc-0000-79ee000008f0")))) (read "1864-04-15 02:22:56.957 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -5316}}))))}
testObject_RemoveBotResponse_user_4 :: RemoveBotResponse
testObject_RemoveBotResponse_user_4 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "00005c96-0000-291a-0000-2a95000064c8")))) ((Id (fromJust (UUID.fromString "0000447b-0000-4769-0000-581a00007857")))) (read "1864-06-02 01:57:53.609 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -10409}}))))}
testObject_RemoveBotResponse_user_5 :: RemoveBotResponse
testObject_RemoveBotResponse_user_5 = RemoveBotResponse {rsRemoveBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "00005a82-0000-0d9c-0000-2fce0000462c")))) ((Id (fromJust (UUID.fromString "00004354-0000-4e2b-0000-4b4400001b70")))) (read "1864-04-09 22:55:41.913 UTC") (Just (EdTyping (TypingData {tdStatus = StoppedTyping}))))}
testObject_RemoveBotResponse_user_6 :: RemoveBotResponse
testObject_RemoveBotResponse_user_6 = RemoveBotResponse {rsRemoveBotEvent = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "00005fe4-0000-0b75-0000-298e000021d4")))) ((Id (fromJust (UUID.fromString "000032e5-0000-43e7-0000-796200005d44")))) (read "1864-05-13 07:51:15.546 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "1d"}, otrRecipient = ClientId {client = "5"}, otrCiphertext = "", otrData = Nothing}))))}
testObject_RemoveBotResponse_user_7 :: RemoveBotResponse
testObject_RemoveBotResponse_user_7 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberLeave) ((Id (fromJust (UUID.fromString "00005a54-0000-01ec-0000-6d0f000078ae")))) ((Id (fromJust (UUID.fromString "000041e5-0000-180c-0000-390a00002dc7")))) (read "1864-04-21 07:51:12.201 UTC") (Just (EdMembersLeave (UserIdList {mUsers = [(Id (fromJust (UUID.fromString "000038c2-0000-66f8-0000-084600001a0a"))),(Id (fromJust (UUID.fromString "000010db-0000-1f6d-0000-7a5f00000291"))),(Id (fromJust (UUID.fromString "0000322e-0000-347d-0000-0b4d00007b39"))),(Id (fromJust (UUID.fromString "00001b53-0000-2a1d-0000-139300000a7f"))),(Id (fromJust (UUID.fromString "000069c4-0000-1fd7-0000-4f6b00004c81"))),(Id (fromJust (UUID.fromString "000044bc-0000-1600-0000-00b100003e18"))),(Id (fromJust (UUID.fromString "00001d7d-0000-45d8-0000-1b3100007f04"))),(Id (fromJust (UUID.fromString "000041cd-0000-4649-0000-057800007fcc"))),(Id (fromJust (UUID.fromString "00007067-0000-02d8-0000-55750000211c"))),(Id (fromJust (UUID.fromString "00001fa6-0000-179c-0000-7318000064ac"))),(Id (fromJust (UUID.fromString "000052bc-0000-7978-0000-699e000025e6"))),(Id (fromJust (UUID.fromString "00002bee-0000-644d-0000-6b8c0000016b"))),(Id (fromJust (UUID.fromString "00006784-0000-7782-0000-035c000039df"))),(Id (fromJust (UUID.fromString "00003929-0000-5c93-0000-0c8d00001aa9"))),(Id (fromJust (UUID.fromString "00005808-0000-2e92-0000-25bd000052a5"))),(Id (fromJust (UUID.fromString "000046e4-0000-2aef-0000-067700002bf2"))),(Id (fromJust (UUID.fromString "00004bd0-0000-6eba-0000-1faa000053db"))),(Id (fromJust (UUID.fromString "0000682c-0000-1b6e-0000-57ca00006c64"))),(Id (fromJust (UUID.fromString "00002b39-0000-4217-0000-7f5800001d7b"))),(Id (fromJust (UUID.fromString "000061b8-0000-4a9a-0000-43ce00006088"))),(Id (fromJust (UUID.fromString "000066fb-0000-09e2-0000-2d0b00004847"))),(Id (fromJust (UUID.fromString "00000a01-0000-47b9-0000-7c8900001acb")))]}))))}
testObject_RemoveBotResponse_user_8 :: RemoveBotResponse
testObject_RemoveBotResponse_user_8 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "00001716-0000-10d8-0000-3791000030dd")))) ((Id (fromJust (UUID.fromString "00004186-0000-79bf-0000-332f00005028")))) (read "1864-06-04 16:35:46.8 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("YqUB0=3Ng0U=hSVnihoY")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("KAB4GY5f")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}))))}
testObject_RemoveBotResponse_user_9 :: RemoveBotResponse
testObject_RemoveBotResponse_user_9 = RemoveBotResponse {rsRemoveBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "00000685-0000-25bf-0000-1b830000258a")))) ((Id (fromJust (UUID.fromString "00002c72-0000-758c-0000-037500005e6d")))) (read "1864-05-10 08:44:06.443 UTC") (Just (EdTyping (TypingData {tdStatus = StoppedTyping}))))}
testObject_RemoveBotResponse_user_10 :: RemoveBotResponse
testObject_RemoveBotResponse_user_10 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberLeave) ((Id (fromJust (UUID.fromString "0000759a-0000-13c9-0000-5b7b0000447c")))) ((Id (fromJust (UUID.fromString "0000448c-0000-2bb3-0000-1a4a00006e19")))) (read "1864-04-20 03:13:39.802 UTC") (Just (EdMembersLeave (UserIdList {mUsers = [(Id (fromJust (UUID.fromString "00002157-0000-3023-0000-041200003f2a"))),(Id (fromJust (UUID.fromString "000065bf-0000-4ec7-0000-26dc00007e58"))),(Id (fromJust (UUID.fromString "00005998-0000-2b61-0000-718e00004bea"))),(Id (fromJust (UUID.fromString "000010f4-0000-76e9-0000-5dd100002cb8"))),(Id (fromJust (UUID.fromString "0000398c-0000-2a68-0000-4c1f00001ded"))),(Id (fromJust (UUID.fromString "00000d48-0000-1d15-0000-476100003ae6")))]}))))}
testObject_RemoveBotResponse_user_11 :: RemoveBotResponse
testObject_RemoveBotResponse_user_11 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCreate) ((Id (fromJust (UUID.fromString "00006177-0000-5be8-0000-36de000003ae")))) ((Id (fromJust (UUID.fromString "000028f2-0000-40e0-0000-0b0600005e1f")))) (read "1864-04-15 05:27:47.925 UTC") (Just (EdConversation (Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))), cnvAccess = [PrivateAccess], cnvAccessRole = PrivateAccessRole, cnvName = Just "\1075177H>", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Nothing, memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "o79c469wa"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "ohom8yrr_7k6e8wtc4qzmmy7br5z4f41_6rr5v094oy7"))}]}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))), cnvMessageTimer = Just (Ms {ms = 7381123082950796}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})}))))}
testObject_RemoveBotResponse_user_12 :: RemoveBotResponse
testObject_RemoveBotResponse_user_12 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvDelete) ((Id (fromJust (UUID.fromString "00003655-0000-03e2-0000-114d00003935")))) ((Id (fromJust (UUID.fromString "000040d4-0000-43ad-0000-0feb0000267e")))) (read "1864-05-16 07:46:20.254 UTC") (Nothing))}
testObject_RemoveBotResponse_user_13 :: RemoveBotResponse
testObject_RemoveBotResponse_user_13 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvAccessUpdate) ((Id (fromJust (UUID.fromString "000068fb-0000-6bc8-0000-269500006f5a")))) ((Id (fromJust (UUID.fromString "00002c41-0000-2a1b-0000-73480000464c")))) (read "1864-04-18 10:21:19.268 UTC") (Just (EdConvAccessUpdate (ConversationAccessUpdate {cupAccess = [InviteAccess,LinkAccess,InviteAccess,CodeAccess,CodeAccess,LinkAccess,PrivateAccess,InviteAccess], cupAccessRole = NonActivatedAccessRole}))))}
testObject_RemoveBotResponse_user_14 :: RemoveBotResponse
testObject_RemoveBotResponse_user_14 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCodeDelete) ((Id (fromJust (UUID.fromString "00003b0d-0000-1bc1-0000-3cd000001063")))) ((Id (fromJust (UUID.fromString "0000154f-0000-2fb2-0000-579b00006750")))) (read "1864-06-08 09:43:41.243 UTC") (Nothing))}
testObject_RemoveBotResponse_user_15 :: RemoveBotResponse
testObject_RemoveBotResponse_user_15 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "000036b6-0000-5a48-0000-403100004c47")))) ((Id (fromJust (UUID.fromString "0000207f-0000-79b5-0000-6ca00000018d")))) (read "1864-04-23 09:52:15.047 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Nothing}))))}
testObject_RemoveBotResponse_user_16 :: RemoveBotResponse
testObject_RemoveBotResponse_user_16 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "00006258-0000-511f-0000-7676000014e1")))) ((Id (fromJust (UUID.fromString "000021f3-0000-4a29-0000-6d680000553f")))) (read "1864-04-27 13:42:46.487 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Nothing}))))}
testObject_RemoveBotResponse_user_17 :: RemoveBotResponse
testObject_RemoveBotResponse_user_17 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCodeDelete) ((Id (fromJust (UUID.fromString "00003fec-0000-12fe-0000-55fd000036e1")))) ((Id (fromJust (UUID.fromString "0000456b-0000-0361-0000-0c0000000a55")))) (read "1864-06-05 03:38:28.041 UTC") (Nothing))}
testObject_RemoveBotResponse_user_18 :: RemoveBotResponse
testObject_RemoveBotResponse_user_18 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCodeDelete) ((Id (fromJust (UUID.fromString "00004f4c-0000-477c-0000-0b1600002256")))) ((Id (fromJust (UUID.fromString "000077e4-0000-0782-0000-10080000067a")))) (read "1864-06-04 21:46:22.362 UTC") (Nothing))}
testObject_RemoveBotResponse_user_19 :: RemoveBotResponse
testObject_RemoveBotResponse_user_19 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberLeave) ((Id (fromJust (UUID.fromString "00004595-0000-1a4f-0000-58500000798b")))) ((Id (fromJust (UUID.fromString "00000158-0000-2e2a-0000-7f3a000065bc")))) (read "1864-05-02 17:18:45.888 UTC") (Just (EdMembersLeave (UserIdList {mUsers = [(Id (fromJust (UUID.fromString "00001166-0000-12aa-0000-59ce000048d7")))]}))))}
testObject_RemoveBotResponse_user_20 :: RemoveBotResponse
testObject_RemoveBotResponse_user_20 = RemoveBotResponse {rsRemoveBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "00006a36-0000-03c7-0000-37bb000058aa")))) ((Id (fromJust (UUID.fromString "0000706f-0000-56e6-0000-4af800003904")))) (read "1864-04-20 19:18:46.372 UTC") (Just (EdTyping (TypingData {tdStatus = StartedTyping}))))}
