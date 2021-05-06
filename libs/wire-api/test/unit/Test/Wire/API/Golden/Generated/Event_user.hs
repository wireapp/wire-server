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
testObject_Event_user_1 = (Event (ConvDelete) ((Id (fromJust (UUID.fromString "00001e06-0000-0327-0000-52bc00007740")))) ((Id (fromJust (UUID.fromString "00004a32-0000-46c7-0000-640000000aea")))) (read "1864-05-04 15:05:18.65 UTC") (Nothing))
testObject_Event_user_2 :: Event
testObject_Event_user_2 = (Event (ConvCodeDelete) ((Id (fromJust (UUID.fromString "000033f8-0000-3cb5-0000-1a9300001ee2")))) ((Id (fromJust (UUID.fromString "00001894-0000-63c2-0000-4fe100000a20")))) (read "1864-04-25 10:31:52.796 UTC") (Nothing))
testObject_Event_user_3 :: Event
testObject_Event_user_3 = (Event (ConvRename) ((Id (fromJust (UUID.fromString "000027d7-0000-491b-0000-6f1a00002e11")))) ((Id (fromJust (UUID.fromString "000022d9-0000-1b60-0000-28ff00002e14")))) (read "1864-06-08 20:50:13.485 UTC") (Just (EdConvRename (ConversationRename {cupName = "\1040936\1036285\DLE}L/\1020713Q"}))))
testObject_Event_user_4 :: Event
testObject_Event_user_4 = (Event (ConvRename) ((Id (fromJust (UUID.fromString "0000659f-0000-583d-0000-082d000010b9")))) ((Id (fromJust (UUID.fromString "00003d6d-0000-333d-0000-168300003999")))) (read "1864-05-26 00:08:05.188 UTC") (Just (EdConvRename (ConversationRename {cupName = "\171778\&4"}))))
testObject_Event_user_5 :: Event
testObject_Event_user_5 = (Event (ConvRename) ((Id (fromJust (UUID.fromString "00002c58-0000-0061-0000-49cb000056d4")))) ((Id (fromJust (UUID.fromString "000064c2-0000-7cb4-0000-4abf00004a9f")))) (read "1864-04-28 01:45:59.63 UTC") (Just (EdConvRename (ConversationRename {cupName = "\1028276\RSV\1056558\USx\1006337\FS3a\DC2\1099341\51944<3\DC372Pi\b\1087189\143494\138206\tr"}))))
testObject_Event_user_6 :: Event
testObject_Event_user_6 = (Event (Typing) ((Id (fromJust (UUID.fromString "00003507-0000-1480-0000-4071000077b5")))) ((Id (fromJust (UUID.fromString "000061c3-0000-334c-0000-6d9700000aa4")))) (read "1864-05-22 08:37:40.591 UTC") (Just (EdTyping (TypingData {tdStatus = StoppedTyping}))))
testObject_Event_user_7 :: Event
testObject_Event_user_7 = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "0000345c-0000-4f32-0000-395200002dc4")))) ((Id (fromJust (UUID.fromString "000032a2-0000-6dde-0000-6c5100005a42")))) (read "1864-04-13 03:05:14.584 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000500000005"))), cMessage = Just "b\99182", cName = Just "$\DLE", cEmail = Nothing}))))
testObject_Event_user_8 :: Event
testObject_Event_user_8 = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "0000580d-0000-0647-0000-6384000010b0")))) ((Id (fromJust (UUID.fromString "000004ef-0000-6d0d-0000-16b100000382")))) (read "1864-04-16 11:40:14.722 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 7412868800333129})}))))
testObject_Event_user_9 :: Event
testObject_Event_user_9 = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "00002d86-0000-4e37-0000-72f000007661")))) ((Id (fromJust (UUID.fromString "00001f62-0000-3790-0000-31b100000265")))) (read "1864-05-29 22:58:39.221 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000500000006"))), cMessage = Just "N\1089842}", cName = Just "\1004918\147543", cEmail = Just "\97642C;v0Z\\"}))))
testObject_Event_user_10 :: Event
testObject_Event_user_10 = (Event (ConvCodeDelete) ((Id (fromJust (UUID.fromString "000040be-0000-48ab-0000-4a46000036cf")))) ((Id (fromJust (UUID.fromString "000010c3-0000-1558-0000-1f3900001ff8")))) (read "1864-05-09 08:08:48.571 UTC") (Nothing))
testObject_Event_user_11 :: Event
testObject_Event_user_11 = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "000058b1-0000-401a-0000-56d900004482")))) ((Id (fromJust (UUID.fromString "00007f62-0000-7d50-0000-1bb400005f3d")))) (read "1864-04-17 23:03:15.783 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 7216281080458098})}))))
testObject_Event_user_12 :: Event
testObject_Event_user_12 = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "00001d37-0000-1271-0000-3f4200001903")))) ((Id (fromJust (UUID.fromString "000048f0-0000-4de1-0000-36f200004749")))) (read "1864-04-10 05:02:26.474 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 3545788489592779})}))))
testObject_Event_user_13 :: Event
testObject_Event_user_13 = (Event (MemberJoin) ((Id (fromJust (UUID.fromString "00003b66-0000-4218-0000-7fec00007199")))) ((Id (fromJust (UUID.fromString "00001263-0000-4f2c-0000-34c50000260c")))) (read "1864-05-25 06:18:28.761 UTC") (Just (EdMembersJoin (SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007e-0000-0033-0000-00690000004d"))), smConvRoleName = (fromJust (parseRoleName "94xcjozvxyzl_e4mx6gvmmvpla6a1mt3h1t2g7k"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000068-0000-0025-0000-00640000006e"))), smConvRoleName = (fromJust (parseRoleName "a5zym5c7fj7heos"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000004a-0000-0046-0000-005f0000005f"))), smConvRoleName = (fromJust (parseRoleName "40f_e5b3q0h1qasnrjfro9pmddkwy8rmk6sj1x8urdikmtm6jw7wazp4b3_uiup_mg50my8u_hsld_mj01oeruq4u9naxj4t0e1td3cxd12f20ceimw77aj_9g9x8c4"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000027-0000-0040-0000-00170000004e"))), smConvRoleName = (fromJust (parseRoleName "no_0bvzivi630um_65vw4ph7d1zonssf4c_z_1oalw4i5jcxgf4l3y3q1sl_i9rgoamkuhswpayexp0vnsw1sigdn6p4fx79g5u7uc7ryxcnv4a5"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007a-0000-007b-0000-00650000007d"))), smConvRoleName = (fromJust (parseRoleName "2017zlkox17l41p9py27vlijv_chohncwtpg4r89ex6dn2wn2870nzoghioyahva4_jszzl7jeqccyp2f2hd526iay6sjeogfxe2hw_8i1uh_4h2v107t6hqy"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "0000002d-0000-0014-0000-006500000032"))), smConvRoleName = (fromJust (parseRoleName "sbj74xsr4gazhocbvbe04f6ycr_jc7581khh057r0rgp1e05fqobgihxy4wqs80kubqqe4chgudh3z7_lih1zdrd1yhcg3d3jpfrxfehlpl6wm64als7ulpzq"))}]}))))
testObject_Event_user_14 :: Event
testObject_Event_user_14 = (Event (ConvRename) ((Id (fromJust (UUID.fromString "000075cc-0000-1602-0000-178400006172")))) ((Id (fromJust (UUID.fromString "00003703-0000-50d7-0000-31f000003add")))) (read "1864-05-31 17:13:50.106 UTC") (Just (EdConvRename (ConversationRename {cupName = "\1059557\83375\1043876\1046945\164920K\ENQ\1063598E"}))))
testObject_Event_user_15 :: Event
testObject_Event_user_15 = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "00001053-0000-3378-0000-6ee70000039c")))) ((Id (fromJust (UUID.fromString "000076ed-0000-039b-0000-06fb0000427a")))) (read "1864-06-05 10:07:56.154 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000400000008"))), cMessage = Just "uH?\SO", cName = Just "\187550#54u", cEmail = Nothing}))))
testObject_Event_user_16 :: Event
testObject_Event_user_16 = (Event (ConvCodeDelete) ((Id (fromJust (UUID.fromString "0000064f-0000-1791-0000-0e730000289d")))) ((Id (fromJust (UUID.fromString "0000195c-0000-491c-0000-47ee00007902")))) (read "1864-05-23 18:44:15.241 UTC") (Nothing))
testObject_Event_user_17 :: Event
testObject_Event_user_17 = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "00007707-0000-27f8-0000-0ccb00002240")))) ((Id (fromJust (UUID.fromString "00002588-0000-601b-0000-41e4000033db")))) (read "1864-05-08 14:31:39.765 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("nm8b-i=Wx=qetA-KeM_W")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("6YrXpVwQ7x2Yl=njO-R")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}))))
testObject_Event_user_18 :: Event
testObject_Event_user_18 = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "00005cc7-0000-7b99-0000-65f400000946")))) ((Id (fromJust (UUID.fromString "00007bd6-0000-7f91-0000-0502000008e2")))) (read "1864-06-01 04:48:58.54 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000400000001"))), cMessage = Nothing, cName = Just "s", cEmail = Just "X"}))))
testObject_Event_user_19 :: Event
testObject_Event_user_19 = (Event (Typing) ((Id (fromJust (UUID.fromString "000076ed-0000-42df-0000-66ee00007914")))) ((Id (fromJust (UUID.fromString "0000678e-0000-4323-0000-7cf6000005ba")))) (read "1864-05-04 16:59:38.294 UTC") (Just (EdTyping (TypingData {tdStatus = StartedTyping}))))
testObject_Event_user_20 :: Event
testObject_Event_user_20 = (Event (MemberStateUpdate) ((Id (fromJust (UUID.fromString "00006edf-0000-66f9-0000-49a000000f65")))) ((Id (fromJust (UUID.fromString "00005c9d-0000-0f08-0000-5a0a00001805")))) (read "1864-05-09 20:45:42.721 UTC") (Just (EdMemberUpdate (MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), misOtrMuted = Nothing, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Just "\DC1", misOtrArchived = Just False, misOtrArchivedRef = Just "<", misHidden = Just False, misHiddenRef = Just "\\!J", misConvRoleName = Just (fromJust (parseRoleName "5e8p82m525ftdacmi8l1tswktyxlm74wbslfy8ex9fbgq"))}))))
