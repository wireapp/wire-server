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
testObject_RemoveBotResponse_user_1 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCodeDelete) ((Id (fromJust (UUID.fromString "00003a80-0000-36bc-0000-583200001b5c")))) ((Id (fromJust (UUID.fromString "0000414d-0000-1e05-0000-7a2000001fc6")))) (read "1864-04-26 14:16:43.65 UTC") (Nothing))}
testObject_RemoveBotResponse_user_2 :: RemoveBotResponse
testObject_RemoveBotResponse_user_2 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCreate) ((Id (fromJust (UUID.fromString "00000934-0000-399a-0000-7e3b000002de")))) ((Id (fromJust (UUID.fromString "00005f2d-0000-2aab-0000-789d00006ac4")))) (read "1864-06-08 23:11:23.963 UTC") (Just (EdConversation (Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), cnvType = One2OneConv, cnvCreator = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), cnvAccess = [InviteAccess], cnvAccessRole = NonActivatedAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "", memOtrArchived = True, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "w33cw3yf7z6c_ky5d"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), cnvMessageTimer = Nothing, cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})}))))}
testObject_RemoveBotResponse_user_3 :: RemoveBotResponse
testObject_RemoveBotResponse_user_3 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberLeave) ((Id (fromJust (UUID.fromString "000063f1-0000-241a-0000-029f000079cf")))) ((Id (fromJust (UUID.fromString "00007086-0000-5d88-0000-6d5e0000077c")))) (read "1864-05-14 06:23:36.396 UTC") (Just (EdMembersLeave (UserIdList {mUsers = [(Id (fromJust (UUID.fromString "00001cc8-0000-463d-0000-1d4000007d0b"))),(Id (fromJust (UUID.fromString "00003d30-0000-74d1-0000-4a29000058f7"))),(Id (fromJust (UUID.fromString "000055c0-0000-17fd-0000-774000004a4a"))),(Id (fromJust (UUID.fromString "00000c24-0000-06e6-0000-22cc00002b94"))),(Id (fromJust (UUID.fromString "0000178f-0000-2a3c-0000-5ea50000365f"))),(Id (fromJust (UUID.fromString "00003e87-0000-4d98-0000-43320000583c"))),(Id (fromJust (UUID.fromString "00001258-0000-613b-0000-423500003dec"))),(Id (fromJust (UUID.fromString "00004200-0000-2d14-0000-6dc900003566"))),(Id (fromJust (UUID.fromString "00007d15-0000-2e74-0000-4fa500001281")))]}))))}
testObject_RemoveBotResponse_user_4 :: RemoveBotResponse
testObject_RemoveBotResponse_user_4 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "000074e8-0000-7211-0000-411c00003a89")))) ((Id (fromJust (UUID.fromString "00007a12-0000-1e3d-0000-55c80000555a")))) (read "1864-04-11 18:38:02.212 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("su3RFJTcN56a9c4H9lg7")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("MXntZrRYJ9nAVa3qPSz5")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}))))}
testObject_RemoveBotResponse_user_5 :: RemoveBotResponse
testObject_RemoveBotResponse_user_5 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "0000281f-0000-2c07-0000-5c2f00003ad8")))) ((Id (fromJust (UUID.fromString "0000071b-0000-54e6-0000-374700004481")))) (read "1864-05-10 14:05:25.86 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000400000008"))), cMessage = Nothing, cName = Just "A;\992451db$", cEmail = Nothing}))))}
