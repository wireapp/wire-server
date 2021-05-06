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
testObject_RemoveBotResponse_user_1 = RemoveBotResponse {rsRemoveBotEvent = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "00004afa-0000-42cc-0000-0fdc00005d8f")))) ((Id (fromJust (UUID.fromString "00000498-0000-31dc-0000-265200004810")))) (read "1864-05-13 13:03:04.707865862794 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "9"}, otrRecipient = ClientId {client = "7"}, otrCiphertext = "", otrData = Just "5\DC4u"}))))}
testObject_RemoveBotResponse_user_2 :: RemoveBotResponse
testObject_RemoveBotResponse_user_2 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "000065e6-0000-6654-0000-062b00006fc6")))) ((Id (fromJust (UUID.fromString "00006c4b-0000-6333-0000-64ac0000501b")))) (read "1864-06-06 18:38:40.595635753445 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 9136}}))))}
testObject_RemoveBotResponse_user_3 :: RemoveBotResponse
testObject_RemoveBotResponse_user_3 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCreate) ((Id (fromJust (UUID.fromString "00000c4a-0000-78e9-0000-752b00002a10")))) ((Id (fromJust (UUID.fromString "000011dd-0000-38c9-0000-22cf00005636")))) (read "1864-05-24 23:05:26.875901535473 UTC") (Just (EdConversation (Conversation {cnvId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), cnvAccess = [], cnvAccessRole = NonActivatedAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), memOtrMuted = False, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "ggmfx3pbu0oq8vto0dewlz7nuaxqz1lx6cymvfqqilo"))}, cmOthers = [OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "dq1m73s7uea4ck98o5gkys_y49l022zptozv2es4h8r6m0cu6j8ci6vluw60eyolnh5e89ymz"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "sey6j07tw2jk5eeysrlvvh33ugr6aq1_i28iagodrb1i10xzn8sceqs0qg4apo07udvrm6f30p2bj96j285xu_ir8ym0s12putbmj"))},OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), omConvRoleName = (fromJust (parseRoleName "wuel22p9b2q4r7zhwxb29errcs_2_khxjeslt1vubzmrn2ffga17yo"))}]}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), cnvMessageTimer = Just (Ms {ms = 4}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})}))))}
testObject_RemoveBotResponse_user_4 :: RemoveBotResponse
testObject_RemoveBotResponse_user_4 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvRename) ((Id (fromJust (UUID.fromString "000047cd-0000-03fd-0000-700600001b9b")))) ((Id (fromJust (UUID.fromString "0000596a-0000-572e-0000-1cce00001657")))) (read "1864-05-06 21:24:08.258632134806 UTC") (Just (EdConvRename (ConversationRename {cupName = "P\168009\1069571\188259"}))))}
testObject_RemoveBotResponse_user_5 :: RemoveBotResponse
testObject_RemoveBotResponse_user_5 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "00003098-0000-56ad-0000-78790000307b")))) ((Id (fromJust (UUID.fromString "000054a2-0000-588e-0000-5ebb000050ce")))) (read "1864-05-05 20:49:23.957522359298 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("2GHCnvCixon8iyvEiA8V")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("ckfdOuK")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}))))}
