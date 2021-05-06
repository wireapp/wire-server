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
testObject_Event_user_1 = (Event (Typing) ((Id (fromJust (UUID.fromString "00006e6a-0000-314a-0000-0ac600004826")))) ((Id (fromJust (UUID.fromString "00006d5c-0000-0b7b-0000-18c200005473")))) (read "1864-05-01 03:56:40.159663204338 UTC") (Just (EdTyping (TypingData {tdStatus = StoppedTyping}))))
testObject_Event_user_2 :: Event
testObject_Event_user_2 = (Event (MemberJoin) ((Id (fromJust (UUID.fromString "000022f8-0000-3fe8-0000-6fa500001b59")))) ((Id (fromJust (UUID.fromString "00001ab8-0000-325d-0000-49d900001e46")))) (read "1864-04-19 21:41:28.848248572059 UTC") (Just (EdMembersJoin (SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "0000007b-0000-0011-0000-005900000072"))), smConvRoleName = (fromJust (parseRoleName "pmp326wnznt_2c53fbgii"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000008-0000-0074-0000-004800000020"))), smConvRoleName = (fromJust (parseRoleName "4oncyha4a37u_fvk9oxksq6p7nv89jow8eivsp6dilo9clnwvgt9vjj8cnmuqrwh060difsewsj4undb0bzv"))}]}))))
testObject_Event_user_3 :: Event
testObject_Event_user_3 = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "0000778b-0000-03d4-0000-768000005101")))) ((Id (fromJust (UUID.fromString "00006e75-0000-4bd3-0000-72e000006585")))) (read "1864-05-23 00:02:15.490818504027 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000500000007"))), cMessage = Nothing, cName = Just "[)", cEmail = Just "*"}))))
testObject_Event_user_4 :: Event
testObject_Event_user_4 = (Event (MemberLeave) ((Id (fromJust (UUID.fromString "000005d6-0000-5f19-0000-1eb5000050aa")))) ((Id (fromJust (UUID.fromString "00004940-0000-26c6-0000-297200001120")))) (read "1864-05-15 07:16:21.337757194905 UTC") (Just (EdMembersLeave (UserIdList {mUsers = [(Id (fromJust (UUID.fromString "0000581a-0000-6b41-0000-38660000642d"))),(Id (fromJust (UUID.fromString "0000302f-0000-0c2a-0000-0ea800004196"))),(Id (fromJust (UUID.fromString "000061d3-0000-10fd-0000-08de000060a0"))),(Id (fromJust (UUID.fromString "00001a07-0000-6c1d-0000-0dc600002485"))),(Id (fromJust (UUID.fromString "00001dd6-0000-435e-0000-12e2000010f3"))),(Id (fromJust (UUID.fromString "0000547a-0000-623f-0000-350f00002be4"))),(Id (fromJust (UUID.fromString "0000477a-0000-1f11-0000-3b1700005fcb"))),(Id (fromJust (UUID.fromString "00002323-0000-5b74-0000-39e100004c03"))),(Id (fromJust (UUID.fromString "00006141-0000-0c18-0000-797a00004144"))),(Id (fromJust (UUID.fromString "00001a69-0000-6e4a-0000-1de500004362"))),(Id (fromJust (UUID.fromString "00005542-0000-54f0-0000-245500002617"))),(Id (fromJust (UUID.fromString "00000256-0000-74d0-0000-5be5000053d4"))),(Id (fromJust (UUID.fromString "0000223a-0000-0fcf-0000-1cda00007fcc")))]}))))
testObject_Event_user_5 :: Event
testObject_Event_user_5 = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "000065ce-0000-3434-0000-511500001b98")))) ((Id (fromJust (UUID.fromString "0000335e-0000-2550-0000-33c700004755")))) (read "1864-05-28 06:12:20.839893256998 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "7"}, otrRecipient = ClientId {client = "2"}, otrCiphertext = "`*\172069\DC4", otrData = Just "4m"}))))
