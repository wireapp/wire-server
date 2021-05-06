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
testObject_Event_user_1 = (Event (MemberLeave) ((Id (fromJust (UUID.fromString "00006d0d-0000-1f31-0000-37bd000060bc")))) ((Id (fromJust (UUID.fromString "000035ce-0000-299d-0000-0b5700000edc")))) (read "1864-04-21 17:38:35.126 UTC") (Just (EdMembersLeave (UserIdList {mUsers = [(Id (fromJust (UUID.fromString "000073c2-0000-5f76-0000-0f0200002350"))),(Id (fromJust (UUID.fromString "000047c5-0000-3b25-0000-3297000022ed"))),(Id (fromJust (UUID.fromString "000022ac-0000-0fd1-0000-1be200006353"))),(Id (fromJust (UUID.fromString "0000604b-0000-3df3-0000-619900006c3f"))),(Id (fromJust (UUID.fromString "0000476a-0000-4c28-0000-77a8000055ed"))),(Id (fromJust (UUID.fromString "00003dbd-0000-51ad-0000-136600006f2c"))),(Id (fromJust (UUID.fromString "000048f0-0000-60d0-0000-21f700004e54"))),(Id (fromJust (UUID.fromString "000074f6-0000-609c-0000-15b700003916"))),(Id (fromJust (UUID.fromString "00004056-0000-22c9-0000-05b500005cf7"))),(Id (fromJust (UUID.fromString "00003b63-0000-399f-0000-359600005585"))),(Id (fromJust (UUID.fromString "00004a7b-0000-087e-0000-4c4d00004bc0"))),(Id (fromJust (UUID.fromString "000018bd-0000-41e4-0000-325a00006c94"))),(Id (fromJust (UUID.fromString "00000d90-0000-01bf-0000-4c260000713c"))),(Id (fromJust (UUID.fromString "0000314d-0000-1cf7-0000-50e40000500f"))),(Id (fromJust (UUID.fromString "00003a76-0000-6144-0000-665d00000c63"))),(Id (fromJust (UUID.fromString "00002877-0000-443f-0000-3c7100007b49")))]}))))
testObject_Event_user_2 :: Event
testObject_Event_user_2 = (Event (MemberLeave) ((Id (fromJust (UUID.fromString "00004f9c-0000-4475-0000-69b200006571")))) ((Id (fromJust (UUID.fromString "00002f11-0000-24cc-0000-5cee0000766b")))) (read "1864-05-16 02:37:54.997 UTC") (Just (EdMembersLeave (UserIdList {mUsers = [(Id (fromJust (UUID.fromString "00006348-0000-3416-0000-512000006f83"))),(Id (fromJust (UUID.fromString "00001849-0000-1822-0000-373c0000254f"))),(Id (fromJust (UUID.fromString "00000f0a-0000-417b-0000-0e9500000918"))),(Id (fromJust (UUID.fromString "00006e87-0000-0ea5-0000-4ec400003c5b"))),(Id (fromJust (UUID.fromString "00000af1-0000-3260-0000-536900001d1e"))),(Id (fromJust (UUID.fromString "0000046a-0000-08bc-0000-6bda000044b2"))),(Id (fromJust (UUID.fromString "0000638e-0000-787e-0000-21c8000045f3"))),(Id (fromJust (UUID.fromString "00002ccd-0000-399a-0000-03ba0000664f"))),(Id (fromJust (UUID.fromString "000049f1-0000-429a-0000-06cb00000d3a"))),(Id (fromJust (UUID.fromString "00001d8b-0000-3e66-0000-4016000019d5"))),(Id (fromJust (UUID.fromString "00000d11-0000-6c21-0000-67f600001081"))),(Id (fromJust (UUID.fromString "00004172-0000-16c8-0000-567d00000a22"))),(Id (fromJust (UUID.fromString "00003562-0000-2a1d-0000-2d32000037ce"))),(Id (fromJust (UUID.fromString "00000772-0000-5f39-0000-087900001b49")))]}))))
testObject_Event_user_3 :: Event
testObject_Event_user_3 = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "000078db-0000-1e53-0000-4bca000014db")))) ((Id (fromJust (UUID.fromString "0000487c-0000-7ed9-0000-626400002cf2")))) (read "1864-06-05 04:01:56.697 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -7034}}))))
testObject_Event_user_4 :: Event
testObject_Event_user_4 = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "00005e5c-0000-4014-0000-71910000218a")))) ((Id (fromJust (UUID.fromString "00002874-0000-6190-0000-6b0200007f9d")))) (read "1864-04-10 15:43:57.866 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("SgjHf-i7Y8CVYxvU1oYj")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("3Nwip389Li7")))))}, conversationUri = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing})}))))
testObject_Event_user_5 :: Event
testObject_Event_user_5 = (Event (ConvDelete) ((Id (fromJust (UUID.fromString "00001b68-0000-3a94-0000-377c00006939")))) ((Id (fromJust (UUID.fromString "00007021-0000-2782-0000-45ff000077f7")))) (read "1864-06-06 00:15:54.849 UTC") (Nothing))
