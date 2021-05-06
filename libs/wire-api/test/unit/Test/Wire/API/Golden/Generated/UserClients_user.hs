{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserClients_user where

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
testObject_UserClients_user_1 :: UserClients
testObject_UserClients_user_1 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00003b97-0000-37bf-0000-7e3100002194"))),fromList [ClientId {client = "0"},ClientId {client = "1"},ClientId {client = "2"}])]}
testObject_UserClients_user_2 :: UserClients
testObject_UserClients_user_2 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00005ac2-0000-3e70-0000-35b3000032fd"))),fromList [ClientId {client = "32"},ClientId {client = "d4"},ClientId {client = "dd"}])]}
testObject_UserClients_user_3 :: UserClients
testObject_UserClients_user_3 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),fromList [ClientId {client = "1"}])]}
testObject_UserClients_user_4 :: UserClients
testObject_UserClients_user_4 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000300000001"))),fromList [ClientId {client = "4"}]),((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000400000000"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000000000000"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000001"))),fromList []),((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000400000004"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000000000004"))),fromList []),((Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000200000004"))),fromList [ClientId {client = "2"}])]}
testObject_UserClients_user_5 :: UserClients
testObject_UserClients_user_5 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "0000003c-0000-0029-0000-00290000007a"))),fromList [ClientId {client = "2"},ClientId {client = "4"}]),((Id (fromJust (UUID.fromString "00000063-0000-0027-0000-002500000080"))),fromList [ClientId {client = "14"},ClientId {client = "1e"}])]}
testObject_UserClients_user_6 :: UserClients
testObject_UserClients_user_6 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))),fromList [ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}])]}
testObject_UserClients_user_7 :: UserClients
testObject_UserClients_user_7 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))),fromList [])]}
testObject_UserClients_user_8 :: UserClients
testObject_UserClients_user_8 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0007-0000-001300000007"))),fromList [ClientId {client = "0"},ClientId {client = "3"}]),((Id (fromJust (UUID.fromString "00000005-0000-0010-0000-001600000009"))),fromList []),((Id (fromJust (UUID.fromString "0000001d-0000-000c-0000-001f00000008"))),fromList [ClientId {client = "1"},ClientId {client = "3"},ClientId {client = "4"}])]}
testObject_UserClients_user_9 :: UserClients
testObject_UserClients_user_9 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),fromList [])]}
testObject_UserClients_user_10 :: UserClients
testObject_UserClients_user_10 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "0000000d-0000-0005-0000-001100000016"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000012-0000-0006-0000-000d00000012"))),fromList [ClientId {client = "7"},ClientId {client = "f"}]),((Id (fromJust (UUID.fromString "00000019-0000-001e-0000-001c00000017"))),fromList [])]}
testObject_UserClients_user_11 :: UserClients
testObject_UserClients_user_11 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000100000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000100000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000300000008"))),fromList [ClientId {client = "d"}]),((Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000800000001"))),fromList [ClientId {client = "1f"}])]}
testObject_UserClients_user_12 :: UserClients
testObject_UserClients_user_12 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000005-0000-0010-0000-000a0000001f"))),fromList [ClientId {client = "10"},ClientId {client = "e"}]),((Id (fromJust (UUID.fromString "0000000c-0000-0006-0000-001900000014"))),fromList [ClientId {client = "0"},ClientId {client = "d"}]),((Id (fromJust (UUID.fromString "00000017-0000-001f-0000-001b00000011"))),fromList [])]}
testObject_UserClients_user_13 :: UserClients
testObject_UserClients_user_13 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000100000004"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000200000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000300000003"))),fromList [ClientId {client = "1"},ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000002"))),fromList []),((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000004"))),fromList [])]}
testObject_UserClients_user_14 :: UserClients
testObject_UserClients_user_14 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00003a5a-0000-36b8-0000-6094000055a4"))),fromList [ClientId {client = "0"},ClientId {client = "1"},ClientId {client = "2"}])]}
testObject_UserClients_user_15 :: UserClients
testObject_UserClients_user_15 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000005"))),fromList []),((Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000500000005"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000400000003"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000100000007"))),fromList [])]}
testObject_UserClients_user_16 :: UserClients
testObject_UserClients_user_16 = UserClients {userClients = fromList []}
testObject_UserClients_user_17 :: UserClients
testObject_UserClients_user_17 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000500000007"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000400000007"))),fromList [ClientId {client = "b"}]),((Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000000000001"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000600000002"))),fromList [])]}
testObject_UserClients_user_18 :: UserClients
testObject_UserClients_user_18 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0007-0000-000500000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000600000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000200000000"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000300000001"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000600000004"))),fromList [ClientId {client = "0"},ClientId {client = "1"}])]}
testObject_UserClients_user_19 :: UserClients
testObject_UserClients_user_19 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000400000001"))),fromList []),((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000002"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000001"))),fromList [ClientId {client = "0"},ClientId {client = "1"}])]}
testObject_UserClients_user_20 :: UserClients
testObject_UserClients_user_20 = UserClients {userClients = fromList []}
