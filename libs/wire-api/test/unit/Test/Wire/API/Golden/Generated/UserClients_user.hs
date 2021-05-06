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
testObject_UserClients_user_1 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000300000007"))),fromList []),((Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000400000006"))),fromList [ClientId {client = "1"},ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000200000007"))),fromList [ClientId {client = "11"}]),((Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000300000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}])]}
testObject_UserClients_user_2 :: UserClients
testObject_UserClients_user_2 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000018-0000-0031-0000-003800000004"))),fromList [ClientId {client = "a08"}]),((Id (fromJust (UUID.fromString "00000035-0000-0037-0000-005500000075"))),fromList [ClientId {client = "e3b"}])]}
testObject_UserClients_user_3 :: UserClients
testObject_UserClients_user_3 = UserClients {userClients = fromList []}
testObject_UserClients_user_4 :: UserClients
testObject_UserClients_user_4 = UserClients {userClients = fromList []}
testObject_UserClients_user_5 :: UserClients
testObject_UserClients_user_5 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000400000008"))),fromList [ClientId {client = "10"}]),((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000500000006"))),fromList []),((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000700000004"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000800000002"))),fromList [ClientId {client = "d"}]),((Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000400000006"))),fromList [])]}
testObject_UserClients_user_6 :: UserClients
testObject_UserClients_user_6 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))),fromList [])]}
testObject_UserClients_user_7 :: UserClients
testObject_UserClients_user_7 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000600000014"))),fromList [ClientId {client = "41"}]),((Id (fromJust (UUID.fromString "00000016-0000-0018-0000-001e00000002"))),fromList [ClientId {client = "27"}]),((Id (fromJust (UUID.fromString "00000016-0000-0019-0000-000e00000003"))),fromList [ClientId {client = "0"},ClientId {client = "1"}])]}
testObject_UserClients_user_8 :: UserClients
testObject_UserClients_user_8 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000012-0000-0007-0000-000000000018"))),fromList [ClientId {client = "4"},ClientId {client = "d"}]),((Id (fromJust (UUID.fromString "00000016-0000-001e-0000-000e00000010"))),fromList [ClientId {client = "3"},ClientId {client = "5"}]),((Id (fromJust (UUID.fromString "00000018-0000-0018-0000-00170000001b"))),fromList [ClientId {client = "0"},ClientId {client = "1"}])]}
testObject_UserClients_user_9 :: UserClients
testObject_UserClients_user_9 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "0000004b-0000-0053-0000-007900000050"))),fromList []),((Id (fromJust (UUID.fromString "00000053-0000-0010-0000-001900000004"))),fromList [ClientId {client = "0"},ClientId {client = "1"},ClientId {client = "2"}])]}
testObject_UserClients_user_10 :: UserClients
testObject_UserClients_user_10 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000700000006"))),fromList [ClientId {client = "0"},ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000100000003"))),fromList [ClientId {client = "0"},ClientId {client = "3"}]),((Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000700000005"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000700000007"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000800000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}])]}
testObject_UserClients_user_11 :: UserClients
testObject_UserClients_user_11 = UserClients {userClients = fromList []}
testObject_UserClients_user_12 :: UserClients
testObject_UserClients_user_12 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),fromList [])]}
testObject_UserClients_user_13 :: UserClients
testObject_UserClients_user_13 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000000000003"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000000000006"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000200000005"))),fromList [ClientId {client = "9"}]),((Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000100000005"))),fromList [ClientId {client = "4"}])]}
testObject_UserClients_user_14 :: UserClients
testObject_UserClients_user_14 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000004"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000200000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))),fromList [ClientId {client = "0"},ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000300000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000100000001"))),fromList [ClientId {client = "7"}])]}
testObject_UserClients_user_15 :: UserClients
testObject_UserClients_user_15 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}])]}
testObject_UserClients_user_16 :: UserClients
testObject_UserClients_user_16 = UserClients {userClients = fromList []}
testObject_UserClients_user_17 :: UserClients
testObject_UserClients_user_17 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "0000005f-0000-0023-0000-00670000003a"))),fromList [ClientId {client = "9"},ClientId {client = "c"},ClientId {client = "f"}]),((Id (fromJust (UUID.fromString "0000006d-0000-003b-0000-004100000079"))),fromList [ClientId {client = "ee7"}])]}
testObject_UserClients_user_18 :: UserClients
testObject_UserClients_user_18 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000100000003"))),fromList [ClientId {client = "5"}]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000400000004"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000003"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000004"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000004"))),fromList []),((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000300000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}])]}
testObject_UserClients_user_19 :: UserClients
testObject_UserClients_user_19 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000300000003"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000200000002"))),fromList [ClientId {client = "6"}]),((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000000000000"))),fromList [ClientId {client = "0"},ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000000000004"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000000000000"))),fromList [ClientId {client = "0"}])]}
testObject_UserClients_user_20 :: UserClients
testObject_UserClients_user_20 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000006-0000-001f-0000-000b00000011"))),fromList []),((Id (fromJust (UUID.fromString "0000001c-0000-0001-0000-001600000003"))),fromList [ClientId {client = "2"},ClientId {client = "b"}]),((Id (fromJust (UUID.fromString "0000001c-0000-0013-0000-00030000000e"))),fromList [])]}
