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
testObject_UserClients_user_1 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "000054af-0000-746c-0000-52e100007422"))),fromList [ClientId {client = "2"},ClientId {client = "5"},ClientId {client = "6"},ClientId {client = "9"},ClientId {client = "d"},ClientId {client = "e"}])]}
testObject_UserClients_user_2 :: UserClients
testObject_UserClients_user_2 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000008"))),fromList [ClientId {client = "9"}]),((Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000200000008"))),fromList []),((Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000000000001"))),fromList [ClientId {client = "12"}]),((Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000700000007"))),fromList [])]}
testObject_UserClients_user_3 :: UserClients
testObject_UserClients_user_3 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "000031d0-0000-3104-0000-70f500007075"))),fromList [ClientId {client = "1"},ClientId {client = "14"},ClientId {client = "1a"},ClientId {client = "1c"}])]}
testObject_UserClients_user_4 :: UserClients
testObject_UserClients_user_4 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000100000005"))),fromList [ClientId {client = "0"},ClientId {client = "4"}]),((Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000200000004"))),fromList [ClientId {client = "5"}]),((Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000000000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000007-0000-0001-0000-000800000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"},ClientId {client = "2"}])]}
testObject_UserClients_user_5 :: UserClients
testObject_UserClients_user_5 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000004"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000400000005"))),fromList []),((Id (fromJust (UUID.fromString "00000007-0000-0001-0000-000400000001"))),fromList [ClientId {client = "0"},ClientId {client = "4"}]),((Id (fromJust (UUID.fromString "00000007-0000-0007-0000-000200000006"))),fromList [ClientId {client = "4"}])]}
testObject_UserClients_user_6 :: UserClients
testObject_UserClients_user_6 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000400000006"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000600000000"))),fromList []),((Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000600000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000600000005"))),fromList []),((Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000100000002"))),fromList [ClientId {client = "a"}])]}
testObject_UserClients_user_7 :: UserClients
testObject_UserClients_user_7 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000600000005"))),fromList [ClientId {client = "19"}]),((Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000400000001"))),fromList []),((Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000600000003"))),fromList [ClientId {client = "1"},ClientId {client = "3"}]),((Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000200000005"))),fromList [ClientId {client = "0"},ClientId {client = "1"}])]}
testObject_UserClients_user_8 :: UserClients
testObject_UserClients_user_8 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00007af5-0000-52a4-0000-169a00001f2c"))),fromList [ClientId {client = "607"},ClientId {client = "cc3"}])]}
testObject_UserClients_user_9 :: UserClients
testObject_UserClients_user_9 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000022-0000-0035-0000-005300000059"))),fromList [ClientId {client = "1"},ClientId {client = "14"}]),((Id (fromJust (UUID.fromString "0000004e-0000-0046-0000-002b00000063"))),fromList [ClientId {client = "0"},ClientId {client = "2"}])]}
testObject_UserClients_user_10 :: UserClients
testObject_UserClients_user_10 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "0000000f-0000-0059-0000-00460000004e"))),fromList [ClientId {client = "10"},ClientId {client = "2"},ClientId {client = "8"}]),((Id (fromJust (UUID.fromString "0000006f-0000-0051-0000-007d0000003f"))),fromList [ClientId {client = "0"},ClientId {client = "1"},ClientId {client = "2"}])]}
testObject_UserClients_user_11 :: UserClients
testObject_UserClients_user_11 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),fromList [ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))),fromList [ClientId {client = "0"}]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))),fromList [ClientId {client = "0"}])]}
testObject_UserClients_user_12 :: UserClients
testObject_UserClients_user_12 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000044-0000-000a-0000-006a0000000f"))),fromList [ClientId {client = "0"},ClientId {client = "1"},ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "0000007e-0000-0010-0000-003300000042"))),fromList [ClientId {client = "9"},ClientId {client = "b"},ClientId {client = "f"}])]}
testObject_UserClients_user_13 :: UserClients
testObject_UserClients_user_13 = UserClients {userClients = fromList []}
testObject_UserClients_user_14 :: UserClients
testObject_UserClients_user_14 = UserClients {userClients = fromList []}
testObject_UserClients_user_15 :: UserClients
testObject_UserClients_user_15 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000700000001"))),fromList [ClientId {client = "0"},ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000800000005"))),fromList [ClientId {client = "b"}]),((Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000100000006"))),fromList [ClientId {client = "2"},ClientId {client = "3"}]),((Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000200000007"))),fromList []),((Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000000000002"))),fromList [ClientId {client = "0"},ClientId {client = "1"}])]}
testObject_UserClients_user_16 :: UserClients
testObject_UserClients_user_16 = UserClients {userClients = fromList []}
testObject_UserClients_user_17 :: UserClients
testObject_UserClients_user_17 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),fromList [ClientId {client = "2"}]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))),fromList [ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))),fromList [ClientId {client = "0"},ClientId {client = "1"}])]}
testObject_UserClients_user_18 :: UserClients
testObject_UserClients_user_18 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "0000014f-0000-67d2-0000-1d4800005ccb"))),fromList [ClientId {client = "0"},ClientId {client = "1"},ClientId {client = "2"}])]}
testObject_UserClients_user_19 :: UserClients
testObject_UserClients_user_19 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000800000004"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000700000004"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000200000005"))),fromList [ClientId {client = "0"},ClientId {client = "1"}]),((Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000800000001"))),fromList [ClientId {client = "1a"}])]}
testObject_UserClients_user_20 :: UserClients
testObject_UserClients_user_20 = UserClients {userClients = fromList [((Id (fromJust (UUID.fromString "00003c5a-0000-1adf-0000-467a00001b03"))),fromList [ClientId {client = "10"},ClientId {client = "1c"},ClientId {client = "de"}])]}
