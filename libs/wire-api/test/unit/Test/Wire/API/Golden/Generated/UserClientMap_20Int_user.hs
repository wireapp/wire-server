{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserClientMap_20Int_user where

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
testObject_UserClientMap_20Int_user_1 :: UserClientMap Int
testObject_UserClientMap_20Int_user_1 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000002-0000-0007-0000-000500000008"))),fromList [(ClientId {client = "2"},1),(ClientId {client = "4"},-1)]),((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000800000007"))),fromList []),((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000200000006"))),fromList [(ClientId {client = "0"},1),(ClientId {client = "1"},-1)]),((Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000300000005"))),fromList [(ClientId {client = "2"},-2)])]}
testObject_UserClientMap_20Int_user_2 :: UserClientMap Int
testObject_UserClientMap_20Int_user_2 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))),fromList [(ClientId {client = "2"},-3)]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),fromList [(ClientId {client = "4"},-2)]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))),fromList [(ClientId {client = "0"},0),(ClientId {client = "1"},0)]),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))),fromList [(ClientId {client = "0"},-1)]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))),fromList [(ClientId {client = "0"},-1)]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))),fromList [(ClientId {client = "0"},-1),(ClientId {client = "1"},0)])]}
testObject_UserClientMap_20Int_user_3 :: UserClientMap Int
testObject_UserClientMap_20Int_user_3 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000022-0000-006d-0000-000800000036"))),fromList []),((Id (fromJust (UUID.fromString "0000003f-0000-0005-0000-005d0000001e"))),fromList [])]}
testObject_UserClientMap_20Int_user_4 :: UserClientMap Int
testObject_UserClientMap_20Int_user_4 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-001f-0000-001b0000001e"))),fromList [(ClientId {client = "0"},-1),(ClientId {client = "2"},0)]),((Id (fromJust (UUID.fromString "0000000a-0000-0015-0000-00180000001f"))),fromList [(ClientId {client = "0"},-2),(ClientId {client = "1"},0),(ClientId {client = "2"},2)]),((Id (fromJust (UUID.fromString "0000001d-0000-0008-0000-000a00000016"))),fromList [(ClientId {client = "0"},1),(ClientId {client = "1"},1),(ClientId {client = "2"},0)])]}
testObject_UserClientMap_20Int_user_5 :: UserClientMap Int
testObject_UserClientMap_20Int_user_5 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000300000001"))),fromList [(ClientId {client = "5"},-5)]),((Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000500000001"))),fromList [(ClientId {client = "1b"},7)]),((Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000100000007"))),fromList [(ClientId {client = "0"},-1),(ClientId {client = "1"},0)]),((Id (fromJust (UUID.fromString "00000008-0000-0007-0000-000600000005"))),fromList [(ClientId {client = "0"},-2),(ClientId {client = "1"},1)])]}
testObject_UserClientMap_20Int_user_6 :: UserClientMap Int
testObject_UserClientMap_20Int_user_6 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "0000001d-0000-0027-0000-002b0000000d"))),fromList [(ClientId {client = "1"},-1),(ClientId {client = "2"},0)]),((Id (fromJust (UUID.fromString "0000006b-0000-002e-0000-003c0000006a"))),fromList [(ClientId {client = "1"},-3),(ClientId {client = "2"},1),(ClientId {client = "3"},-3),(ClientId {client = "4"},2)])]}
testObject_UserClientMap_20Int_user_7 :: UserClientMap Int
testObject_UserClientMap_20Int_user_7 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000300000002"))),fromList [(ClientId {client = "0"},2),(ClientId {client = "2"},-1)]),((Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000400000003"))),fromList [(ClientId {client = "0"},0),(ClientId {client = "1"},0)]),((Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000600000008"))),fromList [(ClientId {client = "0"},0),(ClientId {client = "1"},2)]),((Id (fromJust (UUID.fromString "00000007-0000-0006-0000-000200000004"))),fromList [(ClientId {client = "0"},0),(ClientId {client = "1"},0)]),((Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000500000002"))),fromList [(ClientId {client = "0"},-1)])]}
testObject_UserClientMap_20Int_user_8 :: UserClientMap Int
testObject_UserClientMap_20Int_user_8 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "0000000f-0000-0012-0000-001500000013"))),fromList []),((Id (fromJust (UUID.fromString "00000018-0000-000c-0000-000900000020"))),fromList [(ClientId {client = "58"},10)]),((Id (fromJust (UUID.fromString "00000018-0000-0019-0000-001400000019"))),fromList [(ClientId {client = "1"},-1),(ClientId {client = "2"},2)])]}
testObject_UserClientMap_20Int_user_9 :: UserClientMap Int
testObject_UserClientMap_20Int_user_9 = UserClientMap {userClientMap = fromList []}
testObject_UserClientMap_20Int_user_10 :: UserClientMap Int
testObject_UserClientMap_20Int_user_10 = UserClientMap {userClientMap = fromList []}
testObject_UserClientMap_20Int_user_11 :: UserClientMap Int
testObject_UserClientMap_20Int_user_11 = UserClientMap {userClientMap = fromList []}
testObject_UserClientMap_20Int_user_12 :: UserClientMap Int
testObject_UserClientMap_20Int_user_12 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000800000007"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000000000007"))),fromList [(ClientId {client = "b"},-5)]),((Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000500000007"))),fromList []),((Id (fromJust (UUID.fromString "00000007-0000-0006-0000-000800000002"))),fromList [(ClientId {client = "19"},4)])]}
testObject_UserClientMap_20Int_user_13 :: UserClientMap Int
testObject_UserClientMap_20Int_user_13 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "000019b5-0000-50e0-0000-467200004475"))),fromList [(ClientId {client = "0"},0),(ClientId {client = "1"},1)])]}
testObject_UserClientMap_20Int_user_14 :: UserClientMap Int
testObject_UserClientMap_20Int_user_14 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "0000469b-0000-333e-0000-3dd8000010c5"))),fromList [(ClientId {client = "3c1"},4),(ClientId {client = "752"},-4)])]}
testObject_UserClientMap_20Int_user_15 :: UserClientMap Int
testObject_UserClientMap_20Int_user_15 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000003-0000-0016-0000-002000000006"))),fromList [(ClientId {client = "2f"},-4)]),((Id (fromJust (UUID.fromString "00000010-0000-0015-0000-001100000017"))),fromList [(ClientId {client = "3"},-1),(ClientId {client = "b"},-3)]),((Id (fromJust (UUID.fromString "0000001d-0000-0006-0000-000200000012"))),fromList [(ClientId {client = "1"},-2),(ClientId {client = "3"},2),(ClientId {client = "4"},-3)])]}
testObject_UserClientMap_20Int_user_16 :: UserClientMap Int
testObject_UserClientMap_20Int_user_16 = UserClientMap {userClientMap = fromList []}
testObject_UserClientMap_20Int_user_17 :: UserClientMap Int
testObject_UserClientMap_20Int_user_17 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "000067cb-0000-7498-0000-1f0000000e39"))),fromList [(ClientId {client = "0"},3),(ClientId {client = "6"},2),(ClientId {client = "7"},1),(ClientId {client = "8"},-2)])]}
testObject_UserClientMap_20Int_user_18 :: UserClientMap Int
testObject_UserClientMap_20Int_user_18 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000400000004"))),fromList [(ClientId {client = "0"},1)]),((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000004"))),fromList [(ClientId {client = "0"},0),(ClientId {client = "1"},0)]),((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000003"))),fromList []),((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000200000000"))),fromList [(ClientId {client = "0"},-2)]),((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000300000001"))),fromList []),((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000000000001"))),fromList [])]}
testObject_UserClientMap_20Int_user_19 :: UserClientMap Int
testObject_UserClientMap_20Int_user_19 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000020-0000-0049-0000-004200000037"))),fromList [(ClientId {client = "19"},-3),(ClientId {client = "7"},3)]),((Id (fromJust (UUID.fromString "00000023-0000-0035-0000-004300000043"))),fromList [(ClientId {client = "0"},0),(ClientId {client = "1"},0)])]}
testObject_UserClientMap_20Int_user_20 :: UserClientMap Int
testObject_UserClientMap_20Int_user_20 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0014-0000-00200000000b"))),fromList [(ClientId {client = "0"},-2),(ClientId {client = "1"},-2),(ClientId {client = "2"},0)]),((Id (fromJust (UUID.fromString "00000009-0000-0010-0000-00200000001a"))),fromList []),((Id (fromJust (UUID.fromString "00000011-0000-001b-0000-00000000001f"))),fromList [(ClientId {client = "9"},-1),(ClientId {client = "e"},-1)])]}
