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
testObject_UserClientMap_20Int_user_1 = UserClientMap {userClientMap = fromList []}
testObject_UserClientMap_20Int_user_2 :: UserClientMap Int
testObject_UserClientMap_20Int_user_2 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),fromList [(ClientId {client = "0"},-3)]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))),fromList [(ClientId {client = "3"},-1)]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))),fromList [(ClientId {client = "4"},3)]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))),fromList [(ClientId {client = "1"},-2)]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))),fromList [])]}
testObject_UserClientMap_20Int_user_3 :: UserClientMap Int
testObject_UserClientMap_20Int_user_3 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000008"))),fromList []),((Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000400000001"))),fromList []),((Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000600000001"))),fromList []),((Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000600000008"))),fromList [(ClientId {client = "0"},0),(ClientId {client = "1"},0)])]}
testObject_UserClientMap_20Int_user_4 :: UserClientMap Int
testObject_UserClientMap_20Int_user_4 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),fromList [(ClientId {client = "1"},1)]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))),fromList [(ClientId {client = "0"},0),(ClientId {client = "1"},0)]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))),fromList [(ClientId {client = "0"},-1),(ClientId {client = "1"},1)]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))),fromList [(ClientId {client = "0"},3)]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))),fromList [])]}
testObject_UserClientMap_20Int_user_5 :: UserClientMap Int
testObject_UserClientMap_20Int_user_5 = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00001db0-0000-37cc-0000-0d8900004c9a"))),fromList [(ClientId {client = "8"},-1),(ClientId {client = "a"},4),(ClientId {client = "b"},3),(ClientId {client = "d"},0),(ClientId {client = "e"},4)])]}
