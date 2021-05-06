{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.SimpleMember_user where

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
testObject_SimpleMember_user_1 :: SimpleMember
testObject_SimpleMember_user_1 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000059-0000-006b-0000-00480000006c"))), smConvRoleName = (fromJust (parseRoleName "wqva74jpdq3udait2es8bt_mq9hyg3thezqorzvgy4wzimlzdxqtyp2plr0n7lpwtp4e0krplvn4py5c5hkrzq2am14"))}
testObject_SimpleMember_user_2 :: SimpleMember
testObject_SimpleMember_user_2 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000012-0000-0076-0000-002f00000019"))), smConvRoleName = (fromJust (parseRoleName "q7zpln2hs5ctzd8"))}
testObject_SimpleMember_user_3 :: SimpleMember
testObject_SimpleMember_user_3 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000004c-0000-007b-0000-00650000001f"))), smConvRoleName = (fromJust (parseRoleName "xvokrb2tndh649ecjodpgamksifwmuk5stjjghfeu70vk_yxrc7q01zat383helhhi5asloj0udejk1zgpdh43oavvrxpylcoiaanpb812a0ef6g9aobx"))}
testObject_SimpleMember_user_4 :: SimpleMember
testObject_SimpleMember_user_4 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000046-0000-0019-0000-003800000013"))), smConvRoleName = (fromJust (parseRoleName "gxrmjjmr46ezvgehze2fcgox7q4_qcdz6gf53iv68p7wa1411h2oubjfphqg_fn5hv8_sb4jmqd9bltiptps5t97"))}
testObject_SimpleMember_user_5 :: SimpleMember
testObject_SimpleMember_user_5 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000006f-0000-0062-0000-007200000010"))), smConvRoleName = (fromJust (parseRoleName "r1gp"))}
