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
testObject_SimpleMember_user_1 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000004e-0000-002d-0000-002a0000000e"))), smConvRoleName = (fromJust (parseRoleName "87di9v1zk6fisj8snc5h23yts_7urnhuftg23rfdgwchj3yzqy9xq3sqee2p_xnmj00f_zfcs6rj0le_2b31"))}
testObject_SimpleMember_user_2 :: SimpleMember
testObject_SimpleMember_user_2 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000061-0000-0063-0000-001600000020"))), smConvRoleName = (fromJust (parseRoleName "7k8v3dat1d73530ap10_q12vve8sc44p919sjcqzkwyo48wpdjn1d1zy4bv9rriy_c5o9fn7gggd3sqj5qmew078u_yej1tldr0h2zz7tlfkggzvloa4myl10nwu"))}
testObject_SimpleMember_user_3 :: SimpleMember
testObject_SimpleMember_user_3 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000072-0000-0062-0000-000500000051"))), smConvRoleName = (fromJust (parseRoleName "34mdgvq_l8tvy92"))}
testObject_SimpleMember_user_4 :: SimpleMember
testObject_SimpleMember_user_4 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000062-0000-0030-0000-002f0000003b"))), smConvRoleName = (fromJust (parseRoleName "536mndyvw_u8bes7tzkq2ztb1dspcvh06ycsksjowcaudw139p3g_h1s8um62n2fof"))}
testObject_SimpleMember_user_5 :: SimpleMember
testObject_SimpleMember_user_5 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000015-0000-0003-0000-00410000007d"))), smConvRoleName = (fromJust (parseRoleName "7fs702c43x56o7azfz9smztwxt1fj81752jhwoqaee3cviwly4pe72cgn4a_9ug33htk6mrgn1ux1n2_7jfo40iq_k0teok2bocgv"))}
