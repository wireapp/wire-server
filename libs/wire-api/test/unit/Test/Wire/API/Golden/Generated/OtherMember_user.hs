{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.OtherMember_user where

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
testObject_OtherMember_user_1 :: OtherMember
testObject_OtherMember_user_1 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001a-0000-000d-0000-00150000000b"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000003"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000003")))}), omConvRoleName = (fromJust (parseRoleName "c86ym4todwhkorjfr3j4klm5k0lz0t_iacb5zhlgft14lkamhnqx6he3uzhdv6d5qc66l9orfw6k77adgzlj_c06cud"))}
testObject_OtherMember_user_2 :: OtherMember
testObject_OtherMember_user_2 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001e-0000-001e-0000-001d00000010"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "ctvklslruedk4x_b0uhewwprfuo4x6ptfwdtj3ykvskhfbysfgh9dykkftv0xnuc8wfi9ahufvlb8m_2vm238iqbcn46kfk76c6_"))}
testObject_OtherMember_user_3 :: OtherMember
testObject_OtherMember_user_3 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000000-0000-0015-0000-000400000007"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000300000003"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000400000002")))}), omConvRoleName = (fromJust (parseRoleName "w95tbvn2ie7"))}
testObject_OtherMember_user_4 :: OtherMember
testObject_OtherMember_user_4 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000010-0000-001f-0000-000800000009"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000300000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000003")))}), omConvRoleName = (fromJust (parseRoleName "h4t0uzfrsdl4d_o0c60goeu_32zk4dos9bj7_7n_8hp3bogimk1851sye6b6d03ct3cvftlm5jay3cvrh9yji37fv4i3gd9h_62udwd0x15jvqs71h"))}
testObject_OtherMember_user_5 :: OtherMember
testObject_OtherMember_user_5 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000005-0000-0009-0000-000e00000008"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000000")))}), omConvRoleName = (fromJust (parseRoleName "z1m"))}
