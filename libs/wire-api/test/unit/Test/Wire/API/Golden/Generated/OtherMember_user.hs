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
testObject_OtherMember_user_1 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000007-0000-001d-0000-00130000001b"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000400000003"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000300000004")))}), omConvRoleName = (fromJust (parseRoleName "c6uzgl65a2rf0nl75nxau0a5pgla4n0dze56zh8vrgudkn8crb7imo_3y_otrtc2csl_6xlrb8byxd5xj80qf09syrew"))}
testObject_OtherMember_user_2 :: OtherMember
testObject_OtherMember_user_2 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001a-0000-0011-0000-00120000000e"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002")))}), omConvRoleName = (fromJust (parseRoleName "7g2l5fb7uhbzvcglb3qhpx5w58qw1i1vz2p42wcw810rmwt8mc5o0e9s2h614k_bixrsxsjjb9fnem5cbpz8ow_d3g3x"))}
testObject_OtherMember_user_3 :: OtherMember
testObject_OtherMember_user_3 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000000000019"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "pgtvlmu09eri"))}
testObject_OtherMember_user_4 :: OtherMember
testObject_OtherMember_user_4 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000016-0000-0012-0000-001c00000009"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000300000001")))}), omConvRoleName = (fromJust (parseRoleName "43t3tg6b_u0bhfxah_pr_muzwg2n_zssych1biss_fb_8q1exjobnkq_8ijap9fymrwr_hbvrnqu_w_4hwc0owfkkl2024fq66k7"))}
testObject_OtherMember_user_5 :: OtherMember
testObject_OtherMember_user_5 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000017-0000-000b-0000-00190000001d"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000300000000")))}), omConvRoleName = (fromJust (parseRoleName "vg6vkbw6tx58fhxqee6ryg1awdkmovycy93be_d6vbsu_4gpdccajtzls4ikttcb3c4jm87rcqa"))}
