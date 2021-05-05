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
testObject_OtherMember_1 :: OtherMember
testObject_OtherMember_1 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000019-0000-001a-0000-00070000000e"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000300000003"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002")))}), omConvRoleName = (fromJust (parseRoleName "6p95e04_iyvx8puu8k_m2cjv05220agruvq0ufvm6ni9mhz7y8l8hv_zbbciambf9x7kugxd5hwd7lhmkm2"))}
testObject_OtherMember_2 :: OtherMember
testObject_OtherMember_2 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000017-0000-001f-0000-000d00000005"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000003"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000003")))}), omConvRoleName = (fromJust (parseRoleName "k6lbz9aa64gxlw945be4cqic4f"))}
testObject_OtherMember_3 :: OtherMember
testObject_OtherMember_3 = OtherMember {omId = (Id (fromJust (UUID.fromString "0000001b-0000-0012-0000-001b00000005"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000000000000")))}), omConvRoleName = (fromJust (parseRoleName "mq8otytxda7kt6xt5o8sqyatudw8ugqfny7y1xqzv6jeo9zv3yuhmd7xz9as9wwhuw6jxzvg0n5gpc5fnb11gcecu_f4tq199qvpql0j"))}
testObject_OtherMember_4 :: OtherMember
testObject_OtherMember_4 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000006-0000-001b-0000-000600000019"))), omService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000200000004"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000004")))}), omConvRoleName = (fromJust (parseRoleName "gcla1s0savak_8p46mv0cu2si51hcbm2o4e9bhddwqw91u6112ipttnag0ycjbzc"))}
testObject_OtherMember_5 :: OtherMember
testObject_OtherMember_5 = OtherMember {omId = (Id (fromJust (UUID.fromString "00000008-0000-000b-0000-000400000015"))), omService = Nothing, omConvRoleName = (fromJust (parseRoleName "tnn0uwcrdmlqc8_yyy4ovwxjbl_afn0"))}
