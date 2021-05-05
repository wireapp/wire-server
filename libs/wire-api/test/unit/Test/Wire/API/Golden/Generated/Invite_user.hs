{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Invite_user where

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
testObject_Invite_1 :: Invite
testObject_Invite_1 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000062-0000-0016-0000-00100000004b")))])), invRoleName = (fromJust (parseRoleName "2i7w_splssi8qt4ee635qnxlqk2qrezwkshh3otwxtt_11nwmjiygblpk12z75ev8"))}
testObject_Invite_2 :: Invite
testObject_Invite_2 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000007-0000-0013-0000-001800000053")))])), invRoleName = (fromJust (parseRoleName "5xrrr0aye_w7uyb3p36i90nxklyhxu"))}
testObject_Invite_3 :: Invite
testObject_Invite_3 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000001f-0000-004c-0000-004e0000002a"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))])), invRoleName = (fromJust (parseRoleName "s6on"))}
testObject_Invite_4 :: Invite
testObject_Invite_4 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000002c-0000-0021-0000-00590000000e"))),(Id (fromJust (UUID.fromString "00000024-0000-0022-0000-005100000015")))])), invRoleName = (fromJust (parseRoleName "hm5y2lna9cw5ya_z_oidjoc0tjkxcmqy_kbuvdzlf7zi9n8kz4eir6taed1gq5slrwq5xts9cqe8t2ohr2hwlz5nqkk80las4k21_92upvs7c"))}
testObject_Invite_5 :: Invite
testObject_Invite_5 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000062-0000-0080-0000-004300000042"))),(Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))),(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002")))])), invRoleName = (fromJust (parseRoleName "2ej8ei3qhn0eq4gqm5_y6av8a0eg7w69a78q1za374sfrrhygfwatlu_6yap2aia301cn6b185fkq9d8q10vxbptttjb5wj9190lo"))}
