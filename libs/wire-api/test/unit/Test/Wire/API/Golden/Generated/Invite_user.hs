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
testObject_Invite_user_1 :: Invite
testObject_Invite_user_1 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000034-0000-003a-0000-007b0000006e")))])), invRoleName = (fromJust (parseRoleName "j6rro"))}
testObject_Invite_user_2 :: Invite
testObject_Invite_user_2 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "0000007c-0000-0032-0000-007600000043"))),(Id (fromJust (UUID.fromString "00000052-0000-006a-0000-006c00000027")))])), invRoleName = (fromJust (parseRoleName "oxyojmh5ikcx6"))}
testObject_Invite_user_3 :: Invite
testObject_Invite_user_3 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000000-0000-0080-0000-004200000030"))),(Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000300000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000002")))])), invRoleName = (fromJust (parseRoleName "ynvkbx4aq5jqvckgsb8zy5nn8knqkjgpylyvq14q9er8k46ncdjtsdeq2rh0rgqh5nez5stlist1cbfrx6ilp6fzbwb"))}
testObject_Invite_user_4 :: Invite
testObject_Invite_user_4 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000032-0000-0036-0000-006300000074"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))])), invRoleName = (fromJust (parseRoleName "mc0vvv03k9mhwe5uclb53kxdg_8k7azlopo1mge68wbz84pbh8e4mt70fc_eeo96vcxf8imkjl340u4bcp3q7i5a7_dk_rujwfe3mnuv"))}
testObject_Invite_user_5 :: Invite
testObject_Invite_user_5 = Invite {invUsers = (List1 (NonEmpty.fromList [(Id (fromJust (UUID.fromString "00000077-0000-001c-0000-00670000003b"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))])), invRoleName = (fromJust (parseRoleName "cib24urhv587j6zv4ilb61v64s47l89jl2pr_dm9ojtz61_3w9r6l23"))}
