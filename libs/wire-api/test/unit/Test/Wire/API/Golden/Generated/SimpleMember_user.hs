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
testObject_SimpleMember_1 :: SimpleMember
testObject_SimpleMember_1 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000002b-0000-0013-0000-000e00000028"))), smConvRoleName = (fromJust (parseRoleName "24ii5rxetk5ckgjwgmqsj81vyer6ecdpw9ln59gz05rdml9bthq2zynllc"))}
testObject_SimpleMember_2 :: SimpleMember
testObject_SimpleMember_2 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000032-0000-0002-0000-00030000005b"))), smConvRoleName = (fromJust (parseRoleName "i27apvv48c6s"))}
testObject_SimpleMember_3 :: SimpleMember
testObject_SimpleMember_3 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000001c-0000-0056-0000-004a0000004d"))), smConvRoleName = (fromJust (parseRoleName "u63t8si_dq0n6_jpoz262q6u2oiesa2y0_z5zmi9mt2fd22e3vgbma1eb47acy_699d7j9qpnsdpz425l01gsu6g2bajy3ajlpkg"))}
testObject_SimpleMember_4 :: SimpleMember
testObject_SimpleMember_4 = SimpleMember {smId = (Id (fromJust (UUID.fromString "00000007-0000-0016-0000-00130000000e"))), smConvRoleName = (fromJust (parseRoleName "0m2y5vfocr4npvxhdcqs2gohn1fis5_n4kpp8yjzpci62w9i6tye9jrbsl_y0e5z751coiwbjmjc8igjsqy"))}
testObject_SimpleMember_5 :: SimpleMember
testObject_SimpleMember_5 = SimpleMember {smId = (Id (fromJust (UUID.fromString "0000005e-0000-005e-0000-007e00000037"))), smConvRoleName = (fromJust (parseRoleName "4kvzuhy7i0sis7q1jdzow_aj7u29n3iplgtx15qwtlphcu"))}
