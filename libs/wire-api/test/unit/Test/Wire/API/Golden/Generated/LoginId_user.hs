{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LoginId_user where

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
testObject_LoginId_user_1 :: LoginId
testObject_LoginId_user_1 = LoginByEmail (Email {emailLocal = "7\GSz\"IQ\989961\996189\DC2\1080499\r\rT\DC3\EOT\1044669\NAK\SOHgRTL", emailDomain = "o?fx\ESC!\186466\&1r\997835\&7,\5001,"})
testObject_LoginId_user_2 :: LoginId
testObject_LoginId_user_2 = LoginByHandle (Handle {fromHandle = "jku"})
testObject_LoginId_user_3 :: LoginId
testObject_LoginId_user_3 = LoginByEmail (Email {emailLocal = "l", emailDomain = "]\8004\19583\173165.`\1113939\DLE\1101424\GSF'0sP\1106477"})
testObject_LoginId_user_4 :: LoginId
testObject_LoginId_user_4 = LoginByHandle (Handle {fromHandle = "t2"})
testObject_LoginId_user_5 :: LoginId
testObject_LoginId_user_5 = LoginByHandle (Handle {fromHandle = "428bl27p2kxi2a5k-u6i8mhxthh48lny87cblmzq3bk-wnaa0re6xbjx4fpzhpv_gt9mp7y5mczik8g-8f9dpl49m.6_-rviy679y8-1dm2cr7w_jm1za-edtzt0l0ll0_vmwue4u0mgem1qplng3tgc_nutd0w-bw72lvz4ip_4ge_u91of5n_or4izh0md"})
