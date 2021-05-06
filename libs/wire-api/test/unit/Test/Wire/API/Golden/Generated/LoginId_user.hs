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
testObject_LoginId_user_1 = LoginByPhone (Phone {fromPhone = "+2927860431994"})
testObject_LoginId_user_2 :: LoginId
testObject_LoginId_user_2 = LoginByEmail (Email {emailLocal = "?7\35807L2\1078776r\40526J(1H\161068\1086268Eo\1049131#3\DELr\1018936", emailDomain = "\190041\&2\182254\b\1041750q\1098235\25404)"})
testObject_LoginId_user_3 :: LoginId
testObject_LoginId_user_3 = LoginByHandle (Handle {fromHandle = "9u"})
testObject_LoginId_user_4 :: LoginId
testObject_LoginId_user_4 = LoginByEmail (Email {emailLocal = "Dp-\DEL`#\145050\41578#\DLEW\f \1112382Y9\173057\v{\SYN\EOT(", emailDomain = "\1024287\&3\US:F\RS#\CANkw\FS/a\149472\nn5J\171711\1074021Sk!\1085456\&8H2P"})
testObject_LoginId_user_5 :: LoginId
testObject_LoginId_user_5 = LoginByEmail (Email {emailLocal = "mnfk?h\63307#\\n\FS,\1048851'scr#\v\ACK\36852\1007294BE\1050929\1073640V3", emailDomain = "\990941\&0\1017585\141400"})
