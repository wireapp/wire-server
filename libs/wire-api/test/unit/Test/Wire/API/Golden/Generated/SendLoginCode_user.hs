{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.SendLoginCode_user where

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
testObject_SendLoginCode_user_1 :: SendLoginCode
testObject_SendLoginCode_user_1 = SendLoginCode {lcPhone = Phone {fromPhone = "+903951993276"}, lcCall = False, lcForce = True}
testObject_SendLoginCode_user_2 :: SendLoginCode
testObject_SendLoginCode_user_2 = SendLoginCode {lcPhone = Phone {fromPhone = "+0963158622"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_3 :: SendLoginCode
testObject_SendLoginCode_user_3 = SendLoginCode {lcPhone = Phone {fromPhone = "+856967790874583"}, lcCall = True, lcForce = False}
testObject_SendLoginCode_user_4 :: SendLoginCode
testObject_SendLoginCode_user_4 = SendLoginCode {lcPhone = Phone {fromPhone = "+857448705132811"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_5 :: SendLoginCode
testObject_SendLoginCode_user_5 = SendLoginCode {lcPhone = Phone {fromPhone = "+262821458418"}, lcCall = True, lcForce = False}
testObject_SendLoginCode_user_6 :: SendLoginCode
testObject_SendLoginCode_user_6 = SendLoginCode {lcPhone = Phone {fromPhone = "+332020968297195"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_7 :: SendLoginCode
testObject_SendLoginCode_user_7 = SendLoginCode {lcPhone = Phone {fromPhone = "+89211689935"}, lcCall = True, lcForce = True}
testObject_SendLoginCode_user_8 :: SendLoginCode
testObject_SendLoginCode_user_8 = SendLoginCode {lcPhone = Phone {fromPhone = "+2072068108596"}, lcCall = True, lcForce = True}
testObject_SendLoginCode_user_9 :: SendLoginCode
testObject_SendLoginCode_user_9 = SendLoginCode {lcPhone = Phone {fromPhone = "+026344631574"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_10 :: SendLoginCode
testObject_SendLoginCode_user_10 = SendLoginCode {lcPhone = Phone {fromPhone = "+88299394"}, lcCall = True, lcForce = False}
testObject_SendLoginCode_user_11 :: SendLoginCode
testObject_SendLoginCode_user_11 = SendLoginCode {lcPhone = Phone {fromPhone = "+986470040810847"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_12 :: SendLoginCode
testObject_SendLoginCode_user_12 = SendLoginCode {lcPhone = Phone {fromPhone = "+1876764496657"}, lcCall = True, lcForce = True}
testObject_SendLoginCode_user_13 :: SendLoginCode
testObject_SendLoginCode_user_13 = SendLoginCode {lcPhone = Phone {fromPhone = "+903060676"}, lcCall = True, lcForce = False}
testObject_SendLoginCode_user_14 :: SendLoginCode
testObject_SendLoginCode_user_14 = SendLoginCode {lcPhone = Phone {fromPhone = "+053769728856"}, lcCall = False, lcForce = True}
testObject_SendLoginCode_user_15 :: SendLoginCode
testObject_SendLoginCode_user_15 = SendLoginCode {lcPhone = Phone {fromPhone = "+66695566135800"}, lcCall = False, lcForce = True}
testObject_SendLoginCode_user_16 :: SendLoginCode
testObject_SendLoginCode_user_16 = SendLoginCode {lcPhone = Phone {fromPhone = "+25814944842"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_17 :: SendLoginCode
testObject_SendLoginCode_user_17 = SendLoginCode {lcPhone = Phone {fromPhone = "+61301568"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_18 :: SendLoginCode
testObject_SendLoginCode_user_18 = SendLoginCode {lcPhone = Phone {fromPhone = "+8057484144069"}, lcCall = False, lcForce = True}
testObject_SendLoginCode_user_19 :: SendLoginCode
testObject_SendLoginCode_user_19 = SendLoginCode {lcPhone = Phone {fromPhone = "+47810744998590"}, lcCall = True, lcForce = False}
testObject_SendLoginCode_user_20 :: SendLoginCode
testObject_SendLoginCode_user_20 = SendLoginCode {lcPhone = Phone {fromPhone = "+43464730819305"}, lcCall = False, lcForce = True}
