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
testObject_SendLoginCode_user_1 = SendLoginCode {lcPhone = Phone {fromPhone = "+948589913076"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_2 :: SendLoginCode
testObject_SendLoginCode_user_2 = SendLoginCode {lcPhone = Phone {fromPhone = "+03321825654"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_3 :: SendLoginCode
testObject_SendLoginCode_user_3 = SendLoginCode {lcPhone = Phone {fromPhone = "+382689569353434"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_4 :: SendLoginCode
testObject_SendLoginCode_user_4 = SendLoginCode {lcPhone = Phone {fromPhone = "+642788161589"}, lcCall = True, lcForce = True}
testObject_SendLoginCode_user_5 :: SendLoginCode
testObject_SendLoginCode_user_5 = SendLoginCode {lcPhone = Phone {fromPhone = "+39921582"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_6 :: SendLoginCode
testObject_SendLoginCode_user_6 = SendLoginCode {lcPhone = Phone {fromPhone = "+34520439170316"}, lcCall = True, lcForce = False}
testObject_SendLoginCode_user_7 :: SendLoginCode
testObject_SendLoginCode_user_7 = SendLoginCode {lcPhone = Phone {fromPhone = "+49609857620830"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_8 :: SendLoginCode
testObject_SendLoginCode_user_8 = SendLoginCode {lcPhone = Phone {fromPhone = "+7578227534737"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_9 :: SendLoginCode
testObject_SendLoginCode_user_9 = SendLoginCode {lcPhone = Phone {fromPhone = "+45866459847146"}, lcCall = True, lcForce = True}
testObject_SendLoginCode_user_10 :: SendLoginCode
testObject_SendLoginCode_user_10 = SendLoginCode {lcPhone = Phone {fromPhone = "+81558820822"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_11 :: SendLoginCode
testObject_SendLoginCode_user_11 = SendLoginCode {lcPhone = Phone {fromPhone = "+39799434602303"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_12 :: SendLoginCode
testObject_SendLoginCode_user_12 = SendLoginCode {lcPhone = Phone {fromPhone = "+076875665529"}, lcCall = False, lcForce = True}
testObject_SendLoginCode_user_13 :: SendLoginCode
testObject_SendLoginCode_user_13 = SendLoginCode {lcPhone = Phone {fromPhone = "+553524310655"}, lcCall = True, lcForce = True}
testObject_SendLoginCode_user_14 :: SendLoginCode
testObject_SendLoginCode_user_14 = SendLoginCode {lcPhone = Phone {fromPhone = "+2970690699"}, lcCall = True, lcForce = False}
testObject_SendLoginCode_user_15 :: SendLoginCode
testObject_SendLoginCode_user_15 = SendLoginCode {lcPhone = Phone {fromPhone = "+673861428777"}, lcCall = False, lcForce = True}
testObject_SendLoginCode_user_16 :: SendLoginCode
testObject_SendLoginCode_user_16 = SendLoginCode {lcPhone = Phone {fromPhone = "+02943144"}, lcCall = True, lcForce = True}
testObject_SendLoginCode_user_17 :: SendLoginCode
testObject_SendLoginCode_user_17 = SendLoginCode {lcPhone = Phone {fromPhone = "+37358374239963"}, lcCall = True, lcForce = False}
testObject_SendLoginCode_user_18 :: SendLoginCode
testObject_SendLoginCode_user_18 = SendLoginCode {lcPhone = Phone {fromPhone = "+7770371040"}, lcCall = False, lcForce = True}
testObject_SendLoginCode_user_19 :: SendLoginCode
testObject_SendLoginCode_user_19 = SendLoginCode {lcPhone = Phone {fromPhone = "+663025070"}, lcCall = False, lcForce = False}
testObject_SendLoginCode_user_20 :: SendLoginCode
testObject_SendLoginCode_user_20 = SendLoginCode {lcPhone = Phone {fromPhone = "+916619907490501"}, lcCall = True, lcForce = False}
