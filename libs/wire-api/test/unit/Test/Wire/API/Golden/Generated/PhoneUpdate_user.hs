{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PhoneUpdate_user where

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
testObject_PhoneUpdate_user_1 :: PhoneUpdate
testObject_PhoneUpdate_user_1 = PhoneUpdate {puPhone = Phone {fromPhone = "+938065066466"}}
testObject_PhoneUpdate_user_2 :: PhoneUpdate
testObject_PhoneUpdate_user_2 = PhoneUpdate {puPhone = Phone {fromPhone = "+891906186543"}}
testObject_PhoneUpdate_user_3 :: PhoneUpdate
testObject_PhoneUpdate_user_3 = PhoneUpdate {puPhone = Phone {fromPhone = "+190675170"}}
testObject_PhoneUpdate_user_4 :: PhoneUpdate
testObject_PhoneUpdate_user_4 = PhoneUpdate {puPhone = Phone {fromPhone = "+70556444318"}}
testObject_PhoneUpdate_user_5 :: PhoneUpdate
testObject_PhoneUpdate_user_5 = PhoneUpdate {puPhone = Phone {fromPhone = "+83455258493298"}}
testObject_PhoneUpdate_user_6 :: PhoneUpdate
testObject_PhoneUpdate_user_6 = PhoneUpdate {puPhone = Phone {fromPhone = "+063680616"}}
testObject_PhoneUpdate_user_7 :: PhoneUpdate
testObject_PhoneUpdate_user_7 = PhoneUpdate {puPhone = Phone {fromPhone = "+26498313767812"}}
testObject_PhoneUpdate_user_8 :: PhoneUpdate
testObject_PhoneUpdate_user_8 = PhoneUpdate {puPhone = Phone {fromPhone = "+136116928651"}}
testObject_PhoneUpdate_user_9 :: PhoneUpdate
testObject_PhoneUpdate_user_9 = PhoneUpdate {puPhone = Phone {fromPhone = "+46616206115680"}}
testObject_PhoneUpdate_user_10 :: PhoneUpdate
testObject_PhoneUpdate_user_10 = PhoneUpdate {puPhone = Phone {fromPhone = "+472000985161082"}}
testObject_PhoneUpdate_user_11 :: PhoneUpdate
testObject_PhoneUpdate_user_11 = PhoneUpdate {puPhone = Phone {fromPhone = "+78616072"}}
testObject_PhoneUpdate_user_12 :: PhoneUpdate
testObject_PhoneUpdate_user_12 = PhoneUpdate {puPhone = Phone {fromPhone = "+054197907324415"}}
testObject_PhoneUpdate_user_13 :: PhoneUpdate
testObject_PhoneUpdate_user_13 = PhoneUpdate {puPhone = Phone {fromPhone = "+71602716"}}
testObject_PhoneUpdate_user_14 :: PhoneUpdate
testObject_PhoneUpdate_user_14 = PhoneUpdate {puPhone = Phone {fromPhone = "+0250737056532"}}
testObject_PhoneUpdate_user_15 :: PhoneUpdate
testObject_PhoneUpdate_user_15 = PhoneUpdate {puPhone = Phone {fromPhone = "+330535581888691"}}
testObject_PhoneUpdate_user_16 :: PhoneUpdate
testObject_PhoneUpdate_user_16 = PhoneUpdate {puPhone = Phone {fromPhone = "+994241790288"}}
testObject_PhoneUpdate_user_17 :: PhoneUpdate
testObject_PhoneUpdate_user_17 = PhoneUpdate {puPhone = Phone {fromPhone = "+4903272851"}}
testObject_PhoneUpdate_user_18 :: PhoneUpdate
testObject_PhoneUpdate_user_18 = PhoneUpdate {puPhone = Phone {fromPhone = "+60523821"}}
testObject_PhoneUpdate_user_19 :: PhoneUpdate
testObject_PhoneUpdate_user_19 = PhoneUpdate {puPhone = Phone {fromPhone = "+24403906803308"}}
testObject_PhoneUpdate_user_20 :: PhoneUpdate
testObject_PhoneUpdate_user_20 = PhoneUpdate {puPhone = Phone {fromPhone = "+4482896668836"}}
