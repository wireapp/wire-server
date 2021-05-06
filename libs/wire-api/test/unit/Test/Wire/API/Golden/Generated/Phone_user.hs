{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Phone_user where

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
testObject_Phone_user_1 :: Phone
testObject_Phone_user_1 = Phone {fromPhone = "+693505404"}
testObject_Phone_user_2 :: Phone
testObject_Phone_user_2 = Phone {fromPhone = "+6728831495"}
testObject_Phone_user_3 :: Phone
testObject_Phone_user_3 = Phone {fromPhone = "+24994398"}
testObject_Phone_user_4 :: Phone
testObject_Phone_user_4 = Phone {fromPhone = "+69248612921414"}
testObject_Phone_user_5 :: Phone
testObject_Phone_user_5 = Phone {fromPhone = "+77990010029"}
testObject_Phone_user_6 :: Phone
testObject_Phone_user_6 = Phone {fromPhone = "+941649838183"}
testObject_Phone_user_7 :: Phone
testObject_Phone_user_7 = Phone {fromPhone = "+26887959705114"}
testObject_Phone_user_8 :: Phone
testObject_Phone_user_8 = Phone {fromPhone = "+95719049585"}
testObject_Phone_user_9 :: Phone
testObject_Phone_user_9 = Phone {fromPhone = "+47102443311"}
testObject_Phone_user_10 :: Phone
testObject_Phone_user_10 = Phone {fromPhone = "+686977157662"}
testObject_Phone_user_11 :: Phone
testObject_Phone_user_11 = Phone {fromPhone = "+64841711838886"}
testObject_Phone_user_12 :: Phone
testObject_Phone_user_12 = Phone {fromPhone = "+3136560850603"}
testObject_Phone_user_13 :: Phone
testObject_Phone_user_13 = Phone {fromPhone = "+1025572905"}
testObject_Phone_user_14 :: Phone
testObject_Phone_user_14 = Phone {fromPhone = "+891953728"}
testObject_Phone_user_15 :: Phone
testObject_Phone_user_15 = Phone {fromPhone = "+206627685483"}
testObject_Phone_user_16 :: Phone
testObject_Phone_user_16 = Phone {fromPhone = "+072112068"}
testObject_Phone_user_17 :: Phone
testObject_Phone_user_17 = Phone {fromPhone = "+211531400245563"}
testObject_Phone_user_18 :: Phone
testObject_Phone_user_18 = Phone {fromPhone = "+95989792471"}
testObject_Phone_user_19 :: Phone
testObject_Phone_user_19 = Phone {fromPhone = "+54953312427"}
testObject_Phone_user_20 :: Phone
testObject_Phone_user_20 = Phone {fromPhone = "+29257902758392"}
