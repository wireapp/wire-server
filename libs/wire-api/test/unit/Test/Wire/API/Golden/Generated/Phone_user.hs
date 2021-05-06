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
testObject_Phone_user_1 = Phone {fromPhone = "+6991463857542"}
testObject_Phone_user_2 :: Phone
testObject_Phone_user_2 = Phone {fromPhone = "+316311257"}
testObject_Phone_user_3 :: Phone
testObject_Phone_user_3 = Phone {fromPhone = "+02273583718609"}
testObject_Phone_user_4 :: Phone
testObject_Phone_user_4 = Phone {fromPhone = "+385761697872680"}
testObject_Phone_user_5 :: Phone
testObject_Phone_user_5 = Phone {fromPhone = "+0544544319"}
testObject_Phone_user_6 :: Phone
testObject_Phone_user_6 = Phone {fromPhone = "+873581280418"}
testObject_Phone_user_7 :: Phone
testObject_Phone_user_7 = Phone {fromPhone = "+74662092"}
testObject_Phone_user_8 :: Phone
testObject_Phone_user_8 = Phone {fromPhone = "+593553020956851"}
testObject_Phone_user_9 :: Phone
testObject_Phone_user_9 = Phone {fromPhone = "+737444654679"}
testObject_Phone_user_10 :: Phone
testObject_Phone_user_10 = Phone {fromPhone = "+9997230073"}
testObject_Phone_user_11 :: Phone
testObject_Phone_user_11 = Phone {fromPhone = "+47708323987440"}
testObject_Phone_user_12 :: Phone
testObject_Phone_user_12 = Phone {fromPhone = "+362476806"}
testObject_Phone_user_13 :: Phone
testObject_Phone_user_13 = Phone {fromPhone = "+662374971186"}
testObject_Phone_user_14 :: Phone
testObject_Phone_user_14 = Phone {fromPhone = "+94090766897466"}
testObject_Phone_user_15 :: Phone
testObject_Phone_user_15 = Phone {fromPhone = "+41165169037927"}
testObject_Phone_user_16 :: Phone
testObject_Phone_user_16 = Phone {fromPhone = "+388997537"}
testObject_Phone_user_17 :: Phone
testObject_Phone_user_17 = Phone {fromPhone = "+789951759"}
testObject_Phone_user_18 :: Phone
testObject_Phone_user_18 = Phone {fromPhone = "+8018415941060"}
testObject_Phone_user_19 :: Phone
testObject_Phone_user_19 = Phone {fromPhone = "+440646343775185"}
testObject_Phone_user_20 :: Phone
testObject_Phone_user_20 = Phone {fromPhone = "+86565027091"}
