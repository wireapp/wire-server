{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.CookieId_user where

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
testObject_CookieId_user_1 :: CookieId
testObject_CookieId_user_1 = CookieId {cookieIdNum = 10461}
testObject_CookieId_user_2 :: CookieId
testObject_CookieId_user_2 = CookieId {cookieIdNum = 30259}
testObject_CookieId_user_3 :: CookieId
testObject_CookieId_user_3 = CookieId {cookieIdNum = 28194}
testObject_CookieId_user_4 :: CookieId
testObject_CookieId_user_4 = CookieId {cookieIdNum = 31849}
testObject_CookieId_user_5 :: CookieId
testObject_CookieId_user_5 = CookieId {cookieIdNum = 17298}
testObject_CookieId_user_6 :: CookieId
testObject_CookieId_user_6 = CookieId {cookieIdNum = 10943}
testObject_CookieId_user_7 :: CookieId
testObject_CookieId_user_7 = CookieId {cookieIdNum = 17481}
testObject_CookieId_user_8 :: CookieId
testObject_CookieId_user_8 = CookieId {cookieIdNum = 17152}
testObject_CookieId_user_9 :: CookieId
testObject_CookieId_user_9 = CookieId {cookieIdNum = 17580}
testObject_CookieId_user_10 :: CookieId
testObject_CookieId_user_10 = CookieId {cookieIdNum = 28107}
testObject_CookieId_user_11 :: CookieId
testObject_CookieId_user_11 = CookieId {cookieIdNum = 23842}
testObject_CookieId_user_12 :: CookieId
testObject_CookieId_user_12 = CookieId {cookieIdNum = 18465}
testObject_CookieId_user_13 :: CookieId
testObject_CookieId_user_13 = CookieId {cookieIdNum = 3097}
testObject_CookieId_user_14 :: CookieId
testObject_CookieId_user_14 = CookieId {cookieIdNum = 30437}
testObject_CookieId_user_15 :: CookieId
testObject_CookieId_user_15 = CookieId {cookieIdNum = 2987}
testObject_CookieId_user_16 :: CookieId
testObject_CookieId_user_16 = CookieId {cookieIdNum = 6442}
testObject_CookieId_user_17 :: CookieId
testObject_CookieId_user_17 = CookieId {cookieIdNum = 12728}
testObject_CookieId_user_18 :: CookieId
testObject_CookieId_user_18 = CookieId {cookieIdNum = 1827}
testObject_CookieId_user_19 :: CookieId
testObject_CookieId_user_19 = CookieId {cookieIdNum = 20422}
testObject_CookieId_user_20 :: CookieId
testObject_CookieId_user_20 = CookieId {cookieIdNum = 1341}
