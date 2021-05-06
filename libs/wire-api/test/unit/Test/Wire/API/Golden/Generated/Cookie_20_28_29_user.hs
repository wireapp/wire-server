{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Cookie_20_28_29_user where

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
testObject_Cookie_20_28_29_user_1 :: Cookie ()
testObject_Cookie_20_28_29_user_1 = (Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-08 00:29:34.412241234479 UTC")) (read ("1864-05-06 11:44:13.751401707551 UTC")) (Just (CookieLabel {cookieLabelText = "%\59925"})) (Just (CookieId {cookieIdNum = 1})) (()))
testObject_Cookie_20_28_29_user_2 :: Cookie ()
testObject_Cookie_20_28_29_user_2 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 11:56:31.047123474483 UTC")) (read ("1864-05-08 02:32:57.393849913647 UTC")) (Just (CookieLabel {cookieLabelText = "\100408Z/"})) (Nothing) (()))
testObject_Cookie_20_28_29_user_3 :: Cookie ()
testObject_Cookie_20_28_29_user_3 = (Cookie (CookieId {cookieIdNum = 4}) (SessionCookie) (read ("1864-05-10 21:21:55.350528312003 UTC")) (read ("1864-05-10 04:59:41.795125670829 UTC")) (Just (CookieLabel {cookieLabelText = "\SOH(\1019450"})) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_user_4 :: Cookie ()
testObject_Cookie_20_28_29_user_4 = (Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-07 20:12:36.507762584296 UTC")) (read ("1864-05-11 19:28:59.616199988799 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 1})) (()))
testObject_Cookie_20_28_29_user_5 :: Cookie ()
testObject_Cookie_20_28_29_user_5 = (Cookie (CookieId {cookieIdNum = 2}) (SessionCookie) (read ("1864-05-10 06:22:19.082385397825 UTC")) (read ("1864-05-06 16:59:36.340589432948 UTC")) (Just (CookieLabel {cookieLabelText = "8\ENQw"})) (Just (CookieId {cookieIdNum = 4})) (()))
testObject_Cookie_20_28_29_user_6 :: Cookie ()
testObject_Cookie_20_28_29_user_6 = (Cookie (CookieId {cookieIdNum = 2}) (PersistentCookie) (read ("1864-05-09 21:49:53.210613976349 UTC")) (read ("1864-05-12 04:46:15.258096999215 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 3})) (()))
testObject_Cookie_20_28_29_user_7 :: Cookie ()
testObject_Cookie_20_28_29_user_7 = (Cookie (CookieId {cookieIdNum = 3}) (SessionCookie) (read ("1864-05-08 03:13:26.126077390848 UTC")) (read ("1864-05-11 12:09:36.883682512743 UTC")) (Just (CookieLabel {cookieLabelText = "v"})) (Just (CookieId {cookieIdNum = 1})) (()))
testObject_Cookie_20_28_29_user_8 :: Cookie ()
testObject_Cookie_20_28_29_user_8 = (Cookie (CookieId {cookieIdNum = 4}) (SessionCookie) (read ("1864-05-13 05:11:38.744291042272 UTC")) (read ("1864-05-07 00:55:12.887262292414 UTC")) (Just (CookieLabel {cookieLabelText = "M\1008512"})) (Just (CookieId {cookieIdNum = 0})) (()))
testObject_Cookie_20_28_29_user_9 :: Cookie ()
testObject_Cookie_20_28_29_user_9 = (Cookie (CookieId {cookieIdNum = 2}) (PersistentCookie) (read ("1864-05-11 02:15:56.16254016452 UTC")) (read ("1864-05-09 19:35:22.3782922737 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 0})) (()))
testObject_Cookie_20_28_29_user_10 :: Cookie ()
testObject_Cookie_20_28_29_user_10 = (Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-07 23:32:56.384089738392 UTC")) (read ("1864-05-13 00:07:18.915398803223 UTC")) (Nothing) (Nothing) (()))
testObject_Cookie_20_28_29_user_11 :: Cookie ()
testObject_Cookie_20_28_29_user_11 = (Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-07 12:23:54.861694192746 UTC")) (read ("1864-05-08 06:36:19.853994382116 UTC")) (Just (CookieLabel {cookieLabelText = "\1097061x\SYNZ"})) (Just (CookieId {cookieIdNum = 0})) (()))
testObject_Cookie_20_28_29_user_12 :: Cookie ()
testObject_Cookie_20_28_29_user_12 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-12 07:36:17.502163227185 UTC")) (read ("1864-05-12 10:36:26.76611329391 UTC")) (Just (CookieLabel {cookieLabelText = "R"})) (Just (CookieId {cookieIdNum = 0})) (()))
testObject_Cookie_20_28_29_user_13 :: Cookie ()
testObject_Cookie_20_28_29_user_13 = (Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-08 07:26:29.984031501859 UTC")) (read ("1864-05-05 21:48:03.378115630694 UTC")) (Just (CookieLabel {cookieLabelText = "6W"})) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_user_14 :: Cookie ()
testObject_Cookie_20_28_29_user_14 = (Cookie (CookieId {cookieIdNum = 2}) (PersistentCookie) (read ("1864-05-06 10:28:37.199738841518 UTC")) (read ("1864-05-07 01:19:19.459944145799 UTC")) (Just (CookieLabel {cookieLabelText = "]E`"})) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_user_15 :: Cookie ()
testObject_Cookie_20_28_29_user_15 = (Cookie (CookieId {cookieIdNum = 3}) (PersistentCookie) (read ("1864-05-08 05:11:09.703600662756 UTC")) (read ("1864-05-10 17:31:46.103871729421 UTC")) (Just (CookieLabel {cookieLabelText = "/"})) (Just (CookieId {cookieIdNum = 4})) (()))
testObject_Cookie_20_28_29_user_16 :: Cookie ()
testObject_Cookie_20_28_29_user_16 = (Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-13 08:10:38.007231639909 UTC")) (read ("1864-05-13 05:14:45.706740410281 UTC")) (Just (CookieLabel {cookieLabelText = ":g\STX("})) (Nothing) (()))
testObject_Cookie_20_28_29_user_17 :: Cookie ()
testObject_Cookie_20_28_29_user_17 = (Cookie (CookieId {cookieIdNum = 3}) (SessionCookie) (read ("1864-05-07 03:59:50.051569749637 UTC")) (read ("1864-05-08 17:50:58.206194727696 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Nothing) (()))
testObject_Cookie_20_28_29_user_18 :: Cookie ()
testObject_Cookie_20_28_29_user_18 = (Cookie (CookieId {cookieIdNum = 4}) (SessionCookie) (read ("1864-05-09 18:02:25.026701426686 UTC")) (read ("1864-05-07 20:09:34.384009221481 UTC")) (Just (CookieLabel {cookieLabelText = "H\75018"})) (Nothing) (()))
testObject_Cookie_20_28_29_user_19 :: Cookie ()
testObject_Cookie_20_28_29_user_19 = (Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-12 07:26:01.502768498776 UTC")) (read ("1864-05-09 18:14:40.735484863835 UTC")) (Just (CookieLabel {cookieLabelText = "\DC2\1011277\STX"})) (Just (CookieId {cookieIdNum = 3})) (()))
testObject_Cookie_20_28_29_user_20 :: Cookie ()
testObject_Cookie_20_28_29_user_20 = (Cookie (CookieId {cookieIdNum = 2}) (PersistentCookie) (read ("1864-05-10 02:32:17.569839094125 UTC")) (read ("1864-05-11 21:11:43.395158954372 UTC")) (Just (CookieLabel {cookieLabelText = "\189902!"})) (Just (CookieId {cookieIdNum = 3})) (()))
