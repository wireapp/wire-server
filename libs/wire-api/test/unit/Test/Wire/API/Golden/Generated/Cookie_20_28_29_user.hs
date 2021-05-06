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
testObject_Cookie_20_28_29_user_1 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-13 01:47:33.030987566402 UTC")) (read ("1864-05-10 12:31:16.688418148951 UTC")) (Just (CookieLabel {cookieLabelText = "a"})) (Nothing) (()))
testObject_Cookie_20_28_29_user_2 :: Cookie ()
testObject_Cookie_20_28_29_user_2 = (Cookie (CookieId {cookieIdNum = 2}) (PersistentCookie) (read ("1864-05-09 14:15:47.850448074728 UTC")) (read ("1864-05-07 23:28:53.702844675834 UTC")) (Just (CookieLabel {cookieLabelText = "9\1110885g"})) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_user_3 :: Cookie ()
testObject_Cookie_20_28_29_user_3 = (Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-13 01:58:07.456691512647 UTC")) (read ("1864-05-06 23:10:13.384058358002 UTC")) (Just (CookieLabel {cookieLabelText = "\DEL\\"})) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_user_4 :: Cookie ()
testObject_Cookie_20_28_29_user_4 = (Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-08 15:36:03.19040557518 UTC")) (read ("1864-05-11 23:13:56.87413261515 UTC")) (Just (CookieLabel {cookieLabelText = "V%"})) (Just (CookieId {cookieIdNum = 0})) (()))
testObject_Cookie_20_28_29_user_5 :: Cookie ()
testObject_Cookie_20_28_29_user_5 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-12 21:59:25.960675387017 UTC")) (read ("1864-05-11 12:53:23.48888231266 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 3})) (()))
testObject_Cookie_20_28_29_user_6 :: Cookie ()
testObject_Cookie_20_28_29_user_6 = (Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-12 00:52:45.594492552407 UTC")) (read ("1864-05-10 19:20:04.028628912448 UTC")) (Just (CookieLabel {cookieLabelText = "\ENQh\1110510~"})) (Just (CookieId {cookieIdNum = 1})) (()))
testObject_Cookie_20_28_29_user_7 :: Cookie ()
testObject_Cookie_20_28_29_user_7 = (Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-07 09:31:35.382931727253 UTC")) (read ("1864-05-08 01:56:12.463674595856 UTC")) (Just (CookieLabel {cookieLabelText = ":z"})) (Just (CookieId {cookieIdNum = 3})) (()))
testObject_Cookie_20_28_29_user_8 :: Cookie ()
testObject_Cookie_20_28_29_user_8 = (Cookie (CookieId {cookieIdNum = 2}) (SessionCookie) (read ("1864-05-12 15:18:59.310467613831 UTC")) (read ("1864-05-12 07:48:28.303094012633 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_user_9 :: Cookie ()
testObject_Cookie_20_28_29_user_9 = (Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-06 13:54:16.560091192273 UTC")) (read ("1864-05-08 14:56:32.493680839856 UTC")) (Just (CookieLabel {cookieLabelText = "\1103258\FSd"})) (Just (CookieId {cookieIdNum = 1})) (()))
testObject_Cookie_20_28_29_user_10 :: Cookie ()
testObject_Cookie_20_28_29_user_10 = (Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-10 10:35:46.546942302302 UTC")) (read ("1864-05-08 15:31:38.558048700426 UTC")) (Just (CookieLabel {cookieLabelText = "\167845\&8"})) (Nothing) (()))
testObject_Cookie_20_28_29_user_11 :: Cookie ()
testObject_Cookie_20_28_29_user_11 = (Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-05 06:24:53.650381748859 UTC")) (read ("1864-05-11 20:12:18.376723284396 UTC")) (Just (CookieLabel {cookieLabelText = "2"})) (Just (CookieId {cookieIdNum = 0})) (()))
testObject_Cookie_20_28_29_user_12 :: Cookie ()
testObject_Cookie_20_28_29_user_12 = (Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-13 16:56:03.164133947429 UTC")) (read ("1864-05-06 06:26:32.008984579402 UTC")) (Just (CookieLabel {cookieLabelText = "[\1025715\EOTd"})) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_user_13 :: Cookie ()
testObject_Cookie_20_28_29_user_13 = (Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-12 21:56:03.602474789353 UTC")) (read ("1864-05-12 12:07:19.058830713729 UTC")) (Just (CookieLabel {cookieLabelText = "y"})) (Just (CookieId {cookieIdNum = 3})) (()))
testObject_Cookie_20_28_29_user_14 :: Cookie ()
testObject_Cookie_20_28_29_user_14 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-05 10:58:44.088974715137 UTC")) (read ("1864-05-12 07:16:15.325951191291 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (()))
testObject_Cookie_20_28_29_user_15 :: Cookie ()
testObject_Cookie_20_28_29_user_15 = (Cookie (CookieId {cookieIdNum = 2}) (SessionCookie) (read ("1864-05-12 20:43:37.020567557992 UTC")) (read ("1864-05-09 14:10:36.830481513527 UTC")) (Just (CookieLabel {cookieLabelText = "'\1101063P\""})) (Just (CookieId {cookieIdNum = 3})) (()))
testObject_Cookie_20_28_29_user_16 :: Cookie ()
testObject_Cookie_20_28_29_user_16 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-13 17:46:40.952369273953 UTC")) (read ("1864-05-06 03:34:17.892497660885 UTC")) (Just (CookieLabel {cookieLabelText = "\nI\150978"})) (Nothing) (()))
testObject_Cookie_20_28_29_user_17 :: Cookie ()
testObject_Cookie_20_28_29_user_17 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-05 22:03:58.811587052317 UTC")) (read ("1864-05-06 03:18:12.680463671867 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_user_18 :: Cookie ()
testObject_Cookie_20_28_29_user_18 = (Cookie (CookieId {cookieIdNum = 2}) (PersistentCookie) (read ("1864-05-13 09:43:27.400173826841 UTC")) (read ("1864-05-12 14:32:21.033081976536 UTC")) (Just (CookieLabel {cookieLabelText = "\22341>"})) (Just (CookieId {cookieIdNum = 1})) (()))
testObject_Cookie_20_28_29_user_19 :: Cookie ()
testObject_Cookie_20_28_29_user_19 = (Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-05 01:57:16.99734450708 UTC")) (read ("1864-05-05 21:04:14.820029094335 UTC")) (Just (CookieLabel {cookieLabelText = "\17706"})) (Just (CookieId {cookieIdNum = 0})) (()))
testObject_Cookie_20_28_29_user_20 :: Cookie ()
testObject_Cookie_20_28_29_user_20 = (Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 06:16:26.231476266639 UTC")) (read ("1864-05-08 17:15:35.637862848045 UTC")) (Just (CookieLabel {cookieLabelText = "\37022=H"})) (Just (CookieId {cookieIdNum = 2})) (()))
