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
testObject_Cookie_20_28_29_user_1 = (Cookie (CookieId {cookieIdNum = 2}) (SessionCookie) (read ("1864-05-08 04:26:57.252669619552 UTC")) (read ("1864-05-12 15:10:36.927315111461 UTC")) (Just (CookieLabel {cookieLabelText = "Nk7"})) (Just (CookieId {cookieIdNum = 1})) (()))
testObject_Cookie_20_28_29_user_2 :: Cookie ()
testObject_Cookie_20_28_29_user_2 = (Cookie (CookieId {cookieIdNum = 3}) (PersistentCookie) (read ("1864-05-05 15:21:25.616262110081 UTC")) (read ("1864-05-12 17:42:38.734125958289 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 1})) (()))
testObject_Cookie_20_28_29_user_3 :: Cookie ()
testObject_Cookie_20_28_29_user_3 = (Cookie (CookieId {cookieIdNum = 3}) (SessionCookie) (read ("1864-05-12 18:19:19.886162602255 UTC")) (read ("1864-05-07 09:02:23.307697195439 UTC")) (Just (CookieLabel {cookieLabelText = "j"})) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_user_4 :: Cookie ()
testObject_Cookie_20_28_29_user_4 = (Cookie (CookieId {cookieIdNum = 3}) (SessionCookie) (read ("1864-05-09 11:44:40.806315763955 UTC")) (read ("1864-05-13 11:02:08.546312560007 UTC")) (Just (CookieLabel {cookieLabelText = "N\b"})) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_user_5 :: Cookie ()
testObject_Cookie_20_28_29_user_5 = (Cookie (CookieId {cookieIdNum = 4}) (SessionCookie) (read ("1864-05-09 18:03:11.431748565148 UTC")) (read ("1864-05-06 02:47:57.694942577463 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 1})) (()))
testObject_Cookie_20_28_29_user_6 :: Cookie ()
testObject_Cookie_20_28_29_user_6 = (Cookie (CookieId {cookieIdNum = 3}) (SessionCookie) (read ("1864-05-11 16:21:43.477579208098 UTC")) (read ("1864-05-08 19:24:01.743534147593 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_user_7 :: Cookie ()
testObject_Cookie_20_28_29_user_7 = (Cookie (CookieId {cookieIdNum = 3}) (PersistentCookie) (read ("1864-05-09 00:38:21.971590064193 UTC")) (read ("1864-05-07 23:25:42.624805803599 UTC")) (Just (CookieLabel {cookieLabelText = "U\ETXs"})) (Nothing) (()))
testObject_Cookie_20_28_29_user_8 :: Cookie ()
testObject_Cookie_20_28_29_user_8 = (Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-12 12:49:19.746225231094 UTC")) (read ("1864-05-12 22:52:25.476789654756 UTC")) (Just (CookieLabel {cookieLabelText = "h;\CAN,"})) (Just (CookieId {cookieIdNum = 3})) (()))
testObject_Cookie_20_28_29_user_9 :: Cookie ()
testObject_Cookie_20_28_29_user_9 = (Cookie (CookieId {cookieIdNum = 3}) (PersistentCookie) (read ("1864-05-11 13:17:23.038343792701 UTC")) (read ("1864-05-09 06:07:00.851756176267 UTC")) (Just (CookieLabel {cookieLabelText = "%z"})) (Nothing) (()))
testObject_Cookie_20_28_29_user_10 :: Cookie ()
testObject_Cookie_20_28_29_user_10 = (Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-07 02:38:13.992144298841 UTC")) (read ("1864-05-07 11:38:13.568039673518 UTC")) (Just (CookieLabel {cookieLabelText = "\a]"})) (Just (CookieId {cookieIdNum = 0})) (()))
testObject_Cookie_20_28_29_user_11 :: Cookie ()
testObject_Cookie_20_28_29_user_11 = (Cookie (CookieId {cookieIdNum = 3}) (PersistentCookie) (read ("1864-05-11 19:50:40.274601425931 UTC")) (read ("1864-05-11 10:20:47.154720609003 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 4})) (()))
testObject_Cookie_20_28_29_user_12 :: Cookie ()
testObject_Cookie_20_28_29_user_12 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-13 04:51:56.090183087988 UTC")) (read ("1864-05-05 15:08:01.914368296915 UTC")) (Just (CookieLabel {cookieLabelText = " ~\a\ETB"})) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_user_13 :: Cookie ()
testObject_Cookie_20_28_29_user_13 = (Cookie (CookieId {cookieIdNum = 4}) (SessionCookie) (read ("1864-05-06 03:40:19.601646622653 UTC")) (read ("1864-05-08 04:48:24.167561838756 UTC")) (Just (CookieLabel {cookieLabelText = "9-\SYNe"})) (Nothing) (()))
testObject_Cookie_20_28_29_user_14 :: Cookie ()
testObject_Cookie_20_28_29_user_14 = (Cookie (CookieId {cookieIdNum = 4}) (PersistentCookie) (read ("1864-05-06 09:36:54.487519141672 UTC")) (read ("1864-05-07 01:45:38.125188795544 UTC")) (Just (CookieLabel {cookieLabelText = "\137535BB"})) (Just (CookieId {cookieIdNum = 0})) (()))
testObject_Cookie_20_28_29_user_15 :: Cookie ()
testObject_Cookie_20_28_29_user_15 = (Cookie (CookieId {cookieIdNum = 4}) (SessionCookie) (read ("1864-05-10 05:42:01.277904637507 UTC")) (read ("1864-05-05 08:07:03.175555901259 UTC")) (Just (CookieLabel {cookieLabelText = "\1052830r\ETB"})) (Just (CookieId {cookieIdNum = 1})) (()))
testObject_Cookie_20_28_29_user_16 :: Cookie ()
testObject_Cookie_20_28_29_user_16 = (Cookie (CookieId {cookieIdNum = 2}) (SessionCookie) (read ("1864-05-10 10:12:53.389321605936 UTC")) (read ("1864-05-12 00:32:19.312443234055 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_user_17 :: Cookie ()
testObject_Cookie_20_28_29_user_17 = (Cookie (CookieId {cookieIdNum = 4}) (PersistentCookie) (read ("1864-05-13 00:02:41.599681731967 UTC")) (read ("1864-05-10 23:22:32.849714819763 UTC")) (Just (CookieLabel {cookieLabelText = "\153676"})) (Nothing) (()))
testObject_Cookie_20_28_29_user_18 :: Cookie ()
testObject_Cookie_20_28_29_user_18 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-12 20:13:14.434532099126 UTC")) (read ("1864-05-08 02:14:38.452211551737 UTC")) (Just (CookieLabel {cookieLabelText = "\1054500\190436A\120630"})) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_user_19 :: Cookie ()
testObject_Cookie_20_28_29_user_19 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-06 17:40:25.144821451169 UTC")) (read ("1864-05-12 13:21:32.976619573877 UTC")) (Just (CookieLabel {cookieLabelText = ";"})) (Just (CookieId {cookieIdNum = 1})) (()))
testObject_Cookie_20_28_29_user_20 :: Cookie ()
testObject_Cookie_20_28_29_user_20 = (Cookie (CookieId {cookieIdNum = 4}) (PersistentCookie) (read ("1864-05-05 00:59:42.155025064485 UTC")) (read ("1864-05-13 11:46:21.601133872449 UTC")) (Just (CookieLabel {cookieLabelText = ":A-\1070204"})) (Just (CookieId {cookieIdNum = 3})) (()))
