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
testObject_Cookie_20_28_29_user_1 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-10 11:19:42.115123898394 UTC")) (read ("1864-05-05 10:27:19.13535929982 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 1})) (()))
testObject_Cookie_20_28_29_user_2 :: Cookie ()
testObject_Cookie_20_28_29_user_2 = (Cookie (CookieId {cookieIdNum = 3}) (PersistentCookie) (read ("1864-05-06 01:19:35.439001638204 UTC")) (read ("1864-05-07 13:41:02.446739097882 UTC")) (Just (CookieLabel {cookieLabelText = "F\1102372"})) (Just (CookieId {cookieIdNum = 4})) (()))
testObject_Cookie_20_28_29_user_3 :: Cookie ()
testObject_Cookie_20_28_29_user_3 = (Cookie (CookieId {cookieIdNum = 4}) (PersistentCookie) (read ("1864-05-05 08:02:14.598566130254 UTC")) (read ("1864-05-11 20:44:25.234213085583 UTC")) (Just (CookieLabel {cookieLabelText = "\DC12\SOHE"})) (Just (CookieId {cookieIdNum = 4})) (()))
testObject_Cookie_20_28_29_user_4 :: Cookie ()
testObject_Cookie_20_28_29_user_4 = (Cookie (CookieId {cookieIdNum = 4}) (PersistentCookie) (read ("1864-05-06 21:42:56.827898847404 UTC")) (read ("1864-05-08 02:55:40.118937469316 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (()))
testObject_Cookie_20_28_29_user_5 :: Cookie ()
testObject_Cookie_20_28_29_user_5 = (Cookie (CookieId {cookieIdNum = 3}) (SessionCookie) (read ("1864-05-05 18:20:51.63194943103 UTC")) (read ("1864-05-10 04:24:26.102282571244 UTC")) (Just (CookieLabel {cookieLabelText = "N"})) (Nothing) (()))
