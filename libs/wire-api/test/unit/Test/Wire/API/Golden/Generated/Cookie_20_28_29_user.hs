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
testObject_Cookie_20_28_29_1 :: Cookie ()
testObject_Cookie_20_28_29_1 = (Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 03:52:43.523296166167 UTC")) (read ("1864-05-08 17:22:13.455083124281 UTC")) (Just (CookieLabel {cookieLabelText = "#="})) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_2 :: Cookie ()
testObject_Cookie_20_28_29_2 = (Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 00:32:16.804623603286 UTC")) (read ("1864-05-07 03:47:46.665177051185 UTC")) (Just (CookieLabel {cookieLabelText = "\1106910\1068257\DLEZ"})) (Just (CookieId {cookieIdNum = 4})) (()))
testObject_Cookie_20_28_29_3 :: Cookie ()
testObject_Cookie_20_28_29_3 = (Cookie (CookieId {cookieIdNum = 4}) (SessionCookie) (read ("1864-05-13 18:42:48.360314662815 UTC")) (read ("1864-05-06 02:10:24.578270721184 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 2})) (()))
testObject_Cookie_20_28_29_4 :: Cookie ()
testObject_Cookie_20_28_29_4 = (Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-10 13:04:26.812886386476 UTC")) (read ("1864-05-12 13:51:13.105335101572 UTC")) (Just (CookieLabel {cookieLabelText = "c"})) (Just (CookieId {cookieIdNum = 1})) (()))
testObject_Cookie_20_28_29_5 :: Cookie ()
testObject_Cookie_20_28_29_5 = (Cookie (CookieId {cookieIdNum = 3}) (SessionCookie) (read ("1864-05-08 21:28:15.937030521148 UTC")) (read ("1864-05-11 11:33:17.881637728103 UTC")) (Just (CookieLabel {cookieLabelText = "n"})) (Just (CookieId {cookieIdNum = 1})) (()))
