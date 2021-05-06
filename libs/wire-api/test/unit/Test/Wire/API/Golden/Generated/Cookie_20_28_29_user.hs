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
testObject_Cookie_20_28_29_user_1 = (Cookie (CookieId {cookieIdNum = 2}) (SessionCookie) (read ("1864-05-09 05:18:16.436502887299 UTC")) (read ("1864-05-06 02:14:43.483098457577 UTC")) (Nothing) (Nothing) (()))
testObject_Cookie_20_28_29_user_2 :: Cookie ()
testObject_Cookie_20_28_29_user_2 = (Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 00:27:22.91769780428 UTC")) (read ("1864-05-12 19:17:37.252152923407 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (()))
testObject_Cookie_20_28_29_user_3 :: Cookie ()
testObject_Cookie_20_28_29_user_3 = (Cookie (CookieId {cookieIdNum = 3}) (SessionCookie) (read ("1864-05-05 10:21:40.142031594048 UTC")) (read ("1864-05-05 10:33:09.643915470209 UTC")) (Just (CookieLabel {cookieLabelText = "\1096530\1088991"})) (Just (CookieId {cookieIdNum = 1})) (()))
testObject_Cookie_20_28_29_user_4 :: Cookie ()
testObject_Cookie_20_28_29_user_4 = (Cookie (CookieId {cookieIdNum = 3}) (SessionCookie) (read ("1864-05-10 05:26:38.658476654039 UTC")) (read ("1864-05-12 05:46:47.401478221683 UTC")) (Just (CookieLabel {cookieLabelText = "\SOH\DLE"})) (Just (CookieId {cookieIdNum = 0})) (()))
testObject_Cookie_20_28_29_user_5 :: Cookie ()
testObject_Cookie_20_28_29_user_5 = (Cookie (CookieId {cookieIdNum = 3}) (SessionCookie) (read ("1864-05-08 11:24:38.297565644446 UTC")) (read ("1864-05-11 07:50:53.785102823865 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 3})) (()))
