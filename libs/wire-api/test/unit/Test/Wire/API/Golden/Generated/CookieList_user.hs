{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.CookieList_user where

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
testObject_CookieList_user_1 :: CookieList
testObject_CookieList_user_1 = CookieList {cookieList = []}
testObject_CookieList_user_2 :: CookieList
testObject_CookieList_user_2 = CookieList {cookieList = [(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-09 20:16:16.632873697366 UTC")) (read ("1864-05-09 22:35:43.539950747566 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 16:37:17.473477458261 UTC")) (read ("1864-05-09 07:43:42.322477455039 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 17:46:56.007358655988 UTC")) (read ("1864-05-09 09:49:02.037322246716 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 01:29:10.694526429741 UTC")) (read ("1864-05-09 19:35:31.929846905826 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 12:05:30.959803626203 UTC")) (read ("1864-05-09 09:34:22.128148618554 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 17:29:10.251610618783 UTC")) (read ("1864-05-09 15:43:34.459689158837 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 13:14:03.430907530841 UTC")) (read ("1864-05-09 19:01:36.94010958822 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 04:40:01.709326354484 UTC")) (read ("1864-05-09 10:58:46.198450246367 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 20:41:34.613967101526 UTC")) (read ("1864-05-09 03:54:56.48896261427 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 04:53:15.327303969101 UTC")) (read ("1864-05-09 01:46:09.542344262943 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 06:58:15.410329704147 UTC")) (read ("1864-05-09 04:43:47.243781584362 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 0})) (()))]}
testObject_CookieList_user_3 :: CookieList
testObject_CookieList_user_3 = CookieList {cookieList = [(Cookie (CookieId {cookieIdNum = 2}) (SessionCookie) (read ("1864-05-10 18:03:54.835294624957 UTC")) (read ("1864-05-11 06:41:16.586141052863 UTC")) (Just (CookieLabel {cookieLabelText = "\DC4\a"})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 2}) (SessionCookie) (read ("1864-05-07 05:33:17.156069730786 UTC")) (read ("1864-05-08 09:51:00.368257028836 UTC")) (Just (CookieLabel {cookieLabelText = "\SO\n"})) (Nothing) (()))]}
testObject_CookieList_user_4 :: CookieList
testObject_CookieList_user_4 = CookieList {cookieList = [(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 14:48:28.944316810993 UTC")) (read ("1864-05-09 07:17:14.097293887665 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 13:16:41.312051006068 UTC")) (read ("1864-05-09 08:46:46.65170933085 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 23:55:36.709837307033 UTC")) (read ("1864-05-09 18:10:42.414258855067 UTC")) (Nothing) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 05:09:27.84929461091 UTC")) (read ("1864-05-09 17:00:26.835146796896 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 01:18:02.181743601988 UTC")) (read ("1864-05-09 17:06:13.136274246448 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 05:16:05.180431337063 UTC")) (read ("1864-05-09 08:33:20.524109174955 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 07:21:18.081133485633 UTC")) (read ("1864-05-09 18:00:19.947879612918 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 0})) (()))]}
testObject_CookieList_user_5 :: CookieList
testObject_CookieList_user_5 = CookieList {cookieList = [(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 06:52:26.161580244799 UTC")) (read ("1864-05-09 13:43:40.449714789727 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 07:17:17.699641551949 UTC")) (read ("1864-05-09 09:26:47.961370821547 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 17:06:20.631667321547 UTC")) (read ("1864-05-09 02:18:00.28615464966 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 04:40:12.308323923036 UTC")) (read ("1864-05-09 14:19:36.568252823853 UTC")) (Nothing) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 01:53:53.513879442505 UTC")) (read ("1864-05-09 00:52:23.804898027792 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 17:18:46.863421502515 UTC")) (read ("1864-05-09 17:34:22.070895717428 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 23:20:04.726136811455 UTC")) (read ("1864-05-09 23:23:28.220516131172 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 04:57:17.980175652151 UTC")) (read ("1864-05-09 03:09:46.451764110541 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 04:45:04.258997052271 UTC")) (read ("1864-05-09 08:45:49.832179797602 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 00:50:42.599315914092 UTC")) (read ("1864-05-09 01:11:24.893738409505 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (()))]}
