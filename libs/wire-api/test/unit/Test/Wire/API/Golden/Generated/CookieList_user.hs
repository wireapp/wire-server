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
testObject_CookieList_1 :: CookieList
testObject_CookieList_1 = CookieList {cookieList = [(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 05:29:18.142853214449 UTC")) (read ("1864-05-09 16:21:51.752920377941 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-09 10:03:46.644458487819 UTC")) (read ("1864-05-09 13:36:49.020523054976 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-09 07:46:11.805153490724 UTC")) (read ("1864-05-09 01:58:15.218373767186 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-09 12:43:21.029340214961 UTC")) (read ("1864-05-09 11:49:12.23088097247 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 00:51:52.423835117911 UTC")) (read ("1864-05-09 01:12:15.567536083443 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-09 15:16:42.313743894119 UTC")) (read ("1864-05-09 21:18:09.416473495017 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 04:51:28.031165590124 UTC")) (read ("1864-05-09 19:38:11.52019848609 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 09:31:15.640677140545 UTC")) (read ("1864-05-09 01:06:27.03713066099 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-09 20:13:26.803057621364 UTC")) (read ("1864-05-09 09:18:47.991910186567 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 05:57:59.305602770986 UTC")) (read ("1864-05-09 08:17:57.538608678827 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-09 05:34:17.655359721622 UTC")) (read ("1864-05-09 06:10:01.450751652562 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 21:58:05.093150708533 UTC")) (read ("1864-05-09 06:34:31.225341757795 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 13:26:18.405391986671 UTC")) (read ("1864-05-09 20:20:48.534046383516 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 05:41:23.865638989398 UTC")) (read ("1864-05-09 12:42:49.06767877702 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 15:18:38.124377246174 UTC")) (read ("1864-05-09 22:56:31.632949999417 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 21:44:40.061323188667 UTC")) (read ("1864-05-09 16:30:39.696080763016 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 03:51:10.126898739676 UTC")) (read ("1864-05-09 02:53:33.112191250027 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-09 21:23:02.550894457832 UTC")) (read ("1864-05-09 18:06:37.683579022428 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-09 04:14:54.501463085234 UTC")) (read ("1864-05-09 23:36:13.201476798087 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 11:29:51.394555845698 UTC")) (read ("1864-05-09 20:08:05.59557392729 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 14:55:02.796043184622 UTC")) (read ("1864-05-09 14:29:42.380121859845 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 23:52:53.212117781091 UTC")) (read ("1864-05-09 21:58:35.212413237662 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Nothing) (()))]}
testObject_CookieList_2 :: CookieList
testObject_CookieList_2 = CookieList {cookieList = []}
testObject_CookieList_3 :: CookieList
testObject_CookieList_3 = CookieList {cookieList = []}
testObject_CookieList_4 :: CookieList
testObject_CookieList_4 = CookieList {cookieList = []}
testObject_CookieList_5 :: CookieList
testObject_CookieList_5 = CookieList {cookieList = [(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 17:15:51.383413489122 UTC")) (read ("1864-05-08 05:02:02.173255321056 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-08 09:20:10.324527672208 UTC")) (read ("1864-05-09 16:50:41.904447695196 UTC")) (Just (CookieLabel {cookieLabelText = "<"})) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 10:33:32.750910037488 UTC")) (read ("1864-05-09 19:46:54.780001228573 UTC")) (Nothing) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-08 05:41:52.713133115197 UTC")) (read ("1864-05-10 21:36:30.539803769352 UTC")) (Just (CookieLabel {cookieLabelText = "S"})) (Just (CookieId {cookieIdNum = 0})) (()))]}
