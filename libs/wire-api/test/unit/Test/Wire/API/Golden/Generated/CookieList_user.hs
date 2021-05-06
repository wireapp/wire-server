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
testObject_CookieList_user_1 = CookieList {cookieList = [(Cookie (CookieId {cookieIdNum = 2}) (SessionCookie) (read ("1864-05-11 10:14:06.129895509009 UTC")) (read ("1864-05-11 09:57:30.900164987338 UTC")) (Just (CookieLabel {cookieLabelText = "X"})) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 2}) (SessionCookie) (read ("1864-05-09 02:12:36.105670142303 UTC")) (read ("1864-05-10 19:17:48.354236731829 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 2})) (()))]}
testObject_CookieList_user_2 :: CookieList
testObject_CookieList_user_2 = CookieList {cookieList = [(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 00:07:24.525340167507 UTC")) (read ("1864-05-07 06:55:30.831776671999 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 2}) (SessionCookie) (read ("1864-05-11 17:06:32.075826417938 UTC")) (read ("1864-05-07 23:55:06.651703112642 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Nothing) (()))]}
testObject_CookieList_user_3 :: CookieList
testObject_CookieList_user_3 = CookieList {cookieList = [(Cookie (CookieId {cookieIdNum = 3}) (PersistentCookie) (read ("1864-05-06 21:44:18.679020383031 UTC")) (read ("1864-05-09 08:56:13.637753775761 UTC")) (Just (CookieLabel {cookieLabelText = "\NAK\1028024\NUL6"})) (Nothing) (()))]}
testObject_CookieList_user_4 :: CookieList
testObject_CookieList_user_4 = CookieList {cookieList = [(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-09 09:08:27.674459893671 UTC")) (read ("1864-05-09 17:42:11.426426363956 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 09:09:16.195350623296 UTC")) (read ("1864-05-09 02:14:48.888447719359 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 02:48:11.237448909851 UTC")) (read ("1864-05-09 12:48:20.029282344461 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-09 07:49:59.972550988055 UTC")) (read ("1864-05-09 10:51:44.514541593955 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-09 03:18:38.670558919804 UTC")) (read ("1864-05-09 17:23:38.5040179407 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 11:10:22.00862956437 UTC")) (read ("1864-05-09 13:05:41.090180592298 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (()))]}
testObject_CookieList_user_5 :: CookieList
testObject_CookieList_user_5 = CookieList {cookieList = [(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 17:53:57.4169368098 UTC")) (read ("1864-05-09 15:18:09.236249440014 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 03:11:56.139657499266 UTC")) (read ("1864-05-09 02:17:22.459924930581 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-09 01:08:09.324124787936 UTC")) (read ("1864-05-09 17:17:50.647109114819 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 01:07:36.191051849199 UTC")) (read ("1864-05-09 09:21:28.262006664554 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 17:29:17.648372482472 UTC")) (read ("1864-05-09 05:48:02.399303328285 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 06:41:27.35101108078 UTC")) (read ("1864-05-09 16:26:25.585822666958 UTC")) (Nothing) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 16:49:26.247390112919 UTC")) (read ("1864-05-09 23:50:48.637897283959 UTC")) (Nothing) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-09 17:45:12.775527535552 UTC")) (read ("1864-05-09 23:13:02.680183417513 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 1})) (())),(Cookie (CookieId {cookieIdNum = 1}) (PersistentCookie) (read ("1864-05-09 00:42:58.698442303388 UTC")) (read ("1864-05-09 15:54:08.833739290494 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 1}) (SessionCookie) (read ("1864-05-09 17:59:06.128733589481 UTC")) (read ("1864-05-09 08:52:36.244269471546 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Nothing) (())),(Cookie (CookieId {cookieIdNum = 0}) (PersistentCookie) (read ("1864-05-09 09:31:36.498261818029 UTC")) (read ("1864-05-09 17:51:12.75038092838 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (())),(Cookie (CookieId {cookieIdNum = 0}) (SessionCookie) (read ("1864-05-09 20:26:29.000127546809 UTC")) (read ("1864-05-09 13:35:07.125158453367 UTC")) (Just (CookieLabel {cookieLabelText = ""})) (Just (CookieId {cookieIdNum = 0})) (()))]}
