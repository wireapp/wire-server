{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.AddBot_user where

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
testObject_AddBot_user_1 :: AddBot
testObject_AddBot_user_1 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000015-0000-0005-0000-000700000012"))), addBotService = (Id (fromJust (UUID.fromString "00000014-0000-0020-0000-000a00000020"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.VE, lCountry = Just (Country {fromCountry = TN})})}
testObject_AddBot_user_2 :: AddBot
testObject_AddBot_user_2 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000014-0000-000d-0000-001100000005"))), addBotService = (Id (fromJust (UUID.fromString "0000000f-0000-001d-0000-001a00000019"))), addBotLocale = Nothing}
testObject_AddBot_user_3 :: AddBot
testObject_AddBot_user_3 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000019-0000-0003-0000-001b0000001f"))), addBotService = (Id (fromJust (UUID.fromString "00000001-0000-0007-0000-001200000004"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TN, lCountry = Nothing})}
testObject_AddBot_user_4 :: AddBot
testObject_AddBot_user_4 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001b-0000-001b-0000-00030000001c"))), addBotService = (Id (fromJust (UUID.fromString "00000007-0000-001b-0000-00140000001a"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BR, lCountry = Nothing})}
testObject_AddBot_user_5 :: AddBot
testObject_AddBot_user_5 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000a-0000-000b-0000-000c00000019"))), addBotService = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000b0000000f"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SK, lCountry = Just (Country {fromCountry = LK})})}
testObject_AddBot_user_6 :: AddBot
testObject_AddBot_user_6 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000e-0000-0015-0000-001c00000013"))), addBotService = (Id (fromJust (UUID.fromString "00000011-0000-0020-0000-00110000000f"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SS, lCountry = Just (Country {fromCountry = SI})})}
testObject_AddBot_user_7 :: AddBot
testObject_AddBot_user_7 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001d-0000-0018-0000-001d0000000c"))), addBotService = (Id (fromJust (UUID.fromString "0000001f-0000-000a-0000-000b00000001"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.VI, lCountry = Just (Country {fromCountry = IL})})}
testObject_AddBot_user_8 :: AddBot
testObject_AddBot_user_8 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000c-0000-001c-0000-000a0000001b"))), addBotService = (Id (fromJust (UUID.fromString "00000010-0000-0005-0000-001500000004"))), addBotLocale = Nothing}
testObject_AddBot_user_9 :: AddBot
testObject_AddBot_user_9 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000b-0000-0015-0000-00090000001a"))), addBotService = (Id (fromJust (UUID.fromString "00000002-0000-001f-0000-001c0000000e"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.YO, lCountry = Nothing})}
testObject_AddBot_user_10 :: AddBot
testObject_AddBot_user_10 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000b-0000-001c-0000-000000000015"))), addBotService = (Id (fromJust (UUID.fromString "0000001a-0000-001c-0000-001f00000004"))), addBotLocale = Nothing}
testObject_AddBot_user_11 :: AddBot
testObject_AddBot_user_11 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000b-0000-0019-0000-000200000007"))), addBotService = (Id (fromJust (UUID.fromString "00000009-0000-0020-0000-00130000000e"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BG, lCountry = Just (Country {fromCountry = PH})})}
testObject_AddBot_user_12 :: AddBot
testObject_AddBot_user_12 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001f-0000-000a-0000-001000000020"))), addBotService = (Id (fromJust (UUID.fromString "00000017-0000-0006-0000-000800000001"))), addBotLocale = Nothing}
testObject_AddBot_user_13 :: AddBot
testObject_AddBot_user_13 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001b-0000-0004-0000-001c0000000d"))), addBotService = (Id (fromJust (UUID.fromString "0000001c-0000-0007-0000-00120000001e"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.CV, lCountry = Nothing})}
testObject_AddBot_user_14 :: AddBot
testObject_AddBot_user_14 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001e-0000-0016-0000-002000000000"))), addBotService = (Id (fromJust (UUID.fromString "0000000d-0000-0009-0000-00120000000f"))), addBotLocale = Nothing}
testObject_AddBot_user_15 :: AddBot
testObject_AddBot_user_15 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000017-0000-0003-0000-00150000000a"))), addBotService = (Id (fromJust (UUID.fromString "00000004-0000-000e-0000-000700000020"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.RW, lCountry = Just (Country {fromCountry = KR})})}
testObject_AddBot_user_16 :: AddBot
testObject_AddBot_user_16 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001a-0000-000e-0000-000e00000009"))), addBotService = (Id (fromJust (UUID.fromString "00000002-0000-0012-0000-001300000019"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.ML, lCountry = Just (Country {fromCountry = SA})})}
testObject_AddBot_user_17 :: AddBot
testObject_AddBot_user_17 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000002-0000-0009-0000-00020000001c"))), addBotService = (Id (fromJust (UUID.fromString "00000017-0000-0003-0000-000f0000001f"))), addBotLocale = Nothing}
testObject_AddBot_user_18 :: AddBot
testObject_AddBot_user_18 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000011-0000-000a-0000-001a0000001a"))), addBotService = (Id (fromJust (UUID.fromString "00000003-0000-000f-0000-000b00000009"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KU, lCountry = Just (Country {fromCountry = CF})})}
testObject_AddBot_user_19 :: AddBot
testObject_AddBot_user_19 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000001-0000-0009-0000-000d00000008"))), addBotService = (Id (fromJust (UUID.fromString "00000017-0000-0000-0000-000e0000001a"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BO, lCountry = Nothing})}
testObject_AddBot_user_20 :: AddBot
testObject_AddBot_user_20 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000003-0000-0008-0000-00120000001e"))), addBotService = (Id (fromJust (UUID.fromString "0000000b-0000-0009-0000-001800000014"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.PL, lCountry = Just (Country {fromCountry = NA})})}
