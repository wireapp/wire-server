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
testObject_AddBot_user_1 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000001-0000-0011-0000-00030000001c"))), addBotService = (Id (fromJust (UUID.fromString "0000001a-0000-001f-0000-000c00000009"))), addBotLocale = Nothing}
testObject_AddBot_user_2 :: AddBot
testObject_AddBot_user_2 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000e-0000-001a-0000-00030000001a"))), addBotService = (Id (fromJust (UUID.fromString "0000001d-0000-000d-0000-000c00000002"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KN, lCountry = Just (Country {fromCountry = CX})})}
testObject_AddBot_user_3 :: AddBot
testObject_AddBot_user_3 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000016-0000-000e-0000-00130000001e"))), addBotService = (Id (fromJust (UUID.fromString "0000000c-0000-0001-0000-001a00000005"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.LG, lCountry = Just (Country {fromCountry = GD})})}
testObject_AddBot_user_4 :: AddBot
testObject_AddBot_user_4 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000015-0000-0018-0000-00090000001d"))), addBotService = (Id (fromJust (UUID.fromString "00000008-0000-000c-0000-00180000001b"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SR, lCountry = Just (Country {fromCountry = PW})})}
testObject_AddBot_user_5 :: AddBot
testObject_AddBot_user_5 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000d-0000-000c-0000-00030000001f"))), addBotService = (Id (fromJust (UUID.fromString "0000000e-0000-001e-0000-001900000019"))), addBotLocale = Nothing}
testObject_AddBot_user_6 :: AddBot
testObject_AddBot_user_6 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000015-0000-0016-0000-000c0000000a"))), addBotService = (Id (fromJust (UUID.fromString "00000016-0000-0003-0000-000f00000015"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.NO, lCountry = Just (Country {fromCountry = KZ})})}
testObject_AddBot_user_7 :: AddBot
testObject_AddBot_user_7 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001e-0000-0004-0000-000800000008"))), addBotService = (Id (fromJust (UUID.fromString "00000018-0000-0002-0000-000b00000006"))), addBotLocale = Nothing}
testObject_AddBot_user_8 :: AddBot
testObject_AddBot_user_8 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001e-0000-001f-0000-001f0000001a"))), addBotService = (Id (fromJust (UUID.fromString "0000001a-0000-0018-0000-001600000001"))), addBotLocale = Nothing}
testObject_AddBot_user_9 :: AddBot
testObject_AddBot_user_9 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000019-0000-001c-0000-000000000016"))), addBotService = (Id (fromJust (UUID.fromString "00000003-0000-001e-0000-00050000001f"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.EU, lCountry = Just (Country {fromCountry = HU})})}
testObject_AddBot_user_10 :: AddBot
testObject_AddBot_user_10 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000013-0000-0006-0000-001300000014"))), addBotService = (Id (fromJust (UUID.fromString "00000019-0000-0010-0000-000600000004"))), addBotLocale = Nothing}
testObject_AddBot_user_11 :: AddBot
testObject_AddBot_user_11 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001f-0000-0010-0000-001200000010"))), addBotService = (Id (fromJust (UUID.fromString "00000016-0000-001e-0000-000d0000001d"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.MY, lCountry = Just (Country {fromCountry = YT})})}
testObject_AddBot_user_12 :: AddBot
testObject_AddBot_user_12 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001d-0000-001b-0000-001d0000001b"))), addBotService = (Id (fromJust (UUID.fromString "0000001a-0000-0020-0000-001c00000011"))), addBotLocale = Nothing}
testObject_AddBot_user_13 :: AddBot
testObject_AddBot_user_13 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001c-0000-000b-0000-001a00000001"))), addBotService = (Id (fromJust (UUID.fromString "0000000a-0000-001e-0000-001f00000002"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BN, lCountry = Just (Country {fromCountry = BG})})}
testObject_AddBot_user_14 :: AddBot
testObject_AddBot_user_14 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000016-0000-001c-0000-001a00000000"))), addBotService = (Id (fromJust (UUID.fromString "00000000-0000-0014-0000-000b00000008"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.CY, lCountry = Just (Country {fromCountry = TV})})}
testObject_AddBot_user_15 :: AddBot
testObject_AddBot_user_15 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000015-0000-0007-0000-001b00000014"))), addBotService = (Id (fromJust (UUID.fromString "0000001f-0000-0012-0000-001d00000009"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.MR, lCountry = Just (Country {fromCountry = NL})})}
testObject_AddBot_user_16 :: AddBot
testObject_AddBot_user_16 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000017-0000-0018-0000-000100000010"))), addBotService = (Id (fromJust (UUID.fromString "0000001e-0000-0003-0000-000600000019"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SL, lCountry = Just (Country {fromCountry = NI})})}
testObject_AddBot_user_17 :: AddBot
testObject_AddBot_user_17 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000e-0000-0009-0000-001900000018"))), addBotService = (Id (fromJust (UUID.fromString "00000006-0000-0020-0000-00070000001a"))), addBotLocale = Nothing}
testObject_AddBot_user_18 :: AddBot
testObject_AddBot_user_18 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000008-0000-000b-0000-00140000001b"))), addBotService = (Id (fromJust (UUID.fromString "00000017-0000-0016-0000-001b00000005"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KU, lCountry = Just (Country {fromCountry = PW})})}
testObject_AddBot_user_19 :: AddBot
testObject_AddBot_user_19 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000003-0000-0009-0000-00080000001c"))), addBotService = (Id (fromJust (UUID.fromString "0000001c-0000-0005-0000-000f00000012"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.EO, lCountry = Just (Country {fromCountry = DJ})})}
testObject_AddBot_user_20 :: AddBot
testObject_AddBot_user_20 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-001500000013"))), addBotService = (Id (fromJust (UUID.fromString "00000010-0000-001d-0000-001e0000001f"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KM, lCountry = Just (Country {fromCountry = BR})})}
