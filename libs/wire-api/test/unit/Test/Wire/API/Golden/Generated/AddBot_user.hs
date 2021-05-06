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
testObject_AddBot_user_1 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000008-0000-0016-0000-001a00000019"))), addBotService = (Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000100000019"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.AV, lCountry = Just (Country {fromCountry = SY})})}
testObject_AddBot_user_2 :: AddBot
testObject_AddBot_user_2 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000f-0000-0005-0000-001100000012"))), addBotService = (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-001700000011"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.EN, lCountry = Just (Country {fromCountry = PN})})}
testObject_AddBot_user_3 :: AddBot
testObject_AddBot_user_3 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000005-0000-0017-0000-001300000000"))), addBotService = (Id (fromJust (UUID.fromString "0000000d-0000-0011-0000-000400000014"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.NN, lCountry = Nothing})}
testObject_AddBot_user_4 :: AddBot
testObject_AddBot_user_4 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001f-0000-0011-0000-000400000002"))), addBotService = (Id (fromJust (UUID.fromString "00000020-0000-0001-0000-000400000016"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.EU, lCountry = Just (Country {fromCountry = GL})})}
testObject_AddBot_user_5 :: AddBot
testObject_AddBot_user_5 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000008-0000-0014-0000-000f0000000f"))), addBotService = (Id (fromJust (UUID.fromString "0000001d-0000-001c-0000-00100000001e"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.RN, lCountry = Just (Country {fromCountry = VC})})}
testObject_AddBot_user_6 :: AddBot
testObject_AddBot_user_6 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-001500000003"))), addBotService = (Id (fromJust (UUID.fromString "0000000b-0000-001a-0000-000600000016"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.VO, lCountry = Just (Country {fromCountry = KR})})}
testObject_AddBot_user_7 :: AddBot
testObject_AddBot_user_7 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000014-0000-0020-0000-001f00000015"))), addBotService = (Id (fromJust (UUID.fromString "00000001-0000-0010-0000-000a0000000a"))), addBotLocale = Nothing}
testObject_AddBot_user_8 :: AddBot
testObject_AddBot_user_8 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001e-0000-0018-0000-000900000007"))), addBotService = (Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000d0000001a"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SC, lCountry = Just (Country {fromCountry = GE})})}
testObject_AddBot_user_9 :: AddBot
testObject_AddBot_user_9 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000007-0000-0003-0000-001c0000001b"))), addBotService = (Id (fromJust (UUID.fromString "00000014-0000-0005-0000-000300000014"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.DV, lCountry = Just (Country {fromCountry = HM})})}
testObject_AddBot_user_10 :: AddBot
testObject_AddBot_user_10 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000011-0000-0012-0000-001a00000000"))), addBotService = (Id (fromJust (UUID.fromString "00000020-0000-0019-0000-000d00000001"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.FF, lCountry = Just (Country {fromCountry = AL})})}
testObject_AddBot_user_11 :: AddBot
testObject_AddBot_user_11 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000018-0000-000f-0000-00160000000f"))), addBotService = (Id (fromJust (UUID.fromString "00000008-0000-000d-0000-000f0000001d"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.CA, lCountry = Nothing})}
testObject_AddBot_user_12 :: AddBot
testObject_AddBot_user_12 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000b-0000-0007-0000-002000000019"))), addBotService = (Id (fromJust (UUID.fromString "00000010-0000-0006-0000-001600000010"))), addBotLocale = Nothing}
testObject_AddBot_user_13 :: AddBot
testObject_AddBot_user_13 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000005-0000-0016-0000-00050000001e"))), addBotService = (Id (fromJust (UUID.fromString "00000010-0000-000f-0000-000a00000017"))), addBotLocale = Nothing}
testObject_AddBot_user_14 :: AddBot
testObject_AddBot_user_14 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000001-0000-0009-0000-001600000012"))), addBotService = (Id (fromJust (UUID.fromString "00000007-0000-0016-0000-00030000001c"))), addBotLocale = Nothing}
testObject_AddBot_user_15 :: AddBot
testObject_AddBot_user_15 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000004-0000-000a-0000-001b0000001f"))), addBotService = (Id (fromJust (UUID.fromString "00000013-0000-0007-0000-000900000013"))), addBotLocale = Nothing}
testObject_AddBot_user_16 :: AddBot
testObject_AddBot_user_16 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000019-0000-0005-0000-000b00000000"))), addBotService = (Id (fromJust (UUID.fromString "00000011-0000-001f-0000-001400000001"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.WA, lCountry = Just (Country {fromCountry = GN})})}
testObject_AddBot_user_17 :: AddBot
testObject_AddBot_user_17 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000006-0000-0019-0000-001e0000001b"))), addBotService = (Id (fromJust (UUID.fromString "0000001c-0000-001a-0000-000800000015"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.HY, lCountry = Just (Country {fromCountry = SN})})}
testObject_AddBot_user_18 :: AddBot
testObject_AddBot_user_18 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000b-0000-0016-0000-001800000012"))), addBotService = (Id (fromJust (UUID.fromString "00000007-0000-001f-0000-000300000009"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TN, lCountry = Nothing})}
testObject_AddBot_user_19 :: AddBot
testObject_AddBot_user_19 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000e-0000-001f-0000-000c00000008"))), addBotService = (Id (fromJust (UUID.fromString "0000000d-0000-0001-0000-001900000014"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KV, lCountry = Just (Country {fromCountry = JM})})}
testObject_AddBot_user_20 :: AddBot
testObject_AddBot_user_20 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000a-0000-0009-0000-00020000001d"))), addBotService = (Id (fromJust (UUID.fromString "00000000-0000-0014-0000-000d00000013"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.NB, lCountry = Just (Country {fromCountry = SR})})}
