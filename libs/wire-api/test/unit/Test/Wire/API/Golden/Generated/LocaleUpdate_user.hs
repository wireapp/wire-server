{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LocaleUpdate_user where

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
testObject_LocaleUpdate_user_1 :: LocaleUpdate
testObject_LocaleUpdate_user_1 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.FF, lCountry = Nothing}}
testObject_LocaleUpdate_user_2 :: LocaleUpdate
testObject_LocaleUpdate_user_2 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.HA, lCountry = Just (Country {fromCountry = YT})}}
testObject_LocaleUpdate_user_3 :: LocaleUpdate
testObject_LocaleUpdate_user_3 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.SL, lCountry = Nothing}}
testObject_LocaleUpdate_user_4 :: LocaleUpdate
testObject_LocaleUpdate_user_4 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.RN, lCountry = Nothing}}
testObject_LocaleUpdate_user_5 :: LocaleUpdate
testObject_LocaleUpdate_user_5 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.ST, lCountry = Just (Country {fromCountry = HN})}}
testObject_LocaleUpdate_user_6 :: LocaleUpdate
testObject_LocaleUpdate_user_6 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.KG, lCountry = Nothing}}
testObject_LocaleUpdate_user_7 :: LocaleUpdate
testObject_LocaleUpdate_user_7 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.II, lCountry = Nothing}}
testObject_LocaleUpdate_user_8 :: LocaleUpdate
testObject_LocaleUpdate_user_8 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.BE, lCountry = Just (Country {fromCountry = PW})}}
testObject_LocaleUpdate_user_9 :: LocaleUpdate
testObject_LocaleUpdate_user_9 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.TT, lCountry = Just (Country {fromCountry = MU})}}
testObject_LocaleUpdate_user_10 :: LocaleUpdate
testObject_LocaleUpdate_user_10 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.BH, lCountry = Nothing}}
testObject_LocaleUpdate_user_11 :: LocaleUpdate
testObject_LocaleUpdate_user_11 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.KK, lCountry = Just (Country {fromCountry = SC})}}
testObject_LocaleUpdate_user_12 :: LocaleUpdate
testObject_LocaleUpdate_user_12 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.KJ, lCountry = Nothing}}
testObject_LocaleUpdate_user_13 :: LocaleUpdate
testObject_LocaleUpdate_user_13 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.HI, lCountry = Nothing}}
testObject_LocaleUpdate_user_14 :: LocaleUpdate
testObject_LocaleUpdate_user_14 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.MG, lCountry = Just (Country {fromCountry = LC})}}
testObject_LocaleUpdate_user_15 :: LocaleUpdate
testObject_LocaleUpdate_user_15 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.LO, lCountry = Nothing}}
testObject_LocaleUpdate_user_16 :: LocaleUpdate
testObject_LocaleUpdate_user_16 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.YI, lCountry = Nothing}}
testObject_LocaleUpdate_user_17 :: LocaleUpdate
testObject_LocaleUpdate_user_17 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.MS, lCountry = Just (Country {fromCountry = MS})}}
testObject_LocaleUpdate_user_18 :: LocaleUpdate
testObject_LocaleUpdate_user_18 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.SW, lCountry = Just (Country {fromCountry = FJ})}}
testObject_LocaleUpdate_user_19 :: LocaleUpdate
testObject_LocaleUpdate_user_19 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.EL, lCountry = Just (Country {fromCountry = GA})}}
testObject_LocaleUpdate_user_20 :: LocaleUpdate
testObject_LocaleUpdate_user_20 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.FY, lCountry = Just (Country {fromCountry = MQ})}}
