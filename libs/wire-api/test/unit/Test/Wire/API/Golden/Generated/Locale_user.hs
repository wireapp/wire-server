{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Locale_user where

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
testObject_Locale_user_1 :: Locale
testObject_Locale_user_1 = Locale {lLanguage = Language Data.LanguageCodes.GU, lCountry = Just (Country {fromCountry = OM})}
testObject_Locale_user_2 :: Locale
testObject_Locale_user_2 = Locale {lLanguage = Language Data.LanguageCodes.KM, lCountry = Just (Country {fromCountry = BM})}
testObject_Locale_user_3 :: Locale
testObject_Locale_user_3 = Locale {lLanguage = Language Data.LanguageCodes.DA, lCountry = Just (Country {fromCountry = SG})}
testObject_Locale_user_4 :: Locale
testObject_Locale_user_4 = Locale {lLanguage = Language Data.LanguageCodes.KG, lCountry = Just (Country {fromCountry = NI})}
testObject_Locale_user_5 :: Locale
testObject_Locale_user_5 = Locale {lLanguage = Language Data.LanguageCodes.FO, lCountry = Just (Country {fromCountry = VI})}
testObject_Locale_user_6 :: Locale
testObject_Locale_user_6 = Locale {lLanguage = Language Data.LanguageCodes.EU, lCountry = Nothing}
testObject_Locale_user_7 :: Locale
testObject_Locale_user_7 = Locale {lLanguage = Language Data.LanguageCodes.LB, lCountry = Just (Country {fromCountry = GI})}
testObject_Locale_user_8 :: Locale
testObject_Locale_user_8 = Locale {lLanguage = Language Data.LanguageCodes.TN, lCountry = Just (Country {fromCountry = SY})}
testObject_Locale_user_9 :: Locale
testObject_Locale_user_9 = Locale {lLanguage = Language Data.LanguageCodes.KS, lCountry = Nothing}
testObject_Locale_user_10 :: Locale
testObject_Locale_user_10 = Locale {lLanguage = Language Data.LanguageCodes.ND, lCountry = Just (Country {fromCountry = US})}
testObject_Locale_user_11 :: Locale
testObject_Locale_user_11 = Locale {lLanguage = Language Data.LanguageCodes.FO, lCountry = Just (Country {fromCountry = SC})}
testObject_Locale_user_12 :: Locale
testObject_Locale_user_12 = Locale {lLanguage = Language Data.LanguageCodes.RO, lCountry = Just (Country {fromCountry = OM})}
testObject_Locale_user_13 :: Locale
testObject_Locale_user_13 = Locale {lLanguage = Language Data.LanguageCodes.KL, lCountry = Nothing}
testObject_Locale_user_14 :: Locale
testObject_Locale_user_14 = Locale {lLanguage = Language Data.LanguageCodes.SM, lCountry = Just (Country {fromCountry = DE})}
testObject_Locale_user_15 :: Locale
testObject_Locale_user_15 = Locale {lLanguage = Language Data.LanguageCodes.GD, lCountry = Nothing}
testObject_Locale_user_16 :: Locale
testObject_Locale_user_16 = Locale {lLanguage = Language Data.LanguageCodes.KS, lCountry = Just (Country {fromCountry = KN})}
testObject_Locale_user_17 :: Locale
testObject_Locale_user_17 = Locale {lLanguage = Language Data.LanguageCodes.GD, lCountry = Just (Country {fromCountry = VA})}
testObject_Locale_user_18 :: Locale
testObject_Locale_user_18 = Locale {lLanguage = Language Data.LanguageCodes.AY, lCountry = Just (Country {fromCountry = ML})}
testObject_Locale_user_19 :: Locale
testObject_Locale_user_19 = Locale {lLanguage = Language Data.LanguageCodes.TY, lCountry = Just (Country {fromCountry = NU})}
testObject_Locale_user_20 :: Locale
testObject_Locale_user_20 = Locale {lLanguage = Language Data.LanguageCodes.SW, lCountry = Just (Country {fromCountry = RS})}
