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
testObject_Locale_user_1 = Locale {lLanguage = Language Data.LanguageCodes.IK, lCountry = Just (Country {fromCountry = BZ})}
testObject_Locale_user_2 :: Locale
testObject_Locale_user_2 = Locale {lLanguage = Language Data.LanguageCodes.TE, lCountry = Just (Country {fromCountry = IN})}
testObject_Locale_user_3 :: Locale
testObject_Locale_user_3 = Locale {lLanguage = Language Data.LanguageCodes.RM, lCountry = Just (Country {fromCountry = NU})}
testObject_Locale_user_4 :: Locale
testObject_Locale_user_4 = Locale {lLanguage = Language Data.LanguageCodes.EE, lCountry = Just (Country {fromCountry = MK})}
testObject_Locale_user_5 :: Locale
testObject_Locale_user_5 = Locale {lLanguage = Language Data.LanguageCodes.HE, lCountry = Just (Country {fromCountry = ZW})}
testObject_Locale_user_6 :: Locale
testObject_Locale_user_6 = Locale {lLanguage = Language Data.LanguageCodes.MG, lCountry = Just (Country {fromCountry = WS})}
testObject_Locale_user_7 :: Locale
testObject_Locale_user_7 = Locale {lLanguage = Language Data.LanguageCodes.NE, lCountry = Just (Country {fromCountry = AD})}
testObject_Locale_user_8 :: Locale
testObject_Locale_user_8 = Locale {lLanguage = Language Data.LanguageCodes.LG, lCountry = Just (Country {fromCountry = PY})}
testObject_Locale_user_9 :: Locale
testObject_Locale_user_9 = Locale {lLanguage = Language Data.LanguageCodes.AA, lCountry = Just (Country {fromCountry = HM})}
testObject_Locale_user_10 :: Locale
testObject_Locale_user_10 = Locale {lLanguage = Language Data.LanguageCodes.MI, lCountry = Just (Country {fromCountry = HT})}
testObject_Locale_user_11 :: Locale
testObject_Locale_user_11 = Locale {lLanguage = Language Data.LanguageCodes.RO, lCountry = Just (Country {fromCountry = DO})}
testObject_Locale_user_12 :: Locale
testObject_Locale_user_12 = Locale {lLanguage = Language Data.LanguageCodes.NV, lCountry = Nothing}
testObject_Locale_user_13 :: Locale
testObject_Locale_user_13 = Locale {lLanguage = Language Data.LanguageCodes.UR, lCountry = Just (Country {fromCountry = AS})}
testObject_Locale_user_14 :: Locale
testObject_Locale_user_14 = Locale {lLanguage = Language Data.LanguageCodes.RW, lCountry = Just (Country {fromCountry = RS})}
testObject_Locale_user_15 :: Locale
testObject_Locale_user_15 = Locale {lLanguage = Language Data.LanguageCodes.OM, lCountry = Just (Country {fromCountry = YE})}
testObject_Locale_user_16 :: Locale
testObject_Locale_user_16 = Locale {lLanguage = Language Data.LanguageCodes.FR, lCountry = Just (Country {fromCountry = PY})}
testObject_Locale_user_17 :: Locale
testObject_Locale_user_17 = Locale {lLanguage = Language Data.LanguageCodes.IG, lCountry = Just (Country {fromCountry = KM})}
testObject_Locale_user_18 :: Locale
testObject_Locale_user_18 = Locale {lLanguage = Language Data.LanguageCodes.SS, lCountry = Just (Country {fromCountry = SH})}
testObject_Locale_user_19 :: Locale
testObject_Locale_user_19 = Locale {lLanguage = Language Data.LanguageCodes.AF, lCountry = Nothing}
testObject_Locale_user_20 :: Locale
testObject_Locale_user_20 = Locale {lLanguage = Language Data.LanguageCodes.KL, lCountry = Just (Country {fromCountry = PM})}
