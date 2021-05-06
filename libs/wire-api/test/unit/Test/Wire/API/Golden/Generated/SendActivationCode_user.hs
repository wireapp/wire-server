{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.SendActivationCode_user where

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
testObject_SendActivationCode_user_1 :: SendActivationCode
testObject_SendActivationCode_user_1 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+95788888416687"}), saLocale = Nothing, saCall = False}
testObject_SendActivationCode_user_2 :: SendActivationCode
testObject_SendActivationCode_user_2 = SendActivationCode {saUserKey = Left (Email {emailLocal = "c\1083509\1080887xe", emailDomain = "X?"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.AF, lCountry = Just (Country {fromCountry = PE})}), saCall = False}
testObject_SendActivationCode_user_3 :: SendActivationCode
testObject_SendActivationCode_user_3 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+6361151888"}), saLocale = Nothing, saCall = True}
testObject_SendActivationCode_user_4 :: SendActivationCode
testObject_SendActivationCode_user_4 = SendActivationCode {saUserKey = Left (Email {emailLocal = "\31579\172084pMD\4505\&7", emailDomain = "v"}), saLocale = Nothing, saCall = True}
testObject_SendActivationCode_user_5 :: SendActivationCode
testObject_SendActivationCode_user_5 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+08718378421181"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.ES, lCountry = Just (Country {fromCountry = MO})}), saCall = True}
testObject_SendActivationCode_user_6 :: SendActivationCode
testObject_SendActivationCode_user_6 = SendActivationCode {saUserKey = Left (Email {emailLocal = "", emailDomain = "\1074262\999869RQ\147682g\40382O"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SQ, lCountry = Just (Country {fromCountry = LU})}), saCall = True}
testObject_SendActivationCode_user_7 :: SendActivationCode
testObject_SendActivationCode_user_7 = SendActivationCode {saUserKey = Left (Email {emailLocal = "YQ\7808l\STX\r\US\120003\t2", emailDomain = "g\"\169548\fJw]B("}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.EE, lCountry = Just (Country {fromCountry = MF})}), saCall = False}
testObject_SendActivationCode_user_8 :: SendActivationCode
testObject_SendActivationCode_user_8 = SendActivationCode {saUserKey = Left (Email {emailLocal = "\fI;\ACKxu", emailDomain = "w\1069946&t\\J\126704{"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.UK, lCountry = Just (Country {fromCountry = RW})}), saCall = False}
testObject_SendActivationCode_user_9 :: SendActivationCode
testObject_SendActivationCode_user_9 = SendActivationCode {saUserKey = Left (Email {emailLocal = "\DC2", emailDomain = "V\DC2\186343\n\EOTo"}), saLocale = Nothing, saCall = True}
testObject_SendActivationCode_user_10 :: SendActivationCode
testObject_SendActivationCode_user_10 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+9998398091"}), saLocale = Nothing, saCall = True}
testObject_SendActivationCode_user_11 :: SendActivationCode
testObject_SendActivationCode_user_11 = SendActivationCode {saUserKey = Left (Email {emailLocal = "'\RS\1063469\120345", emailDomain = "FX-\ACK\SOH"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.CH, lCountry = Just (Country {fromCountry = BQ})}), saCall = False}
testObject_SendActivationCode_user_12 :: SendActivationCode
testObject_SendActivationCode_user_12 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+47317166"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.IT, lCountry = Just (Country {fromCountry = LA})}), saCall = True}
testObject_SendActivationCode_user_13 :: SendActivationCode
testObject_SendActivationCode_user_13 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+78850281"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SS, lCountry = Just (Country {fromCountry = MT})}), saCall = True}
testObject_SendActivationCode_user_14 :: SendActivationCode
testObject_SendActivationCode_user_14 = SendActivationCode {saUserKey = Left (Email {emailLocal = "\170799t/X\1051667a\1107343", emailDomain = "7#"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.GD, lCountry = Nothing}), saCall = False}
testObject_SendActivationCode_user_15 :: SendActivationCode
testObject_SendActivationCode_user_15 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+34750553"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.LT, lCountry = Just (Country {fromCountry = HK})}), saCall = False}
testObject_SendActivationCode_user_16 :: SendActivationCode
testObject_SendActivationCode_user_16 = SendActivationCode {saUserKey = Left (Email {emailLocal = "k\DC3'\ETB\1100707\175532\1029033\147022\47751", emailDomain = "\ENQ)\174928"}), saLocale = Nothing, saCall = False}
testObject_SendActivationCode_user_17 :: SendActivationCode
testObject_SendActivationCode_user_17 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+48118479508118"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.MK, lCountry = Just (Country {fromCountry = GN})}), saCall = False}
testObject_SendActivationCode_user_18 :: SendActivationCode
testObject_SendActivationCode_user_18 = SendActivationCode {saUserKey = Left (Email {emailLocal = "4\EM\172322K\73102/", emailDomain = "'"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.IA, lCountry = Just (Country {fromCountry = MO})}), saCall = True}
testObject_SendActivationCode_user_19 :: SendActivationCode
testObject_SendActivationCode_user_19 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+3783261124900"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.UG, lCountry = Just (Country {fromCountry = VG})}), saCall = False}
testObject_SendActivationCode_user_20 :: SendActivationCode
testObject_SendActivationCode_user_20 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+24540326712"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TN, lCountry = Nothing}), saCall = True}
