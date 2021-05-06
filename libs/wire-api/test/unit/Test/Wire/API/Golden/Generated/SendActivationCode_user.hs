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
testObject_SendActivationCode_user_1 = SendActivationCode {saUserKey = Left (Email {emailLocal = "f]\986253Ww", emailDomain = ""}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SI, lCountry = Nothing}), saCall = True}
testObject_SendActivationCode_user_2 :: SendActivationCode
testObject_SendActivationCode_user_2 = SendActivationCode {saUserKey = Left (Email {emailLocal = "\1052515\4736\74459", emailDomain = "\1036769dI=q\\E{\1086417"}), saLocale = Nothing, saCall = False}
testObject_SendActivationCode_user_3 :: SendActivationCode
testObject_SendActivationCode_user_3 = SendActivationCode {saUserKey = Left (Email {emailLocal = "`\62099\126626f\46415", emailDomain = ")\178547"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.PT, lCountry = Just (Country {fromCountry = US})}), saCall = True}
testObject_SendActivationCode_user_4 :: SendActivationCode
testObject_SendActivationCode_user_4 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+27094661639"}), saLocale = Nothing, saCall = True}
testObject_SendActivationCode_user_5 :: SendActivationCode
testObject_SendActivationCode_user_5 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+38337480"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TS, lCountry = Nothing}), saCall = True}
testObject_SendActivationCode_user_6 :: SendActivationCode
testObject_SendActivationCode_user_6 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+837338698"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.PL, lCountry = Just (Country {fromCountry = MX})}), saCall = True}
testObject_SendActivationCode_user_7 :: SendActivationCode
testObject_SendActivationCode_user_7 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+0831355478058"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.FA, lCountry = Nothing}), saCall = False}
testObject_SendActivationCode_user_8 :: SendActivationCode
testObject_SendActivationCode_user_8 = SendActivationCode {saUserKey = Left (Email {emailLocal = "", emailDomain = "\t\1076984\DC2"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.IS, lCountry = Nothing}), saCall = False}
testObject_SendActivationCode_user_9 :: SendActivationCode
testObject_SendActivationCode_user_9 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+104946789195827"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.FJ, lCountry = Just (Country {fromCountry = KR})}), saCall = False}
testObject_SendActivationCode_user_10 :: SendActivationCode
testObject_SendActivationCode_user_10 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+7322577070"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TK, lCountry = Just (Country {fromCountry = PA})}), saCall = False}
testObject_SendActivationCode_user_11 :: SendActivationCode
testObject_SendActivationCode_user_11 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+03572858758"}), saLocale = Nothing, saCall = False}
testObject_SendActivationCode_user_12 :: SendActivationCode
testObject_SendActivationCode_user_12 = SendActivationCode {saUserKey = Left (Email {emailLocal = "\a\47029\&1_", emailDomain = "\b7\46317\38092p~N\997472Ys"}), saLocale = Nothing, saCall = False}
testObject_SendActivationCode_user_13 :: SendActivationCode
testObject_SendActivationCode_user_13 = SendActivationCode {saUserKey = Left (Email {emailLocal = "", emailDomain = "\190085R\DC1\SUB"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.CS, lCountry = Nothing}), saCall = False}
testObject_SendActivationCode_user_14 :: SendActivationCode
testObject_SendActivationCode_user_14 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+84058579988133"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KM, lCountry = Just (Country {fromCountry = CF})}), saCall = True}
testObject_SendActivationCode_user_15 :: SendActivationCode
testObject_SendActivationCode_user_15 = SendActivationCode {saUserKey = Left (Email {emailLocal = "\SO\CAN\38073Rq\ETB\1081853\CAN\1017406", emailDomain = "\CANiefj.\1098575\68743\NAK"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.MI, lCountry = Just (Country {fromCountry = GE})}), saCall = True}
testObject_SendActivationCode_user_16 :: SendActivationCode
testObject_SendActivationCode_user_16 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+69678408571459"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TA, lCountry = Nothing}), saCall = False}
testObject_SendActivationCode_user_17 :: SendActivationCode
testObject_SendActivationCode_user_17 = SendActivationCode {saUserKey = Left (Email {emailLocal = "p\22769:\46504", emailDomain = "RP\1034433s\nkKo>"}), saLocale = Nothing, saCall = True}
testObject_SendActivationCode_user_18 :: SendActivationCode
testObject_SendActivationCode_user_18 = SendActivationCode {saUserKey = Left (Email {emailLocal = "o(L", emailDomain = "\111240K`.\ETX6d\DEL"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.NB, lCountry = Just (Country {fromCountry = BS})}), saCall = True}
testObject_SendActivationCode_user_19 :: SendActivationCode
testObject_SendActivationCode_user_19 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+3603452156"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.CA, lCountry = Nothing}), saCall = True}
testObject_SendActivationCode_user_20 :: SendActivationCode
testObject_SendActivationCode_user_20 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+64445166393"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.HR, lCountry = Just (Country {fromCountry = GQ})}), saCall = True}
