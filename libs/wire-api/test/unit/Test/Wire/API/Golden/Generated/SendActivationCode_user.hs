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
testObject_SendActivationCode_user_1 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+63320561143468"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.MI, lCountry = Just (Country {fromCountry = CG})}), saCall = False}
testObject_SendActivationCode_user_2 :: SendActivationCode
testObject_SendActivationCode_user_2 = SendActivationCode {saUserKey = Left (Email {emailLocal = ")\994294z\140759\CANCs", emailDomain = "U\1003770\136793R\ESC\1082295=U[\EOT"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.HA, lCountry = Nothing}), saCall = False}
testObject_SendActivationCode_user_3 :: SendActivationCode
testObject_SendActivationCode_user_3 = SendActivationCode {saUserKey = Left (Email {emailLocal = "B:;#\DC2", emailDomain = "t\41116"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.LB, lCountry = Just (Country {fromCountry = MV})}), saCall = False}
testObject_SendActivationCode_user_4 :: SendActivationCode
testObject_SendActivationCode_user_4 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+125377505880"}), saLocale = Nothing, saCall = True}
testObject_SendActivationCode_user_5 :: SendActivationCode
testObject_SendActivationCode_user_5 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+245635341884"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.CY, lCountry = Just (Country {fromCountry = NO})}), saCall = False}
testObject_SendActivationCode_user_6 :: SendActivationCode
testObject_SendActivationCode_user_6 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+2009629180"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.ZU, lCountry = Just (Country {fromCountry = AM})}), saCall = False}
testObject_SendActivationCode_user_7 :: SendActivationCode
testObject_SendActivationCode_user_7 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+226727745026"}), saLocale = Nothing, saCall = False}
testObject_SendActivationCode_user_8 :: SendActivationCode
testObject_SendActivationCode_user_8 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+806771566"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.ET, lCountry = Nothing}), saCall = False}
testObject_SendActivationCode_user_9 :: SendActivationCode
testObject_SendActivationCode_user_9 = SendActivationCode {saUserKey = Left (Email {emailLocal = "y`L\92636\GS", emailDomain = "\33806L1"}), saLocale = Nothing, saCall = False}
testObject_SendActivationCode_user_10 :: SendActivationCode
testObject_SendActivationCode_user_10 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+145145953811659"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.AY, lCountry = Just (Country {fromCountry = GB})}), saCall = False}
testObject_SendActivationCode_user_11 :: SendActivationCode
testObject_SendActivationCode_user_11 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+433927741"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.CU, lCountry = Nothing}), saCall = True}
testObject_SendActivationCode_user_12 :: SendActivationCode
testObject_SendActivationCode_user_12 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+94602443182469"}), saLocale = Nothing, saCall = True}
testObject_SendActivationCode_user_13 :: SendActivationCode
testObject_SendActivationCode_user_13 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+4286391797286"}), saLocale = Nothing, saCall = True}
testObject_SendActivationCode_user_14 :: SendActivationCode
testObject_SendActivationCode_user_14 = SendActivationCode {saUserKey = Left (Email {emailLocal = "\a\1064150\98058b\ETX\FSS\36622", emailDomain = "\139344\1035534\57375\132425\184425(6'N"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TN, lCountry = Just (Country {fromCountry = IE})}), saCall = True}
testObject_SendActivationCode_user_15 :: SendActivationCode
testObject_SendActivationCode_user_15 = SendActivationCode {saUserKey = Left (Email {emailLocal = "\rUB\CAN\a", emailDomain = "\na\152477P\SUB\155394\1003778"}), saLocale = Nothing, saCall = False}
testObject_SendActivationCode_user_16 :: SendActivationCode
testObject_SendActivationCode_user_16 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+043332061"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.LN, lCountry = Nothing}), saCall = False}
testObject_SendActivationCode_user_17 :: SendActivationCode
testObject_SendActivationCode_user_17 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+902344353"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TT, lCountry = Nothing}), saCall = True}
testObject_SendActivationCode_user_18 :: SendActivationCode
testObject_SendActivationCode_user_18 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+59674608"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.II, lCountry = Nothing}), saCall = True}
testObject_SendActivationCode_user_19 :: SendActivationCode
testObject_SendActivationCode_user_19 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+2386056925"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TK, lCountry = Just (Country {fromCountry = MU})}), saCall = False}
testObject_SendActivationCode_user_20 :: SendActivationCode
testObject_SendActivationCode_user_20 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+80262939054085"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.HZ, lCountry = Just (Country {fromCountry = VI})}), saCall = True}
