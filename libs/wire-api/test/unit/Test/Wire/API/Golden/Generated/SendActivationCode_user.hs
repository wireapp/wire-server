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
testObject_SendActivationCode_user_1 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+9121298105938"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.IK, lCountry = Just (Country {fromCountry = TJ})}), saCall = False}
testObject_SendActivationCode_user_2 :: SendActivationCode
testObject_SendActivationCode_user_2 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+47815880888545"}), saLocale = Nothing, saCall = True}
testObject_SendActivationCode_user_3 :: SendActivationCode
testObject_SendActivationCode_user_3 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+65679615978993"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.CU, lCountry = Just (Country {fromCountry = GE})}), saCall = False}
testObject_SendActivationCode_user_4 :: SendActivationCode
testObject_SendActivationCode_user_4 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+099858900714612"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KO, lCountry = Just (Country {fromCountry = IE})}), saCall = True}
testObject_SendActivationCode_user_5 :: SendActivationCode
testObject_SendActivationCode_user_5 = SendActivationCode {saUserKey = Left (Email {emailLocal = "Yddtq3", emailDomain = "\"w\771a\ESCl\DLE?\US"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.LU, lCountry = Just (Country {fromCountry = IQ})}), saCall = True}
