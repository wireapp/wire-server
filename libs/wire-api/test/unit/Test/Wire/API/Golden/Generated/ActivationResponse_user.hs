{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ActivationResponse_user where

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
testObject_ActivationResponse_user_1 :: ActivationResponse
testObject_ActivationResponse_user_1 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+5181792146"}), activatedFirst = False}
testObject_ActivationResponse_user_2 :: ActivationResponse
testObject_ActivationResponse_user_2 = ActivationResponse {activatedIdentity = SSOIdentity (UserScimExternalId "s") (Just (Email {emailLocal = "\1079235", emailDomain = "\DC1"})) (Just (Phone {fromPhone = "+8722508725776"})), activatedFirst = True}
testObject_ActivationResponse_user_3 :: ActivationResponse
testObject_ActivationResponse_user_3 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+3181926327217"}), activatedFirst = True}
testObject_ActivationResponse_user_4 :: ActivationResponse
testObject_ActivationResponse_user_4 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "C'\ESCZyxN", emailDomain = "\1015123\&4\SOH\EMK\28839}\1066243.Cp#z"}), activatedFirst = False}
testObject_ActivationResponse_user_5 :: ActivationResponse
testObject_ActivationResponse_user_5 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+1053189758"}), activatedFirst = True}
testObject_ActivationResponse_user_6 :: ActivationResponse
testObject_ActivationResponse_user_6 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "\984828~nk\r#\20356\101064\1027929pz", emailDomain = ""}), activatedFirst = False}
testObject_ActivationResponse_user_7 :: ActivationResponse
testObject_ActivationResponse_user_7 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "", emailDomain = "ee"}) (Phone {fromPhone = "+56507391"}), activatedFirst = False}
testObject_ActivationResponse_user_8 :: ActivationResponse
testObject_ActivationResponse_user_8 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = ":)\6772", emailDomain = "\\\1075676\71738}\\"}) (Phone {fromPhone = "+09778095991625"}), activatedFirst = True}
testObject_ActivationResponse_user_9 :: ActivationResponse
testObject_ActivationResponse_user_9 = ActivationResponse {activatedIdentity = SSOIdentity (UserScimExternalId "") Nothing Nothing, activatedFirst = True}
testObject_ActivationResponse_user_10 :: ActivationResponse
testObject_ActivationResponse_user_10 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+7813975372298"}), activatedFirst = True}
testObject_ActivationResponse_user_11 :: ActivationResponse
testObject_ActivationResponse_user_11 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+74854763153859"}), activatedFirst = True}
testObject_ActivationResponse_user_12 :: ActivationResponse
testObject_ActivationResponse_user_12 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "/fN\ENQR!\1106348Dz\\6", emailDomain = "\DC3\b\155385\135533S)\a\USB\178491%Y"}), activatedFirst = True}
testObject_ActivationResponse_user_13 :: ActivationResponse
testObject_ActivationResponse_user_13 = ActivationResponse {activatedIdentity = SSOIdentity (UserSSOId "\28630\1028966" "m\DC1") Nothing (Just (Phone {fromPhone = "+35419592308"})), activatedFirst = False}
testObject_ActivationResponse_user_14 :: ActivationResponse
testObject_ActivationResponse_user_14 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "4F\1037863'?", emailDomain = "\SYN\1047433\NAK\1077563\1111339"}), activatedFirst = True}
testObject_ActivationResponse_user_15 :: ActivationResponse
testObject_ActivationResponse_user_15 = ActivationResponse {activatedIdentity = SSOIdentity (UserSSOId "" "\151051") (Just (Email {emailLocal = "R", emailDomain = "h\DC3"})) (Just (Phone {fromPhone = "+200065412236863"})), activatedFirst = True}
testObject_ActivationResponse_user_16 :: ActivationResponse
testObject_ActivationResponse_user_16 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "", emailDomain = "\1043019\EOT"}), activatedFirst = True}
testObject_ActivationResponse_user_17 :: ActivationResponse
testObject_ActivationResponse_user_17 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+70901011"}), activatedFirst = True}
testObject_ActivationResponse_user_18 :: ActivationResponse
testObject_ActivationResponse_user_18 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "8", emailDomain = "\97239\49914R\3296Lo"}) (Phone {fromPhone = "+785512330"}), activatedFirst = True}
testObject_ActivationResponse_user_19 :: ActivationResponse
testObject_ActivationResponse_user_19 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "\166945o\CANHt\DC4C", emailDomain = "%,\DC1JNM"}) (Phone {fromPhone = "+354167698579"}), activatedFirst = False}
testObject_ActivationResponse_user_20 :: ActivationResponse
testObject_ActivationResponse_user_20 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "L\58926p\EM\STX\1047357\190544C", emailDomain = ""}), activatedFirst = True}
