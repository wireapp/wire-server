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
testObject_ActivationResponse_user_1 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "On\ETX4CqK\STX\1053703nr", emailDomain = ",\1081598\\\1087313De\NULfY"}), activatedFirst = True}
testObject_ActivationResponse_user_2 :: ActivationResponse
testObject_ActivationResponse_user_2 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+600021439525"}), activatedFirst = False}
testObject_ActivationResponse_user_3 :: ActivationResponse
testObject_ActivationResponse_user_3 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+77200397858"}), activatedFirst = False}
testObject_ActivationResponse_user_4 :: ActivationResponse
testObject_ActivationResponse_user_4 = ActivationResponse {activatedIdentity = SSOIdentity (UserScimExternalId "k") (Just (Email {emailLocal = "Ed\152740\181174", emailDomain = ""})) (Just (Phone {fromPhone = "+91607294647241"})), activatedFirst = True}
testObject_ActivationResponse_user_5 :: ActivationResponse
testObject_ActivationResponse_user_5 = ActivationResponse {activatedIdentity = SSOIdentity (UserScimExternalId "T") (Just (Email {emailLocal = "3", emailDomain = ""})) Nothing, activatedFirst = True}
testObject_ActivationResponse_user_6 :: ActivationResponse
testObject_ActivationResponse_user_6 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+426661748502098"}), activatedFirst = False}
testObject_ActivationResponse_user_7 :: ActivationResponse
testObject_ActivationResponse_user_7 = ActivationResponse {activatedIdentity = SSOIdentity (UserScimExternalId "'") (Just (Email {emailLocal = "", emailDomain = "k"})) (Just (Phone {fromPhone = "+46248188"})), activatedFirst = False}
testObject_ActivationResponse_user_8 :: ActivationResponse
testObject_ActivationResponse_user_8 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "W\21158\DLE3\\n", emailDomain = "\984946k\188578"}) (Phone {fromPhone = "+398274740"}), activatedFirst = True}
testObject_ActivationResponse_user_9 :: ActivationResponse
testObject_ActivationResponse_user_9 = ActivationResponse {activatedIdentity = SSOIdentity (UserSSOId ";" "\"O") Nothing (Just (Phone {fromPhone = "+87939792573581"})), activatedFirst = False}
testObject_ActivationResponse_user_10 :: ActivationResponse
testObject_ActivationResponse_user_10 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "\169331V\1078575", emailDomain = "B\1091633s+\CANES<?\ETX\fd\59164\43504"}), activatedFirst = False}
testObject_ActivationResponse_user_11 :: ActivationResponse
testObject_ActivationResponse_user_11 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "%\1075958y$\ETB;F<\b\1071249", emailDomain = "F\SOHV\EMdR\999748yY\991360\34536(\aP"}), activatedFirst = True}
testObject_ActivationResponse_user_12 :: ActivationResponse
testObject_ActivationResponse_user_12 = ActivationResponse {activatedIdentity = SSOIdentity (UserScimExternalId "") (Just (Email {emailLocal = "'\144723j0F", emailDomain = ""})) (Just (Phone {fromPhone = "+31501621607027"})), activatedFirst = True}
testObject_ActivationResponse_user_13 :: ActivationResponse
testObject_ActivationResponse_user_13 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "T", emailDomain = "9T\987831Tv5\1050191sp\1110457\GS#<%"}), activatedFirst = True}
testObject_ActivationResponse_user_14 :: ActivationResponse
testObject_ActivationResponse_user_14 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "_,z", emailDomain = "w"}), activatedFirst = False}
testObject_ActivationResponse_user_15 :: ActivationResponse
testObject_ActivationResponse_user_15 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = ")\1101610\DLE\NAKK\146959h\181508D&Z\v\1045334", emailDomain = "\1108723]<F"}), activatedFirst = True}
testObject_ActivationResponse_user_16 :: ActivationResponse
testObject_ActivationResponse_user_16 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+124048211352"}), activatedFirst = True}
testObject_ActivationResponse_user_17 :: ActivationResponse
testObject_ActivationResponse_user_17 = ActivationResponse {activatedIdentity = SSOIdentity (UserScimExternalId "\14710\nH\1110341") (Just (Email {emailLocal = "4\ETX\r\EOT", emailDomain = "\FS5"})) (Just (Phone {fromPhone = "+921583161970921"})), activatedFirst = False}
testObject_ActivationResponse_user_18 :: ActivationResponse
testObject_ActivationResponse_user_18 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "r`", emailDomain = "\FS*\993290"}), activatedFirst = False}
testObject_ActivationResponse_user_19 :: ActivationResponse
testObject_ActivationResponse_user_19 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "\DC19\DLE\DC11Y\n\US", emailDomain = "\b\EM\175861."}), activatedFirst = False}
testObject_ActivationResponse_user_20 :: ActivationResponse
testObject_ActivationResponse_user_20 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "3K;to];", emailDomain = "&0\188733r"}) (Phone {fromPhone = "+58379698"}), activatedFirst = False}
