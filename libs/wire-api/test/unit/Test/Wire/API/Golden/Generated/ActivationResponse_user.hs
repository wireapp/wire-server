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
testObject_ActivationResponse_user_1 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "\1026781&", emailDomain = " \ENQ"}) (Phone {fromPhone = "+33815166190694"}), activatedFirst = False}
testObject_ActivationResponse_user_2 :: ActivationResponse
testObject_ActivationResponse_user_2 = ActivationResponse {activatedIdentity = SSOIdentity (UserSSOId "\SOH" "\150931\r") (Just (Email {emailLocal = "", emailDomain = ""})) (Just (Phone {fromPhone = "+023554027"})), activatedFirst = False}
testObject_ActivationResponse_user_3 :: ActivationResponse
testObject_ActivationResponse_user_3 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "\SOH*rXI\SOHd", emailDomain = "^B\a\127797O\\"}) (Phone {fromPhone = "+697018921"}), activatedFirst = True}
testObject_ActivationResponse_user_4 :: ActivationResponse
testObject_ActivationResponse_user_4 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "\1055684\ETX", emailDomain = "\38030j"}) (Phone {fromPhone = "+2711629155"}), activatedFirst = True}
testObject_ActivationResponse_user_5 :: ActivationResponse
testObject_ActivationResponse_user_5 = ActivationResponse {activatedIdentity = SSOIdentity (UserSSOId "\ETX" "\ACK\161204") (Just (Email {emailLocal = "#", emailDomain = "-m"})) Nothing, activatedFirst = True}
testObject_ActivationResponse_user_6 :: ActivationResponse
testObject_ActivationResponse_user_6 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "\DEL", emailDomain = "\NAK\65165\26240"}) (Phone {fromPhone = "+2602519139"}), activatedFirst = True}
testObject_ActivationResponse_user_7 :: ActivationResponse
testObject_ActivationResponse_user_7 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = ";\186735", emailDomain = "u\1076589<"}) (Phone {fromPhone = "+4852599358783"}), activatedFirst = False}
testObject_ActivationResponse_user_8 :: ActivationResponse
testObject_ActivationResponse_user_8 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+137251036"}), activatedFirst = False}
testObject_ActivationResponse_user_9 :: ActivationResponse
testObject_ActivationResponse_user_9 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "\rM\1076459", emailDomain = "\101038\ENQgN9_"}) (Phone {fromPhone = "+814046154"}), activatedFirst = True}
testObject_ActivationResponse_user_10 :: ActivationResponse
testObject_ActivationResponse_user_10 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+733851704160"}), activatedFirst = False}
testObject_ActivationResponse_user_11 :: ActivationResponse
testObject_ActivationResponse_user_11 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+4148615693"}), activatedFirst = True}
testObject_ActivationResponse_user_12 :: ActivationResponse
testObject_ActivationResponse_user_12 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "3", emailDomain = "\US\DC1"}), activatedFirst = True}
testObject_ActivationResponse_user_13 :: ActivationResponse
testObject_ActivationResponse_user_13 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "\1096756", emailDomain = "pu\1113267"}) (Phone {fromPhone = "+962884566876"}), activatedFirst = True}
testObject_ActivationResponse_user_14 :: ActivationResponse
testObject_ActivationResponse_user_14 = ActivationResponse {activatedIdentity = SSOIdentity (UserSSOId "K" "") (Just (Email {emailLocal = "6\DC2 \83146", emailDomain = ""})) (Just (Phone {fromPhone = "+352048980282"})), activatedFirst = False}
testObject_ActivationResponse_user_15 :: ActivationResponse
testObject_ActivationResponse_user_15 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "\1001293]\70022\4362\45298XZk\n<,", emailDomain = "\148871"}), activatedFirst = False}
testObject_ActivationResponse_user_16 :: ActivationResponse
testObject_ActivationResponse_user_16 = ActivationResponse {activatedIdentity = SSOIdentity (UserSSOId "" "") (Just (Email {emailLocal = "\ETB%", emailDomain = "3CCj"})) (Just (Phone {fromPhone = "+8782117529"})), activatedFirst = True}
testObject_ActivationResponse_user_17 :: ActivationResponse
testObject_ActivationResponse_user_17 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "\997623\EOTI)\EM\avT1\EOT", emailDomain = "\131483\145664_B"}), activatedFirst = True}
testObject_ActivationResponse_user_18 :: ActivationResponse
testObject_ActivationResponse_user_18 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "\61753=s\f#<\DC3bI", emailDomain = "\DEL31x\EMIa\ETB\t^\RS2\f"}), activatedFirst = False}
testObject_ActivationResponse_user_19 :: ActivationResponse
testObject_ActivationResponse_user_19 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "t", emailDomain = ""}) (Phone {fromPhone = "+32855168457591"}), activatedFirst = True}
testObject_ActivationResponse_user_20 :: ActivationResponse
testObject_ActivationResponse_user_20 = ActivationResponse {activatedIdentity = SSOIdentity (UserSSOId "" "S") Nothing Nothing, activatedFirst = False}
