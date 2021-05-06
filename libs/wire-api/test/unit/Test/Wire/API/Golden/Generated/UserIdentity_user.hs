{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserIdentity_user where

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
testObject_UserIdentity_user_1 :: UserIdentity
testObject_UserIdentity_user_1 = SSOIdentity (UserSSOId "" "") (Just (Email {emailLocal = "^\37587\ETX_6", emailDomain = "\1100684|\DC45\DC2M\FS\DEL"})) (Just (Phone {fromPhone = "+7142406883992"}))
testObject_UserIdentity_user_2 :: UserIdentity
testObject_UserIdentity_user_2 = FullIdentity (Email {emailLocal = "", emailDomain = "\136758"}) (Phone {fromPhone = "+1095233678"})
testObject_UserIdentity_user_3 :: UserIdentity
testObject_UserIdentity_user_3 = SSOIdentity (UserSSOId "i\\y\1073978_" "") (Just (Email {emailLocal = "\RS\187083;\133355N", emailDomain = "S\1048806\GS\DEL\GSG\26117S"})) Nothing
testObject_UserIdentity_user_4 :: UserIdentity
testObject_UserIdentity_user_4 = FullIdentity (Email {emailLocal = "\DC2\FS", emailDomain = " C}?R\136349"}) (Phone {fromPhone = "+235393066643"})
testObject_UserIdentity_user_5 :: UserIdentity
testObject_UserIdentity_user_5 = PhoneIdentity (Phone {fromPhone = "+9016614922854"})
testObject_UserIdentity_user_6 :: UserIdentity
testObject_UserIdentity_user_6 = SSOIdentity (UserSSOId "" ".j?") (Just (Email {emailLocal = ".\SYN/\1041492\178935s", emailDomain = "\FS\1010300!m\25569KW`M"})) (Just (Phone {fromPhone = "+3393892222"}))
testObject_UserIdentity_user_7 :: UserIdentity
testObject_UserIdentity_user_7 = FullIdentity (Email {emailLocal = "\1098948\DELHJFY\21631VT%KM\NULk", emailDomain = "\ETB\1013464f-\186120\1089184\&9\169212Q\1052339\23684Z!L?"}) (Phone {fromPhone = "+8091836348994"})
testObject_UserIdentity_user_8 :: UserIdentity
testObject_UserIdentity_user_8 = EmailIdentity (Email {emailLocal = "\FSHL^\992253n%\EMV\1026226j\66407\EOT", emailDomain = "\32323\NAK-b\1012348qVr\12967n ,\SYNf\46479n\178366ui]\ETXL\995880-G8h"})
testObject_UserIdentity_user_9 :: UserIdentity
testObject_UserIdentity_user_9 = SSOIdentity (UserSSOId "#\120231@S" "$GF") (Just (Email {emailLocal = "ORk\ETBL", emailDomain = "\EM]s\985989sm\NUL p"})) (Just (Phone {fromPhone = "+36150969646785"}))
testObject_UserIdentity_user_10 :: UserIdentity
testObject_UserIdentity_user_10 = SSOIdentity (UserSSOId "\158219\1109023%\1106712" "2") (Just (Email {emailLocal = "3\77928\nm\1036526W\993167", emailDomain = " P\1017240U\30688\DLEZ79"})) (Just (Phone {fromPhone = "+44807442967571"}))
testObject_UserIdentity_user_11 :: UserIdentity
testObject_UserIdentity_user_11 = SSOIdentity (UserScimExternalId " +\r\"%") Nothing (Just (Phone {fromPhone = "+8951646295"}))
testObject_UserIdentity_user_12 :: UserIdentity
testObject_UserIdentity_user_12 = PhoneIdentity (Phone {fromPhone = "+580505444024612"})
testObject_UserIdentity_user_13 :: UserIdentity
testObject_UserIdentity_user_13 = EmailIdentity (Email {emailLocal = "\v\1051127 _7}C:\39185gNW`8V\1046915\1013608m", emailDomain = "`,\156535o\ENQb\DC4W\r\ACKL\166688"})
testObject_UserIdentity_user_14 :: UserIdentity
testObject_UserIdentity_user_14 = SSOIdentity (UserSSOId "2t\60482" "\"fa\1028469") (Just (Email {emailLocal = "", emailDomain = ""})) (Just (Phone {fromPhone = "+737238101203"}))
testObject_UserIdentity_user_15 :: UserIdentity
testObject_UserIdentity_user_15 = PhoneIdentity (Phone {fromPhone = "+069964339"})
testObject_UserIdentity_user_16 :: UserIdentity
testObject_UserIdentity_user_16 = SSOIdentity (UserScimExternalId "\1012576u") (Just (Email {emailLocal = "l\16997\1055874", emailDomain = "\ACK\70301^4`ij~"})) (Just (Phone {fromPhone = "+165458270159"}))
testObject_UserIdentity_user_17 :: UserIdentity
testObject_UserIdentity_user_17 = PhoneIdentity (Phone {fromPhone = "+2991135046808"})
testObject_UserIdentity_user_18 :: UserIdentity
testObject_UserIdentity_user_18 = EmailIdentity (Email {emailLocal = " \135383\1035238\r\30093?\v\t\1076757\65877~r?Ta\US\1038144d\DC2Zw\988209\17536\&9*\GS", emailDomain = "\47041;\1054009\1038481\1083622\1039949\&2T\SOH\GSWN\n\"\b\68478\\\STX\CAN`:Xg"})
testObject_UserIdentity_user_19 :: UserIdentity
testObject_UserIdentity_user_19 = FullIdentity (Email {emailLocal = "\ENQ10`Xo7\169637", emailDomain = "T\DC4Y\63231\DC4o>tdaZ\SI\SUB\SI"}) (Phone {fromPhone = "+76662569"})
testObject_UserIdentity_user_20 :: UserIdentity
testObject_UserIdentity_user_20 = PhoneIdentity (Phone {fromPhone = "+77155871011"})
