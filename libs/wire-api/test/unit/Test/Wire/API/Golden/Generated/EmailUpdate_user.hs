{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.EmailUpdate_user where

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
testObject_EmailUpdate_user_1 :: EmailUpdate
testObject_EmailUpdate_user_1 = EmailUpdate {euEmail = Email {emailLocal = "cW\US\vz\EOT", emailDomain = "\133684\FS#\8962b/\CAN~TV\CAN\68368j\CAN&\bC\22999\1092706"}}
testObject_EmailUpdate_user_2 :: EmailUpdate
testObject_EmailUpdate_user_2 = EmailUpdate {euEmail = Email {emailLocal = "\1089588\&4\ESC\174757^:\t\128491>&\SOH\RSI\SYN\ETB\USrji", emailDomain = "\CAN\1069155c3|A\EOTY\1062774\DLE\r`KW\DC2?\tu\15974"}}
testObject_EmailUpdate_user_3 :: EmailUpdate
testObject_EmailUpdate_user_3 = EmailUpdate {euEmail = Email {emailLocal = "k1\nK3\GS\166852\STX\DC4q\1072764n-y\988225`\ENQ\147671\&0\t", emailDomain = "\1016731Z\ETBnU\1092705\&0\DC2ZPbk!~g\136088"}}
testObject_EmailUpdate_user_4 :: EmailUpdate
testObject_EmailUpdate_user_4 = EmailUpdate {euEmail = Email {emailLocal = "", emailDomain = "i\136923\70206i:n\SI\1010544"}}
testObject_EmailUpdate_user_5 :: EmailUpdate
testObject_EmailUpdate_user_5 = EmailUpdate {euEmail = Email {emailLocal = "'\28358F\"\EM{\NAKDF\SO\ETX", emailDomain = "\64527s\f\DC4\r\31434\1062894\ACK\NULt^"}}
testObject_EmailUpdate_user_6 :: EmailUpdate
testObject_EmailUpdate_user_6 = EmailUpdate {euEmail = Email {emailLocal = "\ETBE\DEL\1064070vw\1027399\1090026\ETBh*z[q!\1044159\148240d\137666[\CAN", emailDomain = "\\\189426{1\DC1i|i"}}
testObject_EmailUpdate_user_7 :: EmailUpdate
testObject_EmailUpdate_user_7 = EmailUpdate {euEmail = Email {emailLocal = "\EM#I\987985\&7K.", emailDomain = "<\48897"}}
testObject_EmailUpdate_user_8 :: EmailUpdate
testObject_EmailUpdate_user_8 = EmailUpdate {euEmail = Email {emailLocal = "a\DC4", emailDomain = "6"}}
testObject_EmailUpdate_user_9 :: EmailUpdate
testObject_EmailUpdate_user_9 = EmailUpdate {euEmail = Email {emailLocal = "yj\f>~r\78037B+\163866\31344\NUL\145313c(\168946n\155928", emailDomain = "H"}}
testObject_EmailUpdate_user_10 :: EmailUpdate
testObject_EmailUpdate_user_10 = EmailUpdate {euEmail = Email {emailLocal = "\EM", emailDomain = "+Kn\1008583\118810F\22014\1076039_9X12V-L\NULD~W=Y\166455\ETX\1007495d\DC2\1006926;"}}
testObject_EmailUpdate_user_11 :: EmailUpdate
testObject_EmailUpdate_user_11 = EmailUpdate {euEmail = Email {emailLocal = " \"`\83252M9(\23292(\vu\1105035vx8LYH(\1036688&\f\71464", emailDomain = "6\185003cc\9504dr\"\t #yS\STX\1031599"}}
testObject_EmailUpdate_user_12 :: EmailUpdate
testObject_EmailUpdate_user_12 = EmailUpdate {euEmail = Email {emailLocal = "\SYN\\\RS\1036754Ej[UE;cw ", emailDomain = "\176053\1008105\NUL\35759\ACKrL\1112488 \17984U\ETX2?\1097674"}}
testObject_EmailUpdate_user_13 :: EmailUpdate
testObject_EmailUpdate_user_13 = EmailUpdate {euEmail = Email {emailLocal = "\tV/\RS\159812MIZov\21497\&3=G\1006959ub.", emailDomain = "mm\1026976\\6\GS\68615Q>u\985237\1094158"}}
testObject_EmailUpdate_user_14 :: EmailUpdate
testObject_EmailUpdate_user_14 = EmailUpdate {euEmail = Email {emailLocal = "w*\EOT\1111438tL\171546k7k\27408\DC4+K\1086393\\.Q", emailDomain = "\184740r\127960\163341uC\NAK\1005900\29544\28811%00V\54326\NULTWa\GS"}}
testObject_EmailUpdate_user_15 :: EmailUpdate
testObject_EmailUpdate_user_15 = EmailUpdate {euEmail = Email {emailLocal = "\rB\179925\1023401j\SYN\163657T\ETX\ACKY\23383\&3\SUBb/\EOT\47476\SIrv'A\26366\EOT", emailDomain = "94\v\CANG\US\ACK\1026319\65097S\SO\SO\1007455\DLEb.h\5273z\\'\1024832ZHW\1004997\CAN\132852%"}}
testObject_EmailUpdate_user_16 :: EmailUpdate
testObject_EmailUpdate_user_16 = EmailUpdate {euEmail = Email {emailLocal = "\DLE\1692\46337F^\8776\ESC!F\1042585$\ETBk\1057379x\190306\&5}\1090480\t\DEL+\22228", emailDomain = "B\\}\1052123\ETBH\ESCs\NAK\132743 VT\b\1015829<\CANd\917623=\24532\1110857a\151830'f2"}}
testObject_EmailUpdate_user_17 :: EmailUpdate
testObject_EmailUpdate_user_17 = EmailUpdate {euEmail = Email {emailLocal = "/\NAK", emailDomain = "x\7378%\1080449_"}}
testObject_EmailUpdate_user_18 :: EmailUpdate
testObject_EmailUpdate_user_18 = EmailUpdate {euEmail = Email {emailLocal = "", emailDomain = "\DC2\"qy<"}}
testObject_EmailUpdate_user_19 :: EmailUpdate
testObject_EmailUpdate_user_19 = EmailUpdate {euEmail = Email {emailLocal = "K\SUB", emailDomain = ")\SYN\993853\1048036\1072268\&9_v\1003609+/4"}}
testObject_EmailUpdate_user_20 :: EmailUpdate
testObject_EmailUpdate_user_20 = EmailUpdate {euEmail = Email {emailLocal = "3\ETB\137488U>\EOT\DC1#pv\1035032\54777\r\78808'C\US\1049401dr\STX\20697\139133_~", emailDomain = ""}}
