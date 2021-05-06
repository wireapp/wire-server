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
testObject_EmailUpdate_user_1 = EmailUpdate {euEmail = Email {emailLocal = "jq3B#\16229]\a\1083062\&6D\DC3\1003121b\r)\137319w\GS^\SIW>\DC1\985298\t\92222", emailDomain = "l\1062501 j>A\DC1\STX\ESCd-6^\1015823|vR\FS|m\DLE\5512O8q\n\1034149="}}
testObject_EmailUpdate_user_2 :: EmailUpdate
testObject_EmailUpdate_user_2 = EmailUpdate {euEmail = Email {emailLocal = "\996542\SI\144403\19796\FS", emailDomain = "}2l|JmH\14017\30406\1078670C"}}
testObject_EmailUpdate_user_3 :: EmailUpdate
testObject_EmailUpdate_user_3 = EmailUpdate {euEmail = Email {emailLocal = "", emailDomain = "Z\61147+\1036821\997314S{9\29960-C"}}
testObject_EmailUpdate_user_4 :: EmailUpdate
testObject_EmailUpdate_user_4 = EmailUpdate {euEmail = Email {emailLocal = "\1044080\\v}", emailDomain = "O\67142`\1097960\ETB\180031\ETBl"}}
testObject_EmailUpdate_user_5 :: EmailUpdate
testObject_EmailUpdate_user_5 = EmailUpdate {euEmail = Email {emailLocal = "'y\v\60341\&3\n\182312$\2608no\ACKUR\a\SIp9)\ESCP3L", emailDomain = "+\144240\1001377NU \1004230bU!a\48413"}}
testObject_EmailUpdate_user_6 :: EmailUpdate
testObject_EmailUpdate_user_6 = EmailUpdate {euEmail = Email {emailLocal = "le\f\ETB7\CAN>\FSP\DC2\150948TU\1108860\145667 .N\1064997\DC2&P\STX%\1097856", emailDomain = "S\CAN\FS\99445\1067346\5989oJ\US%0\993120\1084334\43486\1048202\STXn\1061373y\983874"}}
testObject_EmailUpdate_user_7 :: EmailUpdate
testObject_EmailUpdate_user_7 = EmailUpdate {euEmail = Email {emailLocal = "FSIE\158615\&4\1032097\r\STX=:Ud*\ESC2\996002\EM=_d\r=\DC2\126107", emailDomain = "^c\42486e\US.h\179807\1102134\DC1O<%\161144zkC\93982\ENQ{re\128041\NUL\94253/\180319%"}}
testObject_EmailUpdate_user_8 :: EmailUpdate
testObject_EmailUpdate_user_8 = EmailUpdate {euEmail = Email {emailLocal = "\GS5\"jX\142396fHV\1102764\1023770\SOV>W~\EM\FS", emailDomain = "u0,)O\DC1c\b]QAt7z\ENQY[\164597\983090\DC3\CANq\187268M\SOH\189672\1020178\63327(\b"}}
testObject_EmailUpdate_user_9 :: EmailUpdate
testObject_EmailUpdate_user_9 = EmailUpdate {euEmail = Email {emailLocal = "69\92782-qx\r\189502a\1076422\&7\1043787\1018329\v|\t>\b\6549OC`60\USa", emailDomain = "[x\1065231\70059\1071014V\DC11\n\17193i\1041409\1105434\1105029`LZ]\997750Gm"}}
testObject_EmailUpdate_user_10 :: EmailUpdate
testObject_EmailUpdate_user_10 = EmailUpdate {euEmail = Email {emailLocal = "q\EM1w\NUL\ESCL\51278\&6(_R\ETB_\60750\\04]", emailDomain = ""}}
testObject_EmailUpdate_user_11 :: EmailUpdate
testObject_EmailUpdate_user_11 = EmailUpdate {euEmail = Email {emailLocal = "J\rw\402*3h", emailDomain = "\22888\SI^'r\DC1\92411\\k\n}\\}\DC2\b"}}
testObject_EmailUpdate_user_12 :: EmailUpdate
testObject_EmailUpdate_user_12 = EmailUpdate {euEmail = Email {emailLocal = ">\44873;*\v%#^)\SOH\\\98537*s<,~$U", emailDomain = "O\t\tJez\1052488g0H\1035142\b\1037587 "}}
testObject_EmailUpdate_user_13 :: EmailUpdate
testObject_EmailUpdate_user_13 = EmailUpdate {euEmail = Email {emailLocal = "3K\SUB\ESC\1071071D\SOr\1049298", emailDomain = "\GS\59397"}}
testObject_EmailUpdate_user_14 :: EmailUpdate
testObject_EmailUpdate_user_14 = EmailUpdate {euEmail = Email {emailLocal = "K<\1001818r\DC34C=x\1002738\ESCP\DC4\STX[B\166600\50264z\ETX\v", emailDomain = "\n\52783\57478\1084634\100168}\SUBB\41129\&4+T1aFnK]\ESCqB\1071673\&0\31177 \99533\&9v"}}
testObject_EmailUpdate_user_15 :: EmailUpdate
testObject_EmailUpdate_user_15 = EmailUpdate {euEmail = Email {emailLocal = "\1111546\74116\24451\CAN", emailDomain = "\99273\STX\154665M\147905f\1020006"}}
testObject_EmailUpdate_user_16 :: EmailUpdate
testObject_EmailUpdate_user_16 = EmailUpdate {euEmail = Email {emailLocal = "\1094691\146126\SUB$+K\1069119\SO\134734\160757cx", emailDomain = "\FSY"}}
testObject_EmailUpdate_user_17 :: EmailUpdate
testObject_EmailUpdate_user_17 = EmailUpdate {euEmail = Email {emailLocal = "\CAN\t\NAK\a", emailDomain = "\1077914j%\1014560t"}}
testObject_EmailUpdate_user_18 :: EmailUpdate
testObject_EmailUpdate_user_18 = EmailUpdate {euEmail = Email {emailLocal = "o\ETB\63115uN\1030617N+\1102345\nd\SUB\164150\14095\GS+0\10762\189840\SI\1073692\137011\1041472(", emailDomain = "s\SI\11910n\CAN\\3\125122Sg\157258xE\38452+O~}[\STX"}}
testObject_EmailUpdate_user_19 :: EmailUpdate
testObject_EmailUpdate_user_19 = EmailUpdate {euEmail = Email {emailLocal = "d#r\1058655\ETX\182712AN\ETB{!S$\187713\DC3.E\"|\"Inqg\171361", emailDomain = "\17192Zd\22345\157379\&6zao;\t\1075794\28259\nxU9\1097666\1044074"}}
testObject_EmailUpdate_user_20 :: EmailUpdate
testObject_EmailUpdate_user_20 = EmailUpdate {euEmail = Email {emailLocal = "i\119974hA", emailDomain = "<\7992\ETX\1055887\&3\SI\DC2\668\STXt\1038308s\6480\GSS\ESCZ"}}
