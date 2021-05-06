{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RichField_user where

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
testObject_RichField_user_1 :: RichField
testObject_RichField_user_1 = RichField {richFieldType = "\152595\35026\NUL\EOTw\1067340s^\94372>>\1110114x_48S\1071248e", richFieldValue = "Q\21568\1032573"}
testObject_RichField_user_2 :: RichField
testObject_RichField_user_2 = RichField {richFieldType = "\1023159", richFieldValue = "D\1043566t\140186"}
testObject_RichField_user_3 :: RichField
testObject_RichField_user_3 = RichField {richFieldType = "\DC2", richFieldValue = "rm\11226\EM\97423'l\1083250\SOH2V\1072547"}
testObject_RichField_user_4 :: RichField
testObject_RichField_user_4 = RichField {richFieldType = "s\1077950\1051318^M\r\1013468[?!?", richFieldValue = "\1098995\68243+o/)%=P\993364\186992\SIp\SUB>p\148135,\ESC5\1094526\"P\CAN\SOH"}
testObject_RichField_user_5 :: RichField
testObject_RichField_user_5 = RichField {richFieldType = ",o\1051125.\ft", richFieldValue = "\1075305[\USwD\\y\997735\&8\GS\r\1087837\164810"}
testObject_RichField_user_6 :: RichField
testObject_RichField_user_6 = RichField {richFieldType = "\DC4n\1090888%\4259\RS\46619\1020573(\6198\&2Zu))&g\DLEB\158375}\133670\1033345P", richFieldValue = "&\USh\SYN\50443?}\CANe\NUL\"y\1104823$2\DLEdi95\SUB\DC2V\993405/\149330"}
testObject_RichField_user_7 :: RichField
testObject_RichField_user_7 = RichField {richFieldType = "#m\185602N8\DLEUOW\USi-\991885e(n", richFieldValue = "\1104976k\STXQOl\16502\DC2}"}
testObject_RichField_user_8 :: RichField
testObject_RichField_user_8 = RichField {richFieldType = "\32223\b\13681\b#_Xa{\988090\NAK\4352\SO&\f\19971W;#bn\173005\40226\a\NULLF\1109011\b", richFieldValue = "\42711ZVL~-"}
testObject_RichField_user_9 :: RichField
testObject_RichField_user_9 = RichField {richFieldType = "<\7653\RS07\b\DEL\1081939\100683\n;t0>7", richFieldValue = "\DLEH%\1028687E0\159512D\f*y\147246\DEL\b\164219\152929%X\984914!\1098089\&8w9\NAK\989919\&3@u"}
testObject_RichField_user_10 :: RichField
testObject_RichField_user_10 = RichField {richFieldType = "\78099\DLE\171468\5076", richFieldValue = " B/4\165204E\1074333\DLE\1094669v\44808=Ju\999651\167957?\EOT7CJV#2m\176339q\20593"}
testObject_RichField_user_11 :: RichField
testObject_RichField_user_11 = RichField {richFieldType = ")\1031258\167833b\175797\161141)Nb\DC10Vc\EOT9\DC28x\54001\1063591Z\STXu\162298\"\SO\96210\n\983710", richFieldValue = "\61018o\96239J=\58164G\1019893V\39059^f7!\11855\vDk\60530\"`kO{z"}
testObject_RichField_user_12 :: RichField
testObject_RichField_user_12 = RichField {richFieldType = "/%\ETX", richFieldValue = "qz\NAK\1051755"}
testObject_RichField_user_13 :: RichField
testObject_RichField_user_13 = RichField {richFieldType = "=", richFieldValue = "+KY, \SOHd\SOq\v,Mq-\1049169@e\992194BUC\1012427U\v\52629\EOTB\143882\150226"}
testObject_RichField_user_14 :: RichField
testObject_RichField_user_14 = RichField {richFieldType = "\1110185|#\21785i\37996-F\GS\178906v\NUL-\162346\27492zcCw|[;)\DC4\1023584\&5i", richFieldValue = "sdB\NAK1\135635`\1053517\41072Rv\146798%\f\1051657C$Plw\1068964b/"}
testObject_RichField_user_15 :: RichField
testObject_RichField_user_15 = RichField {richFieldType = "\156168\anj\27351\51524\STXTB0izA?g<\EM\SI[\260\98246", richFieldValue = "aI\1062456B\1100771\DC4\STX\SYN"}
testObject_RichField_user_16 :: RichField
testObject_RichField_user_16 = RichField {richFieldType = ":\1020969\&0`@fY:+>\22544g\13220k\DC2B\CAN\n\a\SOH\1000700\DC1D{$R0\"\ETXd", richFieldValue = "\110815\NUL)\EMcP\ETXq\NUL7n"}
testObject_RichField_user_17 :: RichField
testObject_RichField_user_17 = RichField {richFieldType = "|\1051704\ETB\11543\DC1$\ENQNz)u", richFieldValue = "b\21656"}
testObject_RichField_user_18 :: RichField
testObject_RichField_user_18 = RichField {richFieldType = "\RS7\917878\SYN1Z\183994\1106281\a\984446;Mu<", richFieldValue = "\999014.L\986542K*Z\1002247B:by\DEL\181509\1076824\ENQa\997574"}
testObject_RichField_user_19 :: RichField
testObject_RichField_user_19 = RichField {richFieldType = "", richFieldValue = "\ESCp\EOT\1091695C\DLE\DEL&\b}\ab)\97554 %;\US"}
testObject_RichField_user_20 :: RichField
testObject_RichField_user_20 = RichField {richFieldType = "`\1054426p.#n\1043320\1010132p\NAKj\74997}\SOH\t5\137952%2[/nr\1080561r\DEL\NUL%\CAN", richFieldValue = "74\40766\1045792\&4%H\DEL\tm6\133949*\1100162\EOT\39089\46584\998002\EM\180974"}
