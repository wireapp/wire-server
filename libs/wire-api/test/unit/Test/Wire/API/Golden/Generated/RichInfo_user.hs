{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RichInfo_user where

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
testObject_RichInfo_1 :: RichInfo
testObject_RichInfo_1 = RichInfo {unRichInfo = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "e\184406[K(*< \140930F?\131194#]\36375", richFieldValue = "\29654J&.ng\bl4||R4U\186313{\b:\NAK}\150085O\a}"},RichField {richFieldType = "\DLEz\1004657\DLEs^\16330`\47563\40774'w\631|\SO\1000859#-\1045101\SI\DC2n\SO", richFieldValue = "\997151\1029006\"a\SUB\ETXxk\vt\1029015sD"},RichField {richFieldType = "\1004730", richFieldValue = "5`a$\25184RT40\ENQ\1113587i\f"},RichField {richFieldType = "&,7l\ACK\1043722_^\v\45919\EM\1015251\159740\1044270\151417'8\1114048\CAN:\ETXl\\\26834nf Y", richFieldValue = "\NAK9"}]}}
testObject_RichInfo_2 :: RichInfo
testObject_RichInfo_2 = RichInfo {unRichInfo = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "uk\986163k\STX\STX\11154J\995816\NUL", richFieldValue = "\DC4}4\5300\1079940\167575\ETX"},RichField {richFieldType = "4\ETB\DLEO]\DEL\169107]\CAN\DC1\169564\139646f&N", richFieldValue = "5\SYN}'>5\SOH\1061073\154508\&1\DC4)\SUBI[\171629^l\1002843\1108583|\153177TZt"},RichField {richFieldType = "\161163h.W\a\1030451\1112113\1015736\186756\\L>\SO\1017866\DC40\163242B<\147598", richFieldValue = "\987469"}]}}
testObject_RichInfo_3 :: RichInfo
testObject_RichInfo_3 = RichInfo {unRichInfo = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "-\DEL<\992646\US\1088131F\78128\v\ETBf!\1011786\1049153\NAK{\DC2\n\"\1044178\1034027", richFieldValue = "\188034\ACKB0u-\viZ\1019516 #=~\1044288\ETX\1073497y!6\1060658\94587\"E"},RichField {richFieldType = "1|\142735\v\120434\&6{\SUB\139328n\142616=#\1063744z;\2495#\ESC\140699\139391Da{\GSI\USAE4", richFieldValue = "\"\187355\36855W]\NUL\1036953\43824\&6,\FS\1039941:\DLE\EM\13737Ii_\1062118\1000012\EOTRXr@P\NUL\STX\20294"},RichField {richFieldType = "\16257\67288Kik\DLEv7!\29791\NAK\97003", richFieldValue = "C\15563\13140\34300\SOHC\v\1047355MHfO<Zg)8;\1018285\SOHX\ETXe\DC4k\1093546"},RichField {richFieldType = "\1113196\&7\aG\25510\a\1111359i\CANM(\\3c\32773e\22706\FS\EMgt", richFieldValue = "\1106635\SIx\68002\&4n*"},RichField {richFieldType = "_&\CAN\1108202<\\\1051258D\1027509\&5T#\SUB*}\128562\&35\v\131553\v:Dx\1071811\&0\1033510", richFieldValue = "_D\1096230\SYN7)Q\120586"},RichField {richFieldType = "\166981\DC4\"Bh\NAKpy\155139\49477\&8\DLE", richFieldValue = " Y3`\SOU~:lfD\FS\1054298G(\1081380\ETXB`9\1015647"},RichField {richFieldType = "\1041310\1069706\993457\41982ld`{\30872g\DLE\47758\NUL8XL\40623'G\997391f\f,\1066615'", richFieldValue = "\1024314g4<\992878\&4\SOy|q\71180\SYNT"},RichField {richFieldType = "\1039070\145990dB\1052965\19270\tm)5g\42270\USd\988088:hd\1056042-\1074709\35119\17349@", richFieldValue = "]\3277"},RichField {richFieldType = "\158912\1074705\180536N", richFieldValue = "?=m9jYsS)\v"},RichField {richFieldType = "^%\162716\SOHR", richFieldValue = "\40311\840%:Z{\1108246\3535\EM~\1037063M"},RichField {richFieldType = "v[_\GS_\1083277!i\72133L?T8\148330\SI$Z\DC2\n\GS\1112796\&9qBa\988433\&7", richFieldValue = "P\1111013Ue\1026607\1106097,it\21229\n^\SO@\RS#V32"},RichField {richFieldType = "\1010462\139192j{%A\1084717\NAK\990308\185622\ETX>3W\SO\1009927Z8\151462nn\1060958\25608\24455yd\a&g", richFieldValue = ";\SOHALYI99]4U0"},RichField {richFieldType = "pi]\ETX8\GS$]\988468\1006711\SI\NAK^Vg\CAN\182592\ACK\141094\16788\119091g\92319B&8\1022206\&9\46719\1049541", richFieldValue = "\SYN\ESC\SOWB\507\r\144124\SOH\ACK$Y|\USR#\1072307\1041104\DC1R\74130\bJ\US"},RichField {richFieldType = "k\SI\11085\ACK\RSs", richFieldValue = "o]\EOTA\ETB\SIjD\18175\EOT\ENQ\r#6\71169v;\FSD;\1003217"}]}}
testObject_RichInfo_4 :: RichInfo
testObject_RichInfo_4 = RichInfo {unRichInfo = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "\1009413\f\US\a\1099989\DC4p|^W|\ETX\SUB\SI\n\1108242QF\94422\&8y\146590", richFieldValue = "\1054635S\DLE\EOTqqiKKk;\v\31937\1039500M<\EMB"},RichField {richFieldType = "z\NUL\1027861u)cI\n\NAK", richFieldValue = "\166899Z$\69428TKaTQm0"},RichField {richFieldType = "X,\ta=k\33192", richFieldValue = "P\1088125\&9F>\1005831'\f\27150o\ETBZ+CI\991147\v\111225\172840\99659\DEL"},RichField {richFieldType = "\DELA\NAK\1021376\v\25495\150971\1051915kJ}X\rX\175041+#Q\DC2\DC2vR/x\32810\SYNY#", richFieldValue = "Mh\SUB"},RichField {richFieldType = "\1260\ACK}\9651\1093762\fEI~w$2;$k7\1039911U\EMCf*_g9ETT\EM\999449", richFieldValue = "\DC1"},RichField {richFieldType = "-U\no\1022913+\995758x_\1011409[\SYNsX", richFieldValue = "u\ACK\SYN\1078024\EOT16NM"},RichField {richFieldType = "\28562f<", richFieldValue = "%@iXp\CAN\991881-\1079099!l+`a7"},RichField {richFieldType = "~\vt\SYN\136799F%y", richFieldValue = "1U\n\NAK\155792r%I\18996/\6683x\51082\167789\&6"},RichField {richFieldType = "\38828\DC3'2Nc", richFieldValue = ".\1028177"},RichField {richFieldType = "[\ETX\ESC\1019444y}(*\173240G", richFieldValue = "\983375uu\SO)k4o\995060=B\1107706\RSLLh_*\1035250\1059729"}]}}
testObject_RichInfo_5 :: RichInfo
testObject_RichInfo_5 = RichInfo {unRichInfo = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "7X\SUB\STXF\t\CANTN", richFieldValue = "\"gi\EOT\USfq]r\19294s4\\\f\t\1110565\DC1[k\150950\1085987\STX\1073158\&7\133870\&4\1099468@\1107035\NUL"}]}}
