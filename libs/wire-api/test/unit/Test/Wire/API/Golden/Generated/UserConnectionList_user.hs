{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserConnectionList_user where

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
testObject_UserConnectionList_user_1 :: UserConnectionList
testObject_UserConnectionList_user_1 = UserConnectionList {clConnections = [UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), ucStatus = Cancelled, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T13:10:52.391Z")), ucMessage = Just (Message {messageText = "\1041490\27715{>(|\ESC\60998\1029030\&7\DLE\8490\CAN\146549.O\1069462\fc\DLE\NUL\DLEe\181584iH&GH\NAK\DC2\1003121)\7391\4047\ENQ\DC4R\SOH\171245\16507\CAN\186353\1030477\&7>\1074377\153381\133066Z.H#\1017959\ETX_,\1069010\172608\b,h*/W\\-\CAN\f\11550\13385\983262/\138473\1064631l\FSRaty\6493@\1105939\67848)\v\bP.\1036782`#8[gfy\100168^\1032245wAyt\179399]2n\tB%O:@D\51474\29170\45893\a\155792e\172507UZv\59584\992426\1103999ub"}), ucConvId = Nothing}], clHasMore = False}
testObject_UserConnectionList_user_2 :: UserConnectionList
testObject_UserConnectionList_user_2 = UserConnectionList {clConnections = [UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T18:38:44.322Z")), ucMessage = Just (Message {messageText = "\SUB5\1025972\1000058\127008?% i\SOv,S9Mn\997702\46893\r<s|(z#\SIR/b\147833L\ETBa\1035018\"\54404\986809&yZI\1077391\US\1073138\1085813/au^\r\CAN~\EOT\52720[\1083632\147157\178694\1088535\ESC;o\182953\18579+U4`bp;Kiu5\SOH\1107410|'\54967t\ETX\DLE]\a\3208\135666Na9<_}AG\vzx\1030520~U\n\EM\13271vS]\51762)\16038[x$3\128005\17498P\157542\1013467\&6x\1039436\&6%`l\USb\CANm\19272~\ACK\139630{\ENQ.\bU<"}), ucConvId = Nothing}], clHasMore = False}
testObject_UserConnectionList_user_3 :: UserConnectionList
testObject_UserConnectionList_user_3 = UserConnectionList {clConnections = [], clHasMore = False}
testObject_UserConnectionList_user_4 :: UserConnectionList
testObject_UserConnectionList_user_4 = UserConnectionList {clConnections = [], clHasMore = False}
testObject_UserConnectionList_user_5 :: UserConnectionList
testObject_UserConnectionList_user_5 = UserConnectionList {clConnections = [UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T17:47:13.393Z")), ucMessage = Just (Message {messageText = "\r\SOu\1038789N*\1061107Iy{\a\41188\DC1ZR0\150077\1053273;G\STX\63882\DC1\1058394\SUB\NUL\1044630\128158T'XJ\66467\&7PMHR\DC4Gs\b\\|^5MJ}^UZ\1067848j\162471\1033536R$\US\177403/~P\65946'\SO\tk\989331\180588\FS\f7j$RW[k%\1057039&\n(o\DLE\GS\t62\DC1/\176789K&\1016952{'\EM\162110e\1111376\&2\t\34408NQS\NAK\a3-\98320)05I\DC1^?{'\1036112\a\1102507\ETXi\146984\998509h^\tEKB\1061171\ACKO'Jro\t1W=Tn\52916Y\1000817\1088064\r\990623(\70164g6\166629<'\996879\1088002[&oc6X\CAN\1049908\FS(\SIaM\4837a\EM9JB` rt\r\NULZ\1064850\STX\24324"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T15:57:08.796Z")), ucMessage = Just (Message {messageText = " p\ETX\DELd\DEL\137355=6h\fC\1018312P3.y<=\1077409w\1094500W\983884\169111\&9B}\1071086wT=S\1080988\vw8\t0\DC4\SO\165539\f\DC4r96\1049126m\19920)\1067919m.\15577Y\1034365"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T03:18:05.825Z")), ucMessage = Just (Message {messageText = "\CAN]#yhp\fF"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T00:12:22.416Z")), ucMessage = Just (Message {messageText = "{c\1045777P\DC3lKk\186027\1008762- \ACK3\RSe7j\1072104p+\1107823o\SI@\46925\DC1\b\1031716/4%v\1028090Dl\1088079XwLkU\157409Yx\144733|+\1019119\&7\1106862\&3\24287\1030070[\SI?\DC1H\n/\1019376\SUB\1042364\nu)TL56YYLZ\FS-\986254\&5\">.M\EM\72981\ACK7$\DLEl\FS`\DC2\RS%\140873U\SYNv\FS\FS"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), ucStatus = Blocked, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T15:31:47.465Z")), ucMessage = Just (Message {messageText = "%6}\SYN\US\1090152\DELHF\149295V\23604\1019616\&3\1102918\"\DC2\DC2\1105198\GS?`\v\28608\&7\1069834\147339\EOT\DLE\194607\1036683\1106975\187630\NUL,\1100763Rvg49s^_mt\153468\189958\1070291'I\STX2u\SYN\1099129)\STX?Y\987063/'\181380f\1049513\139105q\NAKmy\41360\b\FS\148354p"}), ucConvId = Nothing},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T07:43:45.261Z")), ucMessage = Just (Message {messageText = "wt\DC2\EOT\r!Kpj> \43322'\1024866'\1041315\1034025-EL\ETB+\DC1\134537\43155bx\DELQ\1101307:\1012929G2S`F>dc\65313E"}), ucConvId = Nothing},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T09:47:02.441Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T01:32:46.049Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T11:18:58.441Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T14:52:45.948Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}], clHasMore = False}
