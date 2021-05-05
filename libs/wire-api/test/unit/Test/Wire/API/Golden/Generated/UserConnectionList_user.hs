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
testObject_UserConnectionList_1 :: UserConnectionList
testObject_UserConnectionList_1 = UserConnectionList {clConnections = [UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), ucStatus = Cancelled, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T22:11:50.873Z")), ucMessage = Just (Message {messageText = "\1101342`R\1014207:-IaY34.\a3\1099006hp=\EM\169475rJe\158493M1\SI\1052901\1104520\DLEkX\21195\vv\1058597\1046938\30686dM\v\1076251\ENQRT\SI\NAK*\164947=\NAKP[G,\\0k\GS\127852\1016998\GSWN^BA4 \64677\SYN\144368z\137687!4@\GS\n\ETB\99103**gI\1037861\EM:yt\DEL~=C]\n\US\46859d1^_#\181917\USa\ETB%5\\\EOT=\1056071.\NUL\11341Xb!w\1043424\ENQ\173709\1018817\DC1DLa<\\\RSKm8\1107160=\171144\1099556F\\H9H\v\1018028\SOH7\1048135\&3:\21466'\137268\1073621<\EOT"}), ucConvId = Nothing},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T23:55:20.348Z")), ucMessage = Just (Message {messageText = "\1113648\185952t\49704AI\1078694)\"\DC4r\DC3ov\986384\DC3J\CAN`\NAKzuQ&\1047804w\96761o\GS\1047698\rf\1021804\1092933\33167\SUB\46275\1112769\FSE\1090323p\r\f8\NAK(9r\NAK\691\ACK\vC\1017642\RS:}vu\SOH\NUL\SUB\133842J\FS`\\d\1041204wu\\\SO mt\18751\DC2/8SS?W\STXv\GSW_\180026\&5\15786sih\SUB@\1111738\1025852\r\176861\173096\&0`;\1083176\29832\NAK\ACK\985465%N@57\US\1069696}\1014162f\CAN\v\EOT\EOT\f\EOT* \NAKlom\DC2\31988\&6F\SYN=f=\DLE\SIca\n\1093698Z\984493\ny\DC3\1049368o\SI\ACK\143905\a6#\DLE\STXZK\SYNlDfD\155002j@W\1097734\SYN\156911f/\ENQ\1063446\ACK\b\1027148\28594\1024709\1000425\SYN-B\1009248,/\DC2G\DC3\SUB\17864mK8\171788dx\DC3\SYNzPT\CAN*j\1032137=`\155609\f\1050285,9C\1061636\aEOA\13907h\1081717x9=\1046859Ng\a$9\NAK\"\33020}W\ETBi"}), ucConvId = Nothing},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T02:46:09.008Z")), ucMessage = Just (Message {messageText = "\1066375\1102222U\163380x\1032475\162533=8>\"g[\ETXe1\1102148\r\175244\63011:;`\1109639\DLE>A\1064037jF9\1056077\ENQ\RS7\DLE\DC1ff\1037988\ACKwPEOtg\97688r\SUB)\ac<ba\1050559\31044\1112854\&2\1089371\ENQ\USxn\DLE\1020861g\136283\"\1027240\59694\12606+\CAN\\@W\DC4b\184000Nsr.\GS\a\STXc#\67106|\1052735y\25286\1027026O\176467Q\\\1048221^\SI\ENQdDr\afM3J9\17144\13401@\1011288&\FS\EM\t\75024\&00s\ENQ\DC2\1093610 \1019084\SYN\110981\141290$e\ESC\40721.\DEL<h\DLE\STXxO:GN\1015092\DC1\995751\DEL\19668\168446r\1029862Rk\US6DQB\161435y(\1112131w\147423?\47456'\39438|&\65310E\SIq0\1045329"}), ucConvId = Nothing},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T15:16:15.026Z")), ucMessage = Just (Message {messageText = "\SOM\27617a\SOO\134209[<5\"\DC2\USa8\149200m,+j\a\61253ku\1087740\188770>^>a\DLE\SYN\990781\1081283z+\CAN\179293#}&\93971:\121446\166663T\135983\SI\DC1Z\"Z_"}), ucConvId = Nothing},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T08:35:28.824Z")), ucMessage = Just (Message {messageText = "\r^l\NAK\GS\EM\DC3\DC4q\NAK`}\993812\165525\ETXRZ\186062\SI\DLEa\1033517A\ESCX'\SOH\DC1\USg\EOTOM\996297[mF\135806\v\\S@\1013505l\1083493\rQ?\"(;aH\n\DC4SZa.\CAN\124990 \1098284E/zQ\990881C`F\DC2\46306\DC4ES{I\CAN\1046063\NUL%U\1006856SyWbU\\\\]\51967D\\{ST d\1049398\53432I\1047833\1090022%\DC3\164899=\DC4\39170cH~f>3\1056868\USf\1061464\SOT\ac4\1011503\vC\1002797\1032878\128250DC8)-\132633\1109589\97388Z\1033485K7t\47697k\nz\172193\\[\1014063&\138787\26160\\\STX \1006080]5\61270\rY\1011813{ti\1037927\13615OsB\DLEQ\1072515\1083388\58875:\13952rw\DC3#}0\1053154\n'U\b`{R\1025787\SOH&\1063004B\4496\154060\28327e\SUB\38011\1045771($.\144915j%8Bl\SO\132846\v\r\CAN_[\983820\f\1051035rPaT\fP\139024(\SUBQSQ\179333\SYNk"}), ucConvId = Nothing},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T08:00:56.555Z")), ucMessage = Just (Message {messageText = "\ESCI;F\a\6151\1037754gR,-\167665?\DC4\USA-x\DC3\DC4<\1112198`O\1071824+\DC1\991005\SOH>/\1081978\38986\1072624\&8\1022566U\11333 \DC3_h\170752\&7e\165651ML(\147898].\STX\rb\9068\STX\78355\16448\&0F\1050486\NAKHLly\SUB5\174308L_+V{\b\DC2}\DEL\1110004\DEL\1064027\&1a\1066891\b)n9nrr\SOT\1061240\4999\DEL\STX\STX^\917909\1061214\50418( d\SIl\143494q"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T07:25:51.803Z")), ucMessage = Just (Message {messageText = ">\SI:42\166916>1s\1034143\1033428\986451{u\2636\190553\191284\1054055f1y\b\1060457\1085328'9{;6~W`\\\US\1023889ZqjW\NAK)-({\")\ETXbP!\f\ETXF=N\1095495\a?!\38464\184867tmYRD\38767mA\f\EOT\USUQsZ\ACK?XP\CAN\DLEM\4343\29307\1053108\1041635|\1001915?ke\174478A$\USk<\DEL.\1096320\41470\1025472\DC4]\SUB\1020463\EOTl\NULX\1108291S)Y\1019850V\50884S\70310\b>j\SOH\2342U\55264NdaDO\1003110K\999948<\33747z;V\30067\54274\13811v\182504\by\\cfv8BM2\GS\USy\1035512B\SUB\17521gp~N\144287\152501N;\aU\1087829[\31238\&6\1111982\a\27784\"\\6\CAN+d\rw\1027341\1031941_h\1015889,\NUL`O\1097133(w\178931\DEL\\\45708\v\FS\FS\EM(h\1014614\DC4\ACK\994517-6;\170474w\2070\13122\GSx]\1040716tx!{\1042876\f\SUB2Z1\49830:,\NUL\8086"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T21:01:19.526Z")), ucMessage = Just (Message {messageText = "-{\58731\1053954]\r\1035166zi\\N9v/8**\53618\&5\167701\74784g;mo\n\SOHAK\DC2w\vK\184374+\SO\1060574=\DC4nHl\139169X\1075933\1098877Z^;\SOH\ra"}), ucConvId = Nothing},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), ucStatus = Cancelled, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T05:11:30.428Z")), ucMessage = Just (Message {messageText = "=\US\179966H\US\vy%\162378\11291#\135108z\50922\1092539\181576\178453\30065\&5\GSz\1070892o\n\1018186\fO\DLE\1110298\987508\&2}\187554\DC3\992899\148573Sn3\1041199AKHG\1079837;w\1072772\1047863\SO7d\SOg\\[3\EOT)\174471{t)W\1016657\&32U\1038172\1008350\185524^\20703"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T09:39:10.149Z")), ucMessage = Just (Message {messageText = "6\FS8\1079695a5\DC2\178769*\98956n\33432\1046209mk\CAN8?\US\ENQg\1098634\176173\SOH\NAK,\38007\&9n5X@\162299u\DEL\42059\ETX\1017362\SOH\1033657\186387@vk\1096584\DC4\1079283ry\140440I\CANPF7iP6~c\1060714\78370\&1\15940\162801\&1\DC4VD\42689\STX4\SYN\20842\ENQ_\CANu\SOzq\1063430{a\SUB&A\1001054UN\143163uE\1095413N\GSJmP\EM\USu*\28423\1082752\1047327[y\1078042B)2\1004131L`GN\DEL#\">j\989090(Xr\DC2\1004060\1046529\&2x\1054453U\"I9!u\32035/\24849\&85\EMW\FSQdT,\rH%\13860{G\999533w\ts\166215\160239\1069315n\RS^\166798\1032321\17473\1060867&D\ETXV(n\3542\1091177X\152447\100084j\51992#xmrKgt\CANH\128807\1086092\ACK\EM\SOHYT\DC2-3\1055651YC \99309\SOHN4\1018037E+-\b%\GS\1009795L\"$m\RSv;\75013!x:\50707u\RS7l2N\168545\v\1048605h3\CANW\15999\990353\1108124j"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T13:04:40.825Z")), ucMessage = Just (Message {messageText = "~\165105\"\24364\58133\45460Xk\163870k\DLE\135980WR/\FS\136333\ETX\1084391\1020001\1048849\1087802fx\STX4AU\10849\f\NUL\fHl\fb|n\DC1\ETBk`0`\n>\DLE>0\1106561\ACKn\v\1089669\1107862\993739\1061859\DC2\10038\1026015\1003775z\83335f1\DEL\141266_\1000414c>\37433\64896\46533b\SYN\60105aF\1015101FR\187565\DC4~\42054\NULD;^\1043020\NUL\SO0\162076\1069149\62177\1096566\1011134\EOT\f\184886\1035657\&8Z#\ETB\EOTT\1086291K\13487\61274u\EM 4"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}], clHasMore = False}
testObject_UserConnectionList_2 :: UserConnectionList
testObject_UserConnectionList_2 = UserConnectionList {clConnections = [UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T22:30:46.406Z")), ucMessage = Just (Message {messageText = "UU4\1059286<c\1048037^A8\1070024\"\tR\1014350\NAK<z\72354\40450\24954\ACK\996049\&5LsO\162074\STXXb\40011\49202;'07\72125\CAN@o.I{\178034"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T20:53:26.285Z")), ucMessage = Just (Message {messageText = ";=\161443\1034292.Q\1068290y Et#\ENQ!!d%N_\USktj(\1014289\ACK)"}), ucConvId = Nothing},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T19:59:09.525Z")), ucMessage = Just (Message {messageText = "\1085494\&3,\194834\1013003\&4\1039500\ESC\DC2\96379\1098576p~`\ACK$B\177742\184762z\SO^h,h\190105*2\181656\1055140\&6\1007723F\1088397ZZw\1004074A\74848\GS\SI\20348`\b!37U\ETX&\vL\34874\DC4Z\1004256Y\b`^}\1056969\&2gs\NUL\1067629Bb\DEL\183919lJ`\183566\ENQb\STX\1071606G_\a9d\SI\b\t@8\EM\1022732>\181184\FS\ENQ$\ESC\191377dU\\\1099423|R*;-\49004\149786\NULFB\144430\1093575#XQ\149320)\SOu\SO\1080956mb/1Qq:\"\DLEL!uR\EOT\a=fz\RSs\129589K\120814#qVJx"}), ucConvId = Nothing},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T14:05:03.895Z")), ucMessage = Just (Message {messageText = "\142978\&7\DC2\FS \32714\ENQ\aE\SO,7\USiQ\EM\n\139450\1032378 _ov\174977\&5\59943E\ENQ\SOHNP9\1112748\1040811\&0-\ETB5`{\1064708K_N2\53584zF,|W\\f\EM\19322\&5\ENQ'%:\181932-T\1105835}"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T11:32:59.336Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T09:50:05.424Z")), ucMessage = Just (Message {messageText = "($\DC19\SOW\SYNT\NAK\1058927\"\1113190)!jm\1047691\DC4l\171563o\1046441U4\9823&L~\STXKEn\EM\RS\STX~\SYN:L/\SUB\994444\1067437_0ByeN\1056492*\1090447\1070240c&NCX\148860$\147876B\1022025\131557;78\EOT\21472VI^\EOT\45534\30678Q\1000583dwS|!\DC3\1095782>\1030575Q\EMx\EOT\194783:[Mula\ETX7}\1001803A\166097\&0E\162633\SOH8GU\DC3T \rl4\NUL8\CAN\r\1084072l:K##&\1056107Suun2\\ACM\r<F\12219\f\151949rAQ\128327t\148812\&47\986400u&\"\32380\986097\1020617\SUB\DC4X\163173+\1090061N\1066635\188057,\52546\59481D\1112575\&45'a\DC46\EM~\12646\1019006\&0@5\n\121315\ESCd8)`\SYNZVIDT,\"2k?9^\47288\131538D\139284\&7\988922\188281\ETB,cc`"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), ucStatus = Cancelled, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T03:45:13.176Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T00:35:09.637Z")), ucMessage = Just (Message {messageText = "r\173170xvw04\DLE\CAN\986768\1031404qU\21549B\DC3\162250gg\1055615x\DC4n\t\DC2zf\rd5\3109W;\1100390\145231T\1035548U1d\ENQd\35325\DC1>\ETX:h\188374R2\15970\135846t\r*),2MP\ENQ\1040202K\24546.p?6\131186C\fy\ETX\1027178\1060842\&5X\172631=^\b\1029029Qx\39679a\1065071\174640\146409\74644\ETBP\1048285\&1\RS\ETXVid\SIX\1015605(\ESC\ENQ\1083438\126125\167946j\t%6\190519?](8n\STXK\NUL\r62\1109026\GS\r\DC3\1049061\\ms\DEL}\184297A6\EM\\R\147981\DLE\150735,;\37707\133970x\1089151\DC4\135809\1059610\RS,\SI\1055348\1739#k\SO\nY]8k\r\b\EM\1113320\1015940\NAK\46279Z=\36994H\r@5\1068582\DC4}\174991\GSl\FS\t\1026964\1108123_S\DLE\189379\110984\&4J<7\993416\&9\150692%0\1052271\NAK5\1080452t\USS!RgU\1060141Z\NUL]@\61740^<4\28070-d]Bd/\1003548h\rv"}), ucConvId = Nothing}], clHasMore = False}
testObject_UserConnectionList_3 :: UserConnectionList
testObject_UserConnectionList_3 = UserConnectionList {clConnections = [UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), ucStatus = Cancelled, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T18:03:08.339Z")), ucMessage = Just (Message {messageText = "\DC4r\1014849\1033648f\51470;};)\5275RG\28861\ESC*.\1097447\CAN#\ACK]nZnr\987546cL&\1058387\DC1|`\SUB\SYNc\\pU\119901Q3s(r\v}\1084229|\128850\SO\141810uy\r\ENQ\1015603\a\a}\DC2x\129053\97253\&3f\NULu\100766*6)[`\998123\v\1053568U\"fvN\b\137411\US]$\63117\63631\&5r\5306>qx#}\178864\v3\145112B\1027158c\EM\NAK"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T06:21:36.889Z")), ucMessage = Just (Message {messageText = "i5\US7\1092387\&4HxX\1069268\1022608\154891\DC3\STX>_N*;\1108307\42887}!\14336p:{[\n\1050595\&5r\182378a}\141260A:A\1008790\GS\97037Ha\STX\SIZ<\r\r\NAK\33025\EMZ\1082169\1067743q\158122f\DC3\DC3\ACK4M\b\190207Q\1054073\119315\135835|*[lO\ACKh\185053%\174805\1065633\1039735X\ETXY79n\v\165668\10336xx3\71483{\10585\1112232\166329\1003490\26350]\DC1_Dz\983348$6g\f:T\1053443nR\NULi\27712\163718\1000292*\145622\135849\FS\188200k!\USD\CAN\174690uj\EOTs9kj=k`p\SOHHjtu\r\ETB\GS~~o8\133975\&0.\140716\1082427f@\t\39382\n\71169P\ACK\DELhe\995986\26647\&0c2q>\57610@M_\154717^\12520\23079g\CAN\1056901\140513-CGLzX0\RS$b"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), ucStatus = Cancelled, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T07:24:54.425Z")), ucMessage = Just (Message {messageText = "P\13974p\r\6490'[\ACK\1070155i\1056782\ACKio\1094235/t\52396[\6106U\GS\987138Y\NUL\61217B]y\EOTPS\fKM\133181\SYN2b\FSs\rvm&\STX\DEL]\GSM)eCV\6968\163985\95341\DC4.FE\\\54003*B{\1052836\tU%\DC3ogQxh&\EM+\DC2\1045784?\119133\167977r\GS(\43312Z\40837\&7&\25490\1022856v)\183810\144557_\1022348a=\50270n\986897t\DELF\1108766@7G\1009625tZ\179051\&6 \1086778P\DC36\DC1\DC2b[4(\174103tj\aq\133509+?\984084\DEL-\FSB,\1030187\1030899\US@:0\19191<\ETXM\1064009!;\169334D!\133955w\1109357%\31369\DELZ@E\1081230{_\143386Q\SUBoj\157953\44875\DC4[L&\132155\144959\1046321lp\15031\1063063\EMh~C'0BQt$,\DC4@Gi\1067342T\985881p?2\SOHb&T \DC2/"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T07:53:13.119Z")), ucMessage = Just (Message {messageText = "\94380s\5442\&4u,O\120640M[\61077|f9\r\27541\bR\1070701^E\ETB\"\US\34460&cS] \168873\&8\SYNU\bj\tib\ETBR\1007734J\STX\v\1096049U\b\RS\1101754\1104553@N\"\aQp$[\1076692\&7\12143\1078081*hK:U\44902\1060391;h\99908\ENQwb\NUL\173604\62748\13994\ACKS<\1092570\SOH\DEL7Ua\DLE\SUB&@\145500\ENQ\1097704\DC4\38881U]b`.\19326TJ\DEL\1094480\74288!\37484\&5\r\1104989\&5\r\f)\172232,\137318R\1104340\n\190216\42538\ENQ\ETX:qm\1022543.h\v$EP\1094563\GS-%E\42581N\EOT\DC36\1061063\GS\n\1090396\&5\1066057\54265t?Sz\169412!\a\SYN\24431\DC1ZX\4256;!\172906\168609[\1058625r\149037\aE|\4028+\995190\68768E4\DC2\\9\1021267\1068499=rB\987535\126523\DC4k\tD5WqH\95052=\US:p\CANP\1015998v\1015450,\125041]\144327e\13290\DLE\a"}), ucConvId = Nothing}], clHasMore = False}
testObject_UserConnectionList_4 :: UserConnectionList
testObject_UserConnectionList_4 = UserConnectionList {clConnections = [UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), ucStatus = Blocked, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T03:32:45.796Z")), ucMessage = Just (Message {messageText = "\GSN\ENQk&>\1024217B\STX'\97219V\NULf\ETX#p5s-`\DC2J 7\DC4\ETXU\48122\RS\994413U\59139\1059671\1003120d\1030854^M\r012}#1<\EOT\t\137855\177524Q\987513\&0!\180898\&0H\998403<\STXd>\EMU(\133394}@\ETB&A\1065751[{\62378$4F\NUL(\1084107!!l1B Z\ACK#nE>U\4142/\CANbI\n=\1044573y\DC3C\EOT+\1024656B\DEL.]\1007324~\1071082\f78\FS8\FSJ9'"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T19:53:17.976Z")), ucMessage = Just (Message {messageText = "{R&\DEL\1062601/\nqv\166806\DC2|\133967\&0n\32873\159803f\49005\100120\1027575O\1028223PCr\6310AP[[S\NAK\66892\18549\1070972\984554\160663\27882\&1\n%k\154620N+31\1080806D@,m3\1085119\SOH\NUL\186183T ]\1074268\1043949@/C\17015K\EOT\DC3\SUB\1095437F\97409$\988227\vH\113707.\SOH\nE]}\EMlg\1106710\113675\CANR{\1077545ga\129181\1021348\131573\&51\SO~\SO/\173229/W\ENQ(\\1]UrByc\STX]\96921R\8962\1050554]Fj\DC4\GSq\RS{\ACK\1032249\145198\ETX=\SO\52397^3\DC2\985643~\SI\DC3\1113556IU\1090155T\1112589\4491\1032340nn;|\GSH*[\US7\ETX\vX:>@-\1006303\1007859;\1080243\fvc$>6H\DLE5u\140167\ETX|A;\170870\152876"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T07:03:20.519Z")), ucMessage = Just (Message {messageText = "M`-\25424\155264\GS\128147f=\RS\1018148\DC1\ENQl\145541\DC4f\138789\EMQ\US\1076119\53027\1080143d\35109G\11206j\996624(V\120404\a\STXo,\1071070y\138729t\DLE;\ETB\37345\ACKy\1029207\1044170.T\139009h\ACK#l|\120793sjw\DC4p\ACKZ=8\b\189873k\"7t\169483\&0x\1003035~6\8890/\1017018m\1094724d\1027350(\62133#b-'\6721\ETX\ESCz+\EMrU.`?\70669{"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T19:38:46.826Z")), ucMessage = Just (Message {messageText = "\26391qI\NAK\1026714O\1087568@M*p\188412)\bJFh\161421\41012\33225M\1045274FZ\1043035\154593w4G&88n~\98067v$\988372"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T16:17:50.399Z")), ucMessage = Just (Message {messageText = "\GS8\FS\1113593h\DC3\1024534*\ETB%\\3t(y6+qE/\54482!\121428v\a\NULJA0\DLE[jsGIk\177653\150491\\\ENQ+=K\r\DC3%r\149702\f5K\CANKi\71453S\189050\ENQJC\DEL'\DC4ZJ0\990417\1027484Bog\NAK\30443,\150612\155587\&5OQ\70422Y\ETX)VHC\DC4pXz\14186\70064!I\DLE\bNkvW\ENQ\1040944|\NAKwzuo\DC3o\987276\162932H\SOHy\b\917886HG\GSN\176684\157162\RS\37075\61500\f\3892\1021204\STX\153468l|hob\EOT\1069270P\1101824\"\DC3\1062998.(s\1054995K5ek\FSj0Q\SUB\GS\vB\CAN`\ENQ\63004\CANP\31187J\ETX&\1030746w]Mb9\SOH^&Z~|D\ETX"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), ucStatus = Cancelled, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T14:42:32.742Z")), ucMessage = Just (Message {messageText = "\1098197K\1013809\DEL\NAK\ENQ4\SUBdR0;u`u\FS8\48972V\SUBbL\SI\97649\"vO\991837ID}+\DLE%9\f%]\DEL\1071522\133379\1037360\SUB\SI4\168703[\1072094~=:\1043137\984962C\1100375*u>l\v|j\SUB\54633-&\1089028rSl-R\"n0\1044775d\991795\SYN\ETX]\1094422CM\CAN\1064342) \SYN\1090344\SUB\1023125\&1\5714y;\ESCv\14737!h\1029847\EOT'@gC%\126218OVW\45783;\DC4\63078"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T00:23:54.327Z")), ucMessage = Just (Message {messageText = "}1\1002961\1074206\111220g=\1071539\142740s\1088399\190921\1063117\n<}\v\7091K9h\178099\1017930\1058537b\SUB`\989456\64625n\1088052=Xo\DC3#+\EM\989685\DC2\1104432\49663\187679\&6)\1033535r\1090934a-E5\995443)(h\EM)V7\136187o$\ETB*\23028\999824]$\GS\96412PS\27377=[\989642\SOH%\167698~\1039264<\NAKr'BY\f\16517E\SUBBF\1060190\v\986685\1098879\US\179546\r-`\STX=b>\DEL\ai\142421\1060852\r,f/\157470&\\\ETBdM#\181328\24937u\154649r[\a5T\1094953}\179582M+\191314S\"H\1002808~\a\SUBpxX\ENQ\tA\100306\133321\&9\a>\DC4\r\NAK\161971\SUB\1014159\DC1\1014311+\US5\1045413\1095271\&7\\\EMF\28815\27718(\t\DLEPo}as\1080993*jd\1013884\SYN\45065i\DC3\t<K\94448r\1112148mPV-z$\52766k\ACK}\1065695e\1018332H\34484\1041371[\DC3\b\1029428\t\DLE\13429\EM =}I"}), ucConvId = Nothing},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T04:25:10.288Z")), ucMessage = Just (Message {messageText = "A<U\54510O\1014420$$.\36967as\1065593\65452+\64086\RS`\EOT\1025483m\\\DLE\"l\167371{RS&:6@\ESC\140574``C\DC4@\DEL0\EOT\995706i\nwy1\145194od\38104\120951\140177\ESC\ESC\n6M7\USSvTV\168584\60084\ESCt3\v\EM\135754\&8dx@\DEL\b0\SUB\1110520\b\15773\DELu4iD8K\48122t8O\SI\1001795u>\1060109s\23918\SUBuP\DELK\180799uy0JCCTR\DEL\vo\NAKF<\995072\ETXmZA\fr\182715\1025049m\110798[:\997624\1010469\SUBGE\98392l\ETX7\61891\SI\\\ao5k\1058838\157398):pF\2873&r`Me5A#\145609mESmn\1089685BFM\1000546\1100802=\44658Z\1011753\ETXm\FSvK\995861^\917943s\20429\ETBR\24281\CAN\f7\GSc\140982\1030463)\"@\1019730\RS\33060\CAN\FS&On\49401\156981\\\EM5/\ETB\ETBT\1109920\50743r8"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T17:58:33.528Z")), ucMessage = Just (Message {messageText = "\t(\22300r\1003879\fz8VrB\NAK\ACK\NULDBm.F=/h\ENQ\162046\DC4\8990\&5\n;?\148020\100327\DC3\tR\ETB\ETB\ETBm\NAK\\V-\169780\RS\985948{LJv\"\146367\rn\1027070$\"rm\1382\53770\152469<a\986626`^\NAKl\1009621\1078267CLb\1056567\SOH\161682\SOHex\143808\ENQ+\SIZ\169177\GS\46266\1056743'\21727\DC1@@\SUB\47590kqK\ACK\1040178#EW\61598\159464\&6\993948\1108556\1014157\SYN\135961T\1035059Rzz7\59260$\1093051Z@\DC3\11203\140031\NAK\r\ETX\CANE'Nm%\SOv`\DC2\f&\1094550\r\61031\SYN\SO\154624\&38\1050267_\1082677Wps\DEL\DEL\\]\1081019\54831j\n\ENQEq'V\58010m\ENQPq]f\168449d5\EOT(,-2\t|\100299P\ESC\1086964Z8Q\CAN6P\US\US{]do\ENQ\5102~@oc\DC2y\1085802'pON#\r'\1041876"}), ucConvId = Nothing}], clHasMore = False}
testObject_UserConnectionList_5 :: UserConnectionList
testObject_UserConnectionList_5 = UserConnectionList {clConnections = [UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), ucStatus = Blocked, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T00:48:05.024Z")), ucMessage = Just (Message {messageText = "3JR^\\n\41353\1039466\163866%\DC3\1061032X$\1010546\61804dP\SYNO`e{\NAKp\"\152095<|K\\1\1107080\ACK\23476\br9u,\73740r\1545\4726;O\172013_U\175806no\995411\1094446Ue^\988724\98621AUR\1005400\EM4\DC2\EOT\96155r\1034385\RS\67164\&3-^\NAKF\987000i\ESCMl\18049\37169\74134\RSs p\ETX\1073636\RS\1021202\DC2+\989783\EM_D\ESC9)\FS\ACK\131496e*J1$]\f\64659&\132410\b\DELy3\53053\154528kVE\991328\1113087G\125120\&2\162414\1053297\DC1\n\CAN\58163\&6t\98116f7\1079098]=\174670\36450\1028888dA\SUB\ENQ\SUB\156865\172003XT"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T18:13:40.053Z")), ucMessage = Just (Message {messageText = "\991176=LhF\100780\&6\34758\8105pS\ENQ(iV\US\DC1z*\f\v\DC3\1069515\&9(fu\US\65772\ETB\STX\54153\135979\&1V|,\ENQ~\19729\STX\19820\41682'&\ENQ~&8\988802\133353\113668\&2s\50547\94496P|\997977\186768_J\n\FSL6\USW\141111\47503j`Qm\145265[\180754AK\"%|\138706\ETBVy:\994068\1090571\5558\50270\b\190046{LV\SYN\1042074\ESC\1044886\1058091\1093471\1090324G@\SUB\1036999\1082216\GS\\(@\1075721\1045984\46012~$X\EM\EOT\EM\SYNv\989384;\EOT\93031EzM\SOH#0\nK\996667<,\134094;OW\55013\12595l}\1031753\993922I6=\999006\ETBF!\96242\94407\25440\58898s\1029585\1039757&\140230\1016657U\1049970\1027026\153467\ESC\176702X\1081456/\DC3W 5\STX\1107277\NAK\ETXJ+2J9(\166758\&6z\180991\1024610jH\99681u\RSi*tx$\RS v\DC37\GS\1111321;\b\1052394$\1073049MFg\1089702\SUB\NUL\SO\161302\DC4Y\SUB&\t\NAKu]\ETX\184353.t|"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))},UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), ucStatus = Cancelled, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T17:09:17.160Z")), ucMessage = Just (Message {messageText = "\141795\133926\RSA\184203\DC3X\120203L\160536\161852=@\16358\119362\STXy\21866}k\"\ESC<%\ENQ1\96001tAj\1085570XG\1076633zw\180915\RSMm\bl.\1007126\GS\1063022zXx\ETX\54520^\SOH\992570T\3649r\1012984]kp\SOHG\r\DELTl\NUL\39208D\t[9-\996673\SUB0pF<\ENQ|\1032932Dj`\SYN^\166690:2H7\DC4\DC3-\156803\26279\35286\1058092}<>e\EM\FS2W\12808\26346\1091218*f\98110I[]+\182811}LC\RS\STX\1032637\145853\1064217\&3\SOrMvem\SO\92187\NUL[Y\176428p\t\DC1$#\988339\DLEz\63341s\64068!\DC4?[6nd\STXUh!0\98497\23575(J\96922\20979\&9:)1&y\167624\DC3\34570\DC1w\48382\5954Si\1004593\US1\1057009=\DLEc\fV/IV\SYN\32364\165429y$md\1030373\155082Ma\ETB\1057166\30676F(\161471\DC2\US\EOT\f7"}), ucConvId = Nothing}], clHasMore = False}
