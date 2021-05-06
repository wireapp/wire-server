{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserConnection_user where

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
testObject_UserConnection_user_1 :: UserConnection
testObject_UserConnection_user_1 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000200000001"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-07T23:44:44.651Z")), ucMessage = Just (Message {messageText = "R\1015761{\111221zhh\RS\vL*<\1050680\25004>d\NAK\1046250\SOH\1103621\SO&6\120020h:\1052841\1111163\143008<\1073613b j\SYN/\180709\1058496\1065166&uIlEi\SOH_$\f\ACK),C\174439p^\DC2wo\r\DLE\65179=lf(|\EM\43837\EOTo\RS\985000\150887D!\tDyw\139177s.\DC3\71338zB=I%4(\986029#\1095581\&7pMsmk\161505\61215_\160596xPzc\ETX\RS\1076632\100283\USr2 \USO\143950"}), ucConvId = Nothing}
testObject_UserConnection_user_2 :: UserConnection
testObject_UserConnection_user_2 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000003"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000003"))), ucStatus = Blocked, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T11:58:44.851Z")), ucMessage = Just (Message {messageText = "\185871v\175144\SOK?\nD|,l\984396\38742oz/os%-<U\NUL9R\fY\EOT\15631\EOTHK\185620\&9v\GS\ENQd\1036510%\1009459\r7_Q6X\1099208b\187544\98491.Fl\153769\US\1078242\44281-\RS\987743R\999142f\SI\19515\1104403j \DLEk_\STX\az!\986190W8fX\51307\SO$0\"1\1017297\ENQ\ACK\n\ACK@e\DC3!\DC44)\64506\ESCx>jC:\150122F<Ms!\1004358N\1012229\SUB\171354\983456\NULe<\19393\n\1070294Bi\131559\78564^\SOHY\149213_\190039\12840\155443\&0B\1032234 \28111\26131\2109A\171715F(kQY\ACK/-_\rvM\1001268;\FS\v\\5\ETBe\1045631!x\45006|\ETBzB;\FS*v@\SI\1000241vJZ\STX\1072192b\62337\NAK#\158784Y\GSC\vJ\37521b\DC3\999272Hl\1037606\SUB\DELa\52806\nrEw\50419\156034$\1060279\9794H\1111923\nb\ENQ_4rAe\1013556\134792kgU\994889\SOZ}"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000300000000")))}
testObject_UserConnection_user_3 :: UserConnection
testObject_UserConnection_user_3 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000000000002"))), ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000300000000"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-05T00:15:55.349Z")), ucMessage = Just (Message {messageText = "j\CAN]\99587NUF\SOH4\146817n\1089574[u\FS\1084951\143625k\rJ\1102664\1042980\ENQ\ETXVp\DLEr{\EM\22851z\US\DLE\991364\12756\1010174: \ETX80\1087564u!yC\ACK\SOXp\143484\99139$e\ENQ\1064940\&5UU]\DC1\NUL\b~;\DC4Ec\SYN!\RS/f\27773O\1007025\1039102\&3\tbj9Odr 7\\\1081930s\992915\&7\SI\1070761)\1089683\37712\60877\&3W*\182332RB\ETB-Q\186044\1012467~/\ENQ\DC4`\6544]U\STX\168227\1076060:\96735\ETB\1012764i\46650\1066647Yf\STX{H\DC4j"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001")))}
testObject_UserConnection_user_4 :: UserConnection
testObject_UserConnection_user_4 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000100000004"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000003"))), ucStatus = Cancelled, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-06T20:21:25.333Z")), ucMessage = Just (Message {messageText = "2^\1092963 I2puv!S a\ENQ*\EM\1109269C\EOT1/o\nqG\5014li(\57872jZ&]'\SOHXT\36821DMx15>.\139396\63635\160631J\DC4\SO4'[*f8\22114\DC4(VGbK6\120555J\1062082q\1108514,S=\1079304\NAK6\176403ip\995002dq\a\nj\37079S~m\176942\1051390:"}), ucConvId = Nothing}
testObject_UserConnection_user_5 :: UserConnection
testObject_UserConnection_user_5 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000300000003"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000300000001"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-12T12:44:47.711Z")), ucMessage = Just (Message {messageText = "(\138942G\22764\SO[9D=3\EOTq'&\119898\127946\ACKS\SUB\1091790S(~^\b?=hr\t\FS\23528\1041489\994784\&7/\96056s}\bGf\1005176zx1}-*\1041965\"s\26381T\140407n\40646\ETB\10647\DEL\140551\67210\10564\1046162k\152807A\1101883h\1009549k\19383\1036696\ENQ\1091600\n\989674A(,1\146894U\ETB|\1013718\19107fN\1104897\149061\1021689\129555\131352X"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000002")))}
testObject_UserConnection_user_6 :: UserConnection
testObject_UserConnection_user_6 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000200000000"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000002"))), ucStatus = Blocked, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-04T17:22:36.717Z")), ucMessage = Just (Message {messageText = "\ACK[\1057703\1074628\&6.\ACKn\SYNT\31629pX\v\tJ\NAK }`\151464\n&\b'\EOTd\1045115\1041219\1073183\ETB\19576\DLE\aQ;:\1038850=u\GS\155086\158119H\52384\119613\&3((5\US\17488ta\"HQ\47327\v\1021505\&3\EM=\40269\1084718\ACKdNu}4\1060830f~km\1104840\USfh\32128q\129291\ACK\SYN\DLEO\19444~Y[\979\US$H\NAK\DC2:\1015227\74082\40226E<r$\GS\60943\no\119967\174128vx\5669w\1030934]\60647Ib\STX\ENQ3\1036270\&3TP\fUJ\ETB"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000004")))}
testObject_UserConnection_user_7 :: UserConnection
testObject_UserConnection_user_7 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000300000002"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000100000003"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T17:42:50.058Z")), ucMessage = Just (Message {messageText = "}c\DC28O\EOT\998922g\173300fD\177753\&1F\DC2BB5\EMW=]\33572\1039975eT.\tcZ\US\28173(?x\DC2\990109,Rp$;\188835\DEL\r\21826\ENQ$2L(X\159148OA4\1091078\1001903e^=Z0\EM=\SYNZA\1102827A\vb5F8\29111\63998%^V!?G\1216@n\SOH]\15538\42150\&0'Cs\1042640\1104896\t|6a*>\1097892\ETX\1058909c\ENQhs\1105649\&5+9=\DEL\1095620J%C=\f6)\f\b}\993847Mm#cghw\29656G\CAN\1092279E\\F\35667G\31213,c\tl8].@A\1084679\bL\a<\"b\95391-rz\SUB\"zx\1039155\145454\US\t58\\fgd?\1109973(2\1043218\6566.\ETX\vy\983887A\DC16[\15627\DC3\128484s\132281\145349k\ETBM\182304ve\ETB,hpV"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001")))}
testObject_UserConnection_user_8 :: UserConnection
testObject_UserConnection_user_8 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000400000001"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-05T15:37:42.521Z")), ucMessage = Just (Message {messageText = "?-L\CAN5\1022272\1096698V\10312aI\1091346\1001092&\SYN\1031464\f\DC2\62113\1107576a\DC11@\ETB,\136357\&6\US\SI>.zj\1038737\&79L\1081699\172262\95283\ETX\SIQ\SUB)SQR\10495\SO\"\96094\"J\173873\12983\96845\NULK.o;\1092786c U\1091708\&8Y'\1084282b\134461\45265\1107019=\SO\52022\992044\bUxu\FS&n\\Y_HU\26102y#\DEL@ \f\ACKoW2\1056035qy\SOH\b2sT\DLE\EM{\153533\EM\SYN7\\\ESC\DLE0\t"}), ucConvId = Nothing}
testObject_UserConnection_user_9 :: UserConnection
testObject_UserConnection_user_9 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000003"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000400000003"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T02:50:04.147Z")), ucMessage = Just (Message {messageText = "w\9891Az\164580\187603)\1086285<~JN1ld\147514\57736\DC2\169476$\ETB\SIB\161425\51629\&4<HA\1001557 y\DC2\STXc9\64144\1033108C\1039116ch\986241\v6\NULA\99280\NAKOA4\120937\NAK\97351.F\1107177d\61041GO\1110560\SO*\CAN@7\ETBtG\SOH\58315\&7U4\DC4\SIG\SO\vxc\ESC\1027288i)4\7000A9v\1087073\&9I\1015762\132939ho,f:o%w,dB\167490\140386Xw>[{\FSF\FS],nX\bD\23635v\US`^5t\36027~\1088118QNO\45509\EM7.\1005827:\SI_\ESC2k\1041456\159375\101088%\\\132325\40442=z.\996145\1047357vB`\133899+_y\1081761<\CAN>\SOuW\17217G[y\DC4\tY\EOT@\1018791>\20926/\180521/u\EOT\1014510\195074\37176u$\69458\100959v\194699C\1066543\51122a \1052689"}), ucConvId = Nothing}
testObject_UserConnection_user_10 :: UserConnection
testObject_UserConnection_user_10 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000000000001"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000000000000"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T07:41:46.252Z")), ucMessage = Just (Message {messageText = ";]\"k/Oj/7\USRaBFf\1100271\&08: H\SYNhM\1046998<\1062147\RSGLs\EM)<\ETXG\57598[\\V."}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}
testObject_UserConnection_user_11 :: UserConnection
testObject_UserConnection_user_11 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000004"))), ucStatus = Blocked, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-07T16:55:43.944Z")), ucMessage = Just (Message {messageText = "/\1024527KHD?R\994723\138533Y\13802\1071543'\32324A\1026375\SYN\131834:5\128228H'\n\143075\SYN[\n\ENQP\83136k]\1028206m%x6\160911\&19}]9\30577\1036482"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000")))}
testObject_UserConnection_user_12 :: UserConnection
testObject_UserConnection_user_12 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000300000004"))), ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000400000004"))), ucStatus = Blocked, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-13T16:45:04.201Z")), ucMessage = Just (Message {messageText = "Hz2\n\DLE ;&|?\1042040r(#+\1064312\DELmC!$(F\NAK\1105078\NUL\64791C\ESC\ETX\NAK\DC2\DC3\t\14563\SYN\989103*\DC3/\1072545\42455>^m\16594m4 ec\1027015D\1099427\EM\\9\1042932\1074512s-\1048668R \n\f-V"}), ucConvId = Nothing}
testObject_UserConnection_user_13 :: UserConnection
testObject_UserConnection_user_13 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000300000003"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000300000003"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-06T04:37:29.010Z")), ucMessage = Just (Message {messageText = "\172831\v\168777\1108422\DEL\ETX<]n\997527\144500\NULe\183016\156146~:\f'h\CANNu\SIa\28189\1110975\14191\ETB\1002130!]C:\b\EOT\\/R<\54699zz\ETB\1081720\t_SxTQ\5807T*:\SYNnVc.\1069461\DC4\b\f\1073932XE\1083207\167974\58185t/2\1083345\SUB\SUB\ENQA*\"b\DC1{#\1082300)9\41655\CANQ\1025632\77991N9\48698>\DLE\NAKz\22656\&7D2\EM\1007078K>>\1045068(\177231\178449e!%\48827\FS\v\1097235\USC\ESC\DC2\SOe\40117x\DC4,v\STXk\995845)[\1089818\1105073er\ENQT\NAK\SO\FS`v\f\a\158612\1077706.\997787\160615\GSb/\96747|{\ENQ\986308\DC3\FS\989530\USfGrO\1085282G\53956y\DEL\143269\98012\n\1052442~\1029913\SYN:-Y\29820l?yAW\no\1053027\&9b'=f!&;\1037224XWe\bs\1015915\1000300\STX{+5\1079402\SUB\176339\DC3K\120451"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000003")))}
testObject_UserConnection_user_14 :: UserConnection
testObject_UserConnection_user_14 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000400000000"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000000000004"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-10T01:51:00.833Z")), ucMessage = Just (Message {messageText = "\151064\1008376\CANG\1052856H>tAhV\SYN\1066004\1000789\STX\48532\1020308\&338\SI\vf\49081\GSl7ep@h,\ETX\FS\nw6\DC4f^\1027080\58286\RSP&Y\1004965'.-o\33334\153552\&7_ZE\1028230?\1103290zd%\RS6f8\DC1l\1002877|y\1023695\1071146I\53791Q\EOTl\993365EU\RSV\1056007R%?P\1106884-\144887'\DC2goHU\FSV!\1074942;\46926\66664\&1\59521\EM\63842\1055659!E8\ACKX\DC4E+\EOT>t^o*\1029520j/\1058864\133119(Zk\35116<^\US%O\"\141322\DC2\1104078\GSr\992260\NUL"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000400000002")))}
testObject_UserConnection_user_15 :: UserConnection
testObject_UserConnection_user_15 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000000000001"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000300000000"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-13T20:01:35.173Z")), ucMessage = Just (Message {messageText = "\a\167923\169565o\5987\1083574\26847GjJ\STX\1067511TPHb\SI\1056562=\14784\EMr\10333A\148882\986893E`\1017291c\68136\988745\ETB\984959\147909\&56Vi!\1062507Lam7Zje,\ENQ\25822\1017347CYj\179387\tV-7N\998092\1012923@\99902\119260\t8.9\1009444\1069066l9\185465\ESC\1066870\SYNDjxL\167120F)x\DC3\NAKY<M]s\DELG-:\1032243\1048963D/aW\DEL1V*1\DC3N<\60391\vb\20503 ^4\SIYF/]\t.\1062087:PQ\1031163\1062864'uP@\1049230$%\33575T\1031017:\1103911f?m\STX\1061096\146608\&132p0\1025744\1052868\21663\1092650\&6rVzBTD\DC1\DLE8\990455\1036839\b-\996520(<b\n\CANL\"TKKHb\ENQn\1021176\1043127\1059585\46934\1014440oG\179833q\12397u+DH\t+\176907\ENQ"}), ucConvId = Nothing}
testObject_UserConnection_user_16 :: UserConnection
testObject_UserConnection_user_16 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000100000004"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000300000000"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T19:26:31.617Z")), ucMessage = Just (Message {messageText = "t\NAK\175339i6\EOT3D\ACKG\DC2v}\25278sw\64891\1070441\1018873%5x^\125070\n,}\NAK\ESC\SOHTb\\\ETXm\r'V\\\1113464,\7511\&6:ocC\NUL5\RS\1113933 um\1030203\10353-\GSC\SOH\1016888\150610\16509\1018677[\SUBa\1008316\&9*in\bCxP\1023493a9\SYNP5\188837x._TA\1074045nX%;\STX\DEL^\ESCa:Z\139195\SOH\FS\1024506\160640tzu\1094626\120879\&2>\1097012\\\v:2\b\25670>\SOUq0y\166762Cq'J\USY|!\151719s\44598\a\RS)w\DC1o\136175x]\f\1002570\&5\1003367\&8\51936\US\EOT\999871>s)\\<,\GSw#\1081382\393\ESC\fW\1012871\DC3Q\\\ENQ'Z\45577\145744\1057472J\1103537\1080101\&7\4331K1[\98810\f(yJ\167333\38496\1032636p\t{w\1093856\&4\\K\v`.2\FS\184360-)\11276&[\SO\DC33%\1098870\169773%\US\20927\ESCV\STXi0&l;\17103\65288.\a\ESC\1048019gq\189593\SOdh&::"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000400000003")))}
testObject_UserConnection_user_17 :: UserConnection
testObject_UserConnection_user_17 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000300000002"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-13T06:09:04.696Z")), ucMessage = Just (Message {messageText = "(\DC2\STX15cQ\DC3W\194975\74957~T\1041893?\39985BFb\141457\1070603h@\SO1\1074836\f\1090382\DC18\172774\98281N\ESC\136380A\127884Uv(fQ%\146611\45526\DEL\CAN"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000000000002")))}
testObject_UserConnection_user_18 :: UserConnection
testObject_UserConnection_user_18 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000400000003"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-04T06:55:28.905Z")), ucMessage = Just (Message {messageText = "wq]\98012\DC1;\ENQ=fG\"\92687\181656\EOT\92900}|G\1008493\1063191J\140841\DC2\EMd\31369\a{\160504pS{\141671(9J:K\1022726Kh\1104990:\th@k\78534\ETXm\n\45549\996322\19051\&5w\1038767-G\1085123V\SUBrb\139890\142863\FS[8\71854OK6\DLE\FS.sRU\176274"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}
testObject_UserConnection_user_19 :: UserConnection
testObject_UserConnection_user_19 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000000"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000000000003"))), ucStatus = Cancelled, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-10T12:52:05.265Z")), ucMessage = Just (Message {messageText = "\FS2\t\DC1\ACK\\\151754dm\1085301\FSJ\1091764$\CAN\100550?N\r\DC1q3`\148943\FSC7\aPo\1031467#\74853/\SUB\155514?@\195068Jq\US^\145415$2Z|Q!\92606\ETXc\173423\146133\138044VL[\NAK\CAN\1090324\143732\180973\93070\1103833\46480\SUB\985453\ENQ~,\NULKlV>Vt3\8089\34782k8a\NUL2GP* \DC16\1031262\1051811?O6\1070775\128803iH\1035560\ACKxE\f\f5_\1036018G)\60697\991612\31181y.&:\1021261\ETX>h \40311k>t\1093520\DC4hXB\184025\8005|\22867\1045461\1075699\1067310~\1021414\FS\988171\1096413R\\\ACK[7d\184574\1021223\SUB_\31590[?/;'\STXi\ETXrFe;/\CAN[v5L+\1100590*\73713J\fL~r|\1095312\1057358c\1020883\66568\143497 \39141\148033k\DC1\SOH^\NUL\64644-7\62738&Bz\1096901\&3\ENQ[GN'/)b/f\183544\36128\167206z\48690]\128503PJ\155650\6058\1077933H\1024112\162652\&4#\997632Aj>\SO\1004575\16555\DEL\NULYA\r~:\rM"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000004")))}
testObject_UserConnection_user_20 :: UserConnection
testObject_UserConnection_user_20 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000004"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000100000003"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-12T10:39:08.597Z")), ucMessage = Just (Message {messageText = "\ACKxuV\ETB9\DC4\NULH\DC3<\176459U\111033_W\bw%dr-l\1041890yC\1035872\24695\nu\DC3}n&\t\169600\95048L\DC1\96546\EOT\CAN\40092\ACKf\EMA^]\1009910\1021267\141714\1030863=gE\1024345\SYN\fG\1102587r\DELyWF\DC4\vqR\164992G\EOTHZ+mm1\b\ETB\1036758\167496#A\150102\\u\a1\172642\DLEF}\EOTz\135082v3\160903\SYN\1000112z1H\11070\SO\1106821\&9Z\1069765p%G=\173139q+*\145630\ENQ\SUB\74229dmJ?\1008414E@a\FS\\\1051878!\nC\NAK@\1041868Z\r2W\1089302\ETB\NAK\187601\1022445\163941\1009629\1017758D^\3147-5xF\b\SI\r,A/\27242!]]\1061895*(\DC3n:\65887q*|d\955\1024158u*\100141\170307)\25010j\UShKGH\1066738\SOzL\3769p\US\1038981\178940\ACK\DC3\SO\EM#\DC3\165222\47001f^\166893U\110796\147987F\1085514@\CANS-i\189809 \ETX\98485"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000000")))}
