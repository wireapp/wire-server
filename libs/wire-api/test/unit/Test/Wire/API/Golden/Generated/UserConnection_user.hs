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
testObject_UserConnection_user_1 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000002"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000002"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-04T02:44:37.716Z")), ucMessage = Just (Message {messageText = "\1012201`"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000004")))}
testObject_UserConnection_user_2 :: UserConnection
testObject_UserConnection_user_2 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), ucStatus = Blocked, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-06T08:07:09.194Z")), ucMessage = Just (Message {messageText = "\CAN|n\142404h]S\ETB\DC3qL\1065515\nI2\SUB|\51087Lt\154054\1103901\176335\135173,$v\1041848\SOH1x\SOx\CANCD\ETBK6\38598i*!/xl\1078136\RSc\1078406\DC2eB,(F\vc\SOH\GS5#\1008519jt*\17036\&8lSp7ih\166434\157243\NAK\1060877ww=\127520D\145144{\rx\145524\"\1043535\GS)\77929){\18242\171685}\USY\52569\1082492Ub\1065787\a\1002490/@\174722\SO\1023718\&2\48831\USl<RUY\144545S\\3\DEL\f\4418 gy\1072024@\161792_\1095972)\SYNz\1110269mR"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000001")))}
testObject_UserConnection_user_3 :: UserConnection
testObject_UserConnection_user_3 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000400000001"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000100000003"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-05T10:47:06.954Z")), ucMessage = Just (Message {messageText = ")7\v\184125\1041649I\1106155\DC1,\STX\RS\1045016\1023907\97575o\191160\GSr\32008-\DEL\1063218?\97221l\DC3\SIZ.\101076{p*]\FS{+~+W^\1088491\987339*$<0uJa\DC4\SYN\121365\v\STXY1Xip\1007743\SO\30108I\r\48867\v=YL\13725=[F\73809QJ\f\ACK\EOTl\59694\&0\NULeb3c\GSOLfb\RS3\12769u\52286\a\1000841M\DC4\a\CAN\SUB\1045636|vY@=\1079074Q\EM\DC3N\991040?\24646\SOHm\137462&^j\31254\&1\20439Jv;;]4\ESC\1050203\1030964ZW&\96298\ETBZ\38798{w\1072817\1000993\&7&\145419O\73048+Pm(U\1067712\ESC\142024Q\US\SOH\STX\DC1\1045126\DC3\153171\1029652\r`\146572\1023865\&1d\54275q\aJ\CAN\DC3u9\SO6\DLE"}), ucConvId = Nothing}
testObject_UserConnection_user_4 :: UserConnection
testObject_UserConnection_user_4 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000100000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000400000002"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T15:02:13.442Z")), ucMessage = Just (Message {messageText = "\1017275\121188\DELq\1069692\NUL-Frv\EOT>\\S\1090311[F\r\99896X\160884=&r%x\SUB}t\995900^\DC2oI\118978\t94U\DC2\69772\74068.@:\154204NJ\DLEeiL5\DEL&O\1111330pWnwC\n\1619\SYNY\1104226\1011987`hn\152717}7\RS\46658p\GS\ETB_\GS\190129.%RN.;\161300\1061358\CANWi\141176\1082822R\1058951!\GS6\1061545$~0\nE\182814rs\36614\&1\SOH<\1014699\7021G\170810\v|*U\ENQ\FS\f=8\186972\1004442\1029743\NAKW{B"}), ucConvId = Nothing}
testObject_UserConnection_user_5 :: UserConnection
testObject_UserConnection_user_5 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000000000003"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000300000002"))), ucStatus = Cancelled, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-12T17:49:39.893Z")), ucMessage = Just (Message {messageText = "\DEL.\STX7![I\3881\18930h\991217\DEL\fq\"*\63481Z'\NUL\DC4| \160343\r9jIGx\43926\64340.a\STX\1019094\SO\ENQ_>\1091677E\SYNM\45443\1027462Np\t\1042956&H{\DC4\96409\vT4G\DLE?\165016@h\1010956\aXf\t\5531\DEL\1030581\&0FkI,t1\FSC'C\1084179__\35153*\1097658\CANXN6yS\RS%JFdP\b\"H\169671\166238U2rLa,\SYN>\53559\&7Q\1108753f\1086370\&0\182337"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002")))}
testObject_UserConnection_user_6 :: UserConnection
testObject_UserConnection_user_6 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000400000003"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000004"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T06:02:32.356Z")), ucMessage = Just (Message {messageText = "\USz\GS\396r\NAK\DC1\aX\US7\EM\1039616\167894\&6\DC1\180526UJ\173258Xl\152115N\1109876Sv\DELGqFm\SUB\RShIV\DC1\GS5qw[Pi$\177927V\26122\&2\DC3\27662\51747#I\t'\ETXDE*4A;\ACK\DC4\SIL Cg\1045265N\998365\185205\1030493dS'[G1\NULIF\1035028xR\2215\FS95"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000")))}
testObject_UserConnection_user_7 :: UserConnection
testObject_UserConnection_user_7 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000400000004"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T01:44:35.303Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000400000000")))}
testObject_UserConnection_user_8 :: UserConnection
testObject_UserConnection_user_8 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000100000004"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-13T23:36:59.337Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000400000003")))}
testObject_UserConnection_user_9 :: UserConnection
testObject_UserConnection_user_9 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000400000003"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-13T06:42:06.966Z")), ucMessage = Just (Message {messageText = "H\DLE\1060611\DLE40\184136;Q\SUB\120030:L.\1045322s/|\1057536\1006966\SIa2k,\1051400Z\ACK@\r%JE\1064127\NAK\DEL\187297\NUL\985181\20517\\\1030273\DLE\26346Y2\GS\170764\3330\1105673Z\176639sb!\1107640p\SYN\DC2\1031352 mQ\1023988\50551ER\GS\EOT\1020506I\RS*\DELRn\1035554~\ESC\37641\ETXLii\r_\t\ESC\EM\64437L\144948j\EOT\184842T\188590#\SYNj\SO\1068020O\136127j\1037983\SOH\120183d\b\17269\NAK\f\DC3@L\t}\a;j\DC4E$3MI\38311*+\1083109fK\DC3;,(\STXse l\ESC\21915\&9N\US\SOH\ng\1080286\&3\1051943\SOH#\DELO\1018252M\166468?m/\SUB\SI\n\98363^\1113830v`a"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000")))}
testObject_UserConnection_user_10 :: UserConnection
testObject_UserConnection_user_10 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000400000003"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000001"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-06T13:37:58.216Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000002")))}
testObject_UserConnection_user_11 :: UserConnection
testObject_UserConnection_user_11 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000004"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000200000003"))), ucStatus = Cancelled, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-14T03:50:26.986Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000003")))}
testObject_UserConnection_user_12 :: UserConnection
testObject_UserConnection_user_12 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000400000004"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000000"))), ucStatus = Cancelled, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-06T08:58:44.965Z")), ucMessage = Just (Message {messageText = ":6;\177898\ETBc\\\ETXB\NULN\bJ\v?8\1020775T\1013362pZ\1107252L\190759m\SOHy\ACK\141330D\1037090="}), ucConvId = Nothing}
testObject_UserConnection_user_13 :: UserConnection
testObject_UserConnection_user_13 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000003"))), ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000300000003"))), ucStatus = Blocked, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-05T18:46:46.244Z")), ucMessage = Just (Message {messageText = "3)1FH\DLE\b\GS\NUL\1095112b%\ETX.\ETX{\988255?"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000")))}
testObject_UserConnection_user_14 :: UserConnection
testObject_UserConnection_user_14 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000003"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000300000000"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-13T11:44:01.585Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000300000000")))}
testObject_UserConnection_user_15 :: UserConnection
testObject_UserConnection_user_15 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000100000004"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000300000002"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-07T05:07:47.899Z")), ucMessage = Just (Message {messageText = "\EOT\1075830\ESC~!\132298\b@$T\121310\33358I\EOT\186283\66493\42223\22701\&4\US\145849VHQ\vm{p\63446,\186918OQ->YzU'i\38032\1042085\27979Yl\183799\32950\40942\&1&E\158883m"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000000000004")))}
testObject_UserConnection_user_16 :: UserConnection
testObject_UserConnection_user_16 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000004"))), ucStatus = Cancelled, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T22:44:36.089Z")), ucMessage = Just (Message {messageText = "\1323?\FS\44941\&13q1\ENQ[e+`0[X\995938\38716\142041*YP\FS\1006286K\168717\998819F\150111oZ}|]\1001753\64793-p\1026029A1)0\170894\&0\143094\ESCZ\97795\1110569\&9\rX\1098086\990504w\82997\ACK\15870\22808\58601;}"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000004")))}
testObject_UserConnection_user_17 :: UserConnection
testObject_UserConnection_user_17 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000400000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000000000000"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-05T02:25:47.039Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000003")))}
testObject_UserConnection_user_18 :: UserConnection
testObject_UserConnection_user_18 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000004"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-14T21:30:54.596Z")), ucMessage = Just (Message {messageText = "\983222\b]oj\NAKUn\145439\19354=qS\SO-t\1058682_\STX\1019006\1047194*%\160420<\EOT\a\SUBUH\\\194928\CAN|0\1071988\1038716r'}\406rx\ACKvQ\26814\&2g\SO\b\1048907\&8c\DC1pZ!QPc`x\1019623\NAKjn\1036581\57808\b\SYNfX>\n\\fD\CAN\DC1UL\RS\DC2\ac\FSm(k\n\STXh\vq\DLE\DLE\b\1066895L\182124x\95246\188975!\ETB6\fn\DLElvl(\1100258h\1049923\71430\EOT\1100445}\1062416\DLE\179026\1054077T\SO!p\SI\1054333\&3\DC3-Keh\SOO\145607~\STX\48585]\CAN\997733H\EOT\182077\1018926x'\148520\SOHPop\t/..\170120\1033307\162774\&1\68491.zY\EOTH)\1079517y\"b\182566>H"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000000")))}
testObject_UserConnection_user_19 :: UserConnection
testObject_UserConnection_user_19 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000002"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-12T08:15:25.707Z")), ucMessage = Just (Message {messageText = "\178372\DLE3\29709\ESC\FSD*#\ENQ\123150:e\ESC1|l\SYNm\1010683\SOb\1090484B\170715\1109967UrUQ\1013531Vh\v\1030289*M\EM\167892?NB\7124\DEL\NUL8\26527[\SOwS\f\US\1038503\187084\92468\ETX[\1100959\1046899X\188547V%\989396\165309\151204\SUB\1031953b\1102749%\\\1067644\vL\1058149\1021172\DC4X1|\31408\&1\190925|\ENQ\144468I:`\1100903PhV\CAN\145264i\ETB\"\1037463x<\v/Z\42659N5\a=\1045499\33818D\DC1\1035359\167316\992991R"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002")))}
testObject_UserConnection_user_20 :: UserConnection
testObject_UserConnection_user_20 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000300000004"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000003"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-12T11:05:27.012Z")), ucMessage = Just (Message {messageText = "\141652\a-\1018394mJ3\UST\\\1057575'\ETX\13681+2I\RS-g\9368&T~\STX=]y^\GS\173741\1023366#\1032571\41897z\1018614^\1006703d\1040022>\DEL\1113192GJ]\DC3\18446\1060798\&9"}), ucConvId = Nothing}
