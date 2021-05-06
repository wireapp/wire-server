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
testObject_UserConnection_user_1 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000300000003"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-10T13:53:09.067Z")), ucMessage = Just (Message {messageText = "\998563C\49061\n@\1093343\61627R\99804s\169882\DC3.$c4\1087207`ZL\US\188106E~Sd\"H}1\38619\159278'\48547\ENQ2\59200\&4\DC4&\"\94797C\1043172E\1019459R~[|\168714&D$\1102428\&5\vv\180027\ESC\ACK\r\ACK4\1007459\RSj\189658L;$9\5425B_\NUL2\1000727\1055426\138552\DEL8B0rt\18619|,\1056024,S9\1014793KS?<u"}), ucConvId = Nothing}
testObject_UserConnection_user_2 :: UserConnection
testObject_UserConnection_user_2 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000300000002"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000400000003"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T17:36:14.789Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002")))}
testObject_UserConnection_user_3 :: UserConnection
testObject_UserConnection_user_3 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000400000004"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000100000001"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-06T08:48:38.449Z")), ucMessage = Just (Message {messageText = "\148309\DC2\NUL+\DC4Ty\147715\&9\a?\984279\">\1004037aHRH\1014267\&8m\1055731\1113087\1090638\NUL\168987S\1076564\148348\1043263Y/8f\155783\DC2JG\t\1051144\b<\rR\1054382dx{\DEL\1079787SL+X\CAN?\\\n\DLEH\1010873;\166330DnH,*\SYN\1073236\1106031\&77&\1023731(\985068\SOf\175443_;\1017091\"\999523)Q\ETX\SUBh\64890\b\ESC\RS^\146966q\SYNrR&\22693Wm%\47886\US\985574gP\SIp\1110852Pz\1000237/\ETX|jSo[\988423d\78874\a\v0kJ'=\FS\ACK{Z'mD\FSQT\\p\1064411xT0\1023719Q\37139|A\50926\GS\162280[\EOT:\vv\DC2t=\b\19182\1061477\&2d\4377m\1066021XT}\1072302\177785\DC4xk\NAK\EOT.\RS\SYN^K#P\SI`\SYN\CAN%\EM\135710V&\184251\95824\7440\&6!Q\995463B\52804\991645\13958\\2Z0's\988099\ETXw\NAKr\174913\59449<m\tjMtvP8\1094606QheX=3N\30246\&4>\ETB\US9R\RSt^y\ETB'\991881_"}), ucConvId = Nothing}
testObject_UserConnection_user_4 :: UserConnection
testObject_UserConnection_user_4 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000003"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000002"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-07T16:47:14.760Z")), ucMessage = Just (Message {messageText = "\65720\&6\RS\1099866_\DC3\DEL\n\DC3cz\EM]\125268\EOTD,\1076405\US\STX\SO\ACKfi*\US\tz.\"Z\127111a\1030526E9_\"qF<\GSGj\1083243 \ETX\1089221\136334\"\\\33097AA#\1029709\1063411\1026070dk\STX\137038lNj\14414y\r\NULLCa8On\1056985\SI^t\1105980RA\1059570\1076245\&8v\2301\1109568\n\1065370\1044394tt:\134589l3DGQ\137197\SUB\SI\1037921\DELN\1099800\f-\1096513$v\1054469\DEL\"}\133042\68090<\1034826\DEL\118829\96083\&5G\ETB}S\119101\19090\&8+;"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000300000003")))}
testObject_UserConnection_user_5 :: UserConnection
testObject_UserConnection_user_5 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000004"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T17:53:02.682Z")), ucMessage = Just (Message {messageText = "\DLE{\30649k)\US\SOHC\v\100780w\1092190i\163297\1108260vg%\990198\40734\1985\148524X\SYN@\ESC\USY\DLE\\ez\RS\RS0\SO\ETB=\158444f7"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000000000004")))}
testObject_UserConnection_user_6 :: UserConnection
testObject_UserConnection_user_6 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000100000004"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000004"))), ucStatus = Blocked, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-04T19:49:51.363Z")), ucMessage = Just (Message {messageText = "t\1016580:\DC4|\b\110651{[\43825\ENQ\1092736I\f\DEL\121070@#\1056394\12152\USNC .S\DELfr\150636}\ENQrl[\132606)\40258\94474\a\DLEAU~\1045860\1069414\&9\997592@G3\SOH\1004441e)\1016865\FS.\991277\1069998n+\DC3%.L X\99998e\1057002/5=\20889w=\1066840\1098062\&6(\SOHr\38389Rz3\tqy2\1105755_)\DLE2\100723T\1018790\&0,)\1080653[P\STX\1041078~\DC2\b\ETX!\59933\1069683]\GS\22073\NUL9[\n;yV(@\rO\1034740\125013({\NAK\SOXQ\64345g\1109508.\\n9\68252f\154403\DLE\917598,[:\ESCe0\SUB]\995177\&78\988407j\185283JX[h|\1024496T?\998941\v\1007316\SUB\1043304\33286Wn#3C\1107514\t8\f\vf\1011473dY\SI\92194,{\DLE\170490XS\CANA\n?(k\NUL\ay\37471\29186s|! lf7q/\14111\1016958ZLY\US@\1099342m\SO\1065576.z\r\ETX\f\997302\146835\59062B\154707\989163"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000100000004")))}
testObject_UserConnection_user_7 :: UserConnection
testObject_UserConnection_user_7 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000300000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000001"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-07T01:07:57.548Z")), ucMessage = Just (Message {messageText = "\997921>F5izs\23285\RS\ESC\DC4S\1083208\994816\&3\96291\38737\RS\1010063\180987n\1081979\5464\EOT%/\1070495H,W3\SOD\NAK\297V\1035812T={\NAK\6793\DC45\SOH3^\DC4\1008060\&3\95447\1044982\70743\ACK\CAN/Z\1085562-\1090342;mg|\1108486%\135819'\1082282h\n\\\a\USZ\189113-9\a\SI\993326m(\140873\1029053OU\140251\161200azWo\96592e\74282\DC4\NUL\FSuhdA\44544\1026662\1046094\ETBT5\aK@\GS\25457fi\DC4\ACK\RS\185728Xy\66708%\186150\68912R\r\993320\ACKb\NAKkKD\177644\ENQ\r|\1030335\f#4\bq\CAN\36442Va\1099593k:\SOHg\187151\ENQ|{6\13678\&5\STXD[\NAKx\DEL,\175603\263\1103137^RkW\176614\DC2\"$Bcg>@\1050195\DEL\1015801\69610wdp%\1060412h.1w{^\78743Q@\NUL\CAN\NAKI\US\\5\150998\&7\DC3Dc9\1031385O<5N\US\STX$\USB\1075036\SO\NULq\1038809\&6]\ETBH\1076496\173824"}), ucConvId = Nothing}
testObject_UserConnection_user_8 :: UserConnection
testObject_UserConnection_user_8 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000300000004"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000001"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-05T18:53:22.802Z")), ucMessage = Nothing, ucConvId = Nothing}
testObject_UserConnection_user_9 :: UserConnection
testObject_UserConnection_user_9 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000400000002"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-13T08:20:25.671Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000003")))}
testObject_UserConnection_user_10 :: UserConnection
testObject_UserConnection_user_10 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000200000004"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000100000003"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-13T15:10:54.007Z")), ucMessage = Just (Message {messageText = "\ETX\1095475\&83\986732O\NAK\DC4n*\992735}\GS?!&I\1085086_c\DLE[\1059932fyI\t\42791F&\GS*Shi`:s\1089855\1076759D\1056026Kt\1060205E\132099\1112293\NAK\SI\ETB=\1033282\32462D\1003979\185028A\1083974[c\1057052<t?W@#+v2FEX[tB-&\1079846I\DC2\tY\1030023\1030546 \1075356\179803\1096200H\179082!\SYNu:\t\DC1\SO\60185\b*r%\1016834\DELg\6383AHE^\139654-\SUBJ\8760J66J\998982\83029t\ESCe\1000402\SOg9\DC4\vx/\\ih\SYN7\ENQ\9855\111244\US\ACKM\DC2lcQ\SUB\DLE\23237\147693r\1099681\1088162j\EM\163901D*y\1061144\993980\&7\1047356\1020835\1059445+wA\FSU\184940\&7\33970V\SYN>h<\n\94975\v\NUL2I\1035461g\SYNB\1096375\DC1\SOHr!3>\SO\CAN\DC4\f\DEL3BL\DC4\181924\USr\1011152\993789Q'\f%]{\1063819\t\54218/\134961\EMS\ETX?(N\1109377"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000000000003")))}
testObject_UserConnection_user_11 :: UserConnection
testObject_UserConnection_user_11 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000004"))), ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000200000003"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-12T16:11:25.805Z")), ucMessage = Just (Message {messageText = "\1043108;k\1060085\&5\GS\1018480&\1079538\96545BK{!\ETXl\SO4\bhd\EM?\35347\SOR\tZ\SYN\1015600E$&\179490\&0L\CANHs7xE\1086446\a\vOhd\75041G\FS)[4jy\998448\1109742|XQ\n\NAK\tEm\1043245\1053583ngK\DC2RB{tqH\"9|{\\\SUBi+\1077893\1097749O+9(v\SUB=v`\DELg\FS9\SOHb\1088260m\EM\72409t;\65610\&8*`\100050qL\1071702\1098675H#\1089792\r\984056\ENQ.\1074470v\SOHF[AD;{%&P\131340\1089396|]]\1031969\154501\5130\989394_\ESC\r\DC1f\1037398u1\1003829\7507@H\999667=\1044611\128922\EMu=J\USM\1007092(\STX\998706\162610_Q\v/CY=2)a\EOT9W(\FS4\bNSfh\166659\1087238\EM\1089739b."}), ucConvId = Just (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000000000000")))}
testObject_UserConnection_user_12 :: UserConnection
testObject_UserConnection_user_12 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000400000000"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000003"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-07T20:31:48.899Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000300000002")))}
testObject_UserConnection_user_13 :: UserConnection
testObject_UserConnection_user_13 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000400000004"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000300000003"))), ucStatus = Blocked, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-14T10:32:53.999Z")), ucMessage = Just (Message {messageText = "\39740\&8\1070237\1112784fw!>^\FS\1009509\101033S6}z\DC4e/y\1104041~\1091359bmrU\46895\189683~*\DC4X\SO\DC1\STXJ\1103660j\994936O_Nx\DEL\140218&\162482IQ\NUL\155388\v\1088109\&6T\DC4\ESC\SI\1064553\190972^ue\1044707`\42278\34483J\DC1g|\STX\DC3\GS'\1050988j$p>\27459\SOH;\SI\1085284\NULJ\155896\&6\134257W/\53849Y0a<\1063611p\24579h\61906Y\SUB]uj\aQ\1015220Rd\r\SOH\SOHP\NULd\DLE<\171496\NUL2O\1090005\157081\SUB\94233\ACK \1019023Q*\ENQ7h>\FSmb\ESC\ETB(G}\ACK\SYN\159696XMsq-\NUL\1066635\183799:Z\141424\ti7|\1101996\1015103\ACK#\EOT\100818}E\1025879\1083977j\170192\186665\NAK\16058]\1064841\STX_f\156289\&3\EM\ENQ7\r'\986980Kv\NAKZ\GS\30905\151063|\1077235[M#?\bh\DC3\ACK\DC2\DC3\EM1\DEL"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000004")))}
testObject_UserConnection_user_14 :: UserConnection
testObject_UserConnection_user_14 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000004"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000400000001"))), ucStatus = Cancelled, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-10T03:07:56.165Z")), ucMessage = Just (Message {messageText = "w`F \SYNT]3\ACK\178629\1107444\20417\tRr7:R\1101528\1070118:\983204\1006018\171761\95020\CAN\191411\&4\1025287e@g/\SUB}\1070022\ENQw\DC1{JFo\42372CS{Q\1064975H\\_\EOT\NULnI\4124\144705Q\1040199\&7\169975\SO\ACKa`=oZ-\989631\EOTC \t\1088240\1074969\1099726\1041819/}f`\4400ji\nz\142202\180691x\1023582qrX\1021974\58301q.&`\25839\78822\138037\ACK;6\SO\t\119061\&3lTz\b\ACK\DLE\123194\&2\NAKB\1021594]@M\DC4z\33759/\1097269]!_\RS^\1059721\ESCV\t\136621\&2[%\13507ks\RS\190735\1105437\FS,Q\161824\f\1110185\33496\1093276h?\EM'\149939z\43506M\1066113\ETX\1030340\NAKD\SON"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000003")))}
testObject_UserConnection_user_15 :: UserConnection
testObject_UserConnection_user_15 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000300000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000003"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-06T13:55:55.039Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000100000000")))}
testObject_UserConnection_user_16 :: UserConnection
testObject_UserConnection_user_16 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000000000000"))), ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000002"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-04T03:46:41.873Z")), ucMessage = Just (Message {messageText = "f\1018188]\\\r\992056\1068226\134457\DELHB|\1046340\21894\SUB-k\30768@Yi\NAK\FSo"}), ucConvId = Nothing}
testObject_UserConnection_user_17 :: UserConnection
testObject_UserConnection_user_17 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000004"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), ucStatus = Blocked, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-07T03:42:01.252Z")), ucMessage = Just (Message {messageText = " \SYN\EM\134392\1104547\189653\ENQ\72352VE\f*L\ESCe\158219\SUB/\rd\1081958\139816\vAsM\49731>ni"}), ucConvId = Nothing}
testObject_UserConnection_user_18 :: UserConnection
testObject_UserConnection_user_18 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000002"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000004"))), ucStatus = Blocked, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-04T16:51:38.989Z")), ucMessage = Just (Message {messageText = "j6G\DC1\178014\55237~Qw\1048438/\NUL\17431@(q\DC2\DC1n\r\1098735Fv}|g+~$l\EMp*cW$\SIp\SYN\DC1\STX\t^\1034911\1093099\58744\1093740\&8AU!=Lf\984162U\a\159823]\SOx<F\"\STX\SOH\1005391g3w9\STX\187810)YHfU\SI9\SI\1026016\SOH\1095185hh\EM\54691)L\20291o\1106610\98355\189996?P85f\SUB\97468I7\25978~|\13869u\167040y\SI\f\1065809\SO0\ACKN,`X\57874\EMK\SUB)W@\1046480]\8477Yw\184647\1020578\10516(\1093771X\\\SIg0\DLEX*\68162\63931f\1004701#D\"q\ENQ]on\SI\EOT\DLE0\US\148296\STX/D\DC2\ENQ0\94684X\98727\SI\SOH\tq\a"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000400000002")))}
testObject_UserConnection_user_19 :: UserConnection
testObject_UserConnection_user_19 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000200000002"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000000000001"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T19:32:46.389Z")), ucMessage = Just (Message {messageText = "Q\DLE\\$\STXk|6c\ap\171271\\\26285\38992,\54262I\42563m\1053053Tft\r\96167\1068154Hn\FS|\NULj\171147\GSO3\1089862\1060861\DC2_\STXYy?hgvXC[\RSp&\1085632\&8\1079300\&0h\159452\1083488f%c6^h\1021512\&6m;\b\1057384\USP$\1080152\US\f,'\991412\1021143p\180066\1001436\1065600\&1Wk\1021670[\1073343\1016507@j\SOH>\SOn)\1006039')@b\74151\f\t4;\92380\CAN}j~\54735\1040426AP\1107611ng=\147819A%\a\a\1102294\11623\183101tEl\187570[@a\139057L\\7BE4iP\1073019\96390\US\DC1G"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000004")))}
testObject_UserConnection_user_20 :: UserConnection
testObject_UserConnection_user_20 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000001"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-05T05:24:53.186Z")), ucMessage = Just (Message {messageText = "\DC4\DC2\EOT5-^\DC3\63282a\1030642\72881\nIYJ\1038800'\ETB:Q\n!\r\EM\r\110870-[\SUB\132441\SI\b*\149664\"\n\1016887\149040\156295\GS%qLJ\aB\1067886\SI\STX-(\1014063p\98338=*5J@uP\DC3 \1058525q\1021529\&4\SYN~\1110155.F\1048189G^*\131275\US-\CANp4\SOHA\168351\f7\180797\ESC\EOT{i\119298\1033065ngk?j\ETBVKi_q5\DC4tC\181277\DC3$\ENQ\122911c\35568\bS\1070673\&8g'/'M\986467\28217\1073389\ESC<\DC1RD\111300\48441B\r9\n7\SYNHr\163991\t\121112\&8|kx\1044246x\137262]yW\1101129\1110597\SYNL\US\1041011WS[\EM\1049725\172661\SOH9g\136035G\"_Y8Z\1097117\14865\acF6I}\b>\1033292G68\51669\DEL\US7\EM\1059535LBW'\\\28406\1057356e(\1075502uIz`Z\67099\1107484\SOn\1051647I1\SOH"}), ucConvId = Nothing}
