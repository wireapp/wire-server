{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Message_user where

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
testObject_Message_user_1 :: Message
testObject_Message_user_1 = Message {messageText = "Xm0\987762$..^v \a6Y\1084892\1035538pV\DEL)\ESC\SUB.\ETBV\16215\1113663i\ACKH%e\NUL\1051324\SOyP\150934v^3&\176721\1038890iE\59288\RSE\1008876\a(5\1000871\ESC*\1081523p,\USM\b\1026104\&5s1\1015421yP\SUB\ETX$e\1039663\1105333?\150960\DC4\CAN\1037197\CAN\CAN\144213/O\18887"}
testObject_Message_user_2 :: Message
testObject_Message_user_2 = Message {messageText = "D\ETX9\1060918\1020229;6=>>\"_\993383u\DLEBB]e:\8460\ETX?x\48217o\DLE7X\SYNh\1109186\186249\100291\SYN\STX\1061835\995012va+D\1050878Q\FS\SYNCY\1002206,n\NAK\SO\ETB~\1005633^C9\DC36\taJvP\996955A\SUB\CAN4X\ETX\ACK\ESCrD\1009719\NAKH(\1005795SWU9\998540q^\"\1015751F.~Vq\RS\ESC\GS"}
testObject_Message_user_3 :: Message
testObject_Message_user_3 = Message {messageText = "!\1082141\1063729=\EM\1101425\&9W=\1080374\ETB\ETXsk\1007873\13458qsw\ETB6}rh%\v"}
testObject_Message_user_4 :: Message
testObject_Message_user_4 = Message {messageText = "\SOH\NULThn6*\24172A0hKq\DC1\1003152\1083949=\1105804@\1000733BTG\1008247o6\160567M[a\1032217@#<\59840-\144893\ETB\STX\1084111\&0^\146727W\SO\ENQ\ESC\SO\45073y\83523\ETB\95906A~0{\996951Z\1108871O\11737{q_\190381"}
testObject_Message_user_5 :: Message
testObject_Message_user_5 = Message {messageText = "l\132236,Q\DLE\1068995\STX\ETB2'H*\SYNBk\NAKb6\t`f\5457\"$lWO\1084434#\1050234\NUL\EOT^r\99728w`GZ\RSb_\157754Ea\16782Ra6oI\1094499zz$\aZ\ETB\159842\1055261/??H x>\5531g'\DC4F})|Z?ZXOE\CAN\133795\1042691f;uL\EM\1055359\SUBzI\180932b\1086365j\998637w8\DC2Q\f\nLB\ETXO-m&\22304Xi/\1056843\1003366\29128\6819\136645\51206Oiz\97387E~M\SYN\20229\SOH|5\ACK\DC4\984800\43150\&2\DLE\119873\&3\1038319#\EOT\1008662ZUR\DC3\DC3PdO\986553]\EOT\NAK\1066860\DC2\1012788\vK\43437\ACK+}1\172011\&51\CAN,]J\1020212Q\63831s^Z_N]&RI$9\SUB)\140025&l"}
testObject_Message_user_6 :: Message
testObject_Message_user_6 = Message {messageText = "\1094442\179070cP\SUB\ETBys\ETXj\US\155473\GS\1030321x\GS\ENQ\39497\STX\CAN\29650>8o\94698\1104369|#*+\SO\1045681k\1011772:0\DELP\1016760JC\SYNv\FS:+\1083160\USt&\984909+\EMo\NULRCq\1113078@\ENQ\DC3_\v\CAN"}
testObject_Message_user_7 :: Message
testObject_Message_user_7 = Message {messageText = "B&\1092399\EM\100679\SUBx\142420\DELiB=\96753\SUB\20802F#\148808T\65584c\32214\RS_\100358+\v#\66779\NAK\b\ETB"}
testObject_Message_user_8 :: Message
testObject_Message_user_8 = Message {messageText = "\1031770\99178W-\1040666%hq\166787?\\jN/\SUB!Sb\SOH9\1083735t'{\1074887\NAKkj#nvS\NUL\1045457A2q%,\134801e\1022517\1025583n\")\aq\186124@3\EOT\b0L\v\DC2\131950\SIMD\1096582\CAN\161471\n@Pr7\DC3N\1089821jG\t\182561\EM0!\147297 \1053546\&9evb\1024756`V7a'svgn\ETX2\1090655IsG\13523\159358\ETXF\13008DV\133864@9CG\1014621\&0}gZ\1099441=%M^N;\SONYY\1039772\RS\169013E\1059723(8\1111074\&9\54384\nO\\PN\16980`\1012409\"\CANy!\1097991\ETXR\24681\1035599\131905\189414zl\191239\ENQGS\DC3(P^\996842\DC2\1006777\1108040\ETByH\EOT\182092\ACK(\1065721g\CAN\59805U.v\USc\US\1001009H\121061\ACK\DC4\"$j*Ti/\139226uK{k^T"}
testObject_Message_user_9 :: Message
testObject_Message_user_9 = Message {messageText = "J7\74327\999894w\DEL\DC28\DLE?rG>\182981\ETB\168994v\vk\1082422UYU,%\ESCc\174730\19280+\DC2\1008073F>\1052390l\148591\ACK\DLELO\1074041N{N\16767\1003896dm3)K\25793\1029820\FS\146561{:\"\1071648a\60677kU1b7~U\SOO1!\172486<\EMy\61920i\1110836-\DC1\1031386\100666\167822\39858_3\FS\DEL\1043482\ESC\DLE\SI:o\RSm\141881k\999057\EM\STXU\83055!\US\165448jC\DC2\23420\1009656\165866&\178585\997361zb#\1070079tb|i(?%Z'mevHA\172643\1078410\EM1<7\189825T\167527\RS\1030580|\1095130HhSj_$\139384\DC3\DC3OH\1078628\CAN\132815\25232#26\1088128\1066935<Vwv\ENQJh`\GS\1063457\v=}\159056b\\\NUL2C\1047926hu\SO0V\EOT"}
testObject_Message_user_10 :: Message
testObject_Message_user_10 = Message {messageText = "8Y\1046722\1083980\141250a35zu ${ek_Nn\1034443\95300N\99380 t<[\1004906\SUB\1004288\996050>&*r\44604\NAK\37179\t>\DC2u\1075155\\_Pe\FS\135092nM\70340\ACK\GS\53033a\1111077QW\165995/w\1111510.\DC2*\GS\SOH\157313>~=\984718\SO\ESC)\1044201\29854\NUL\SUB[2#}+n?,i/\GS \165424"}
testObject_Message_user_11 :: Message
testObject_Message_user_11 = Message {messageText = "\168892U\94821\b\29234\&1\46453?=\1080669\1041462\1066356O]7\rPj\ESCB\v\8926\1110797\RS]f=\19115),~\143683q\132410h1\18766Z4q\STX`HAJ\119188\94262\1093292Rs0\FST'\1015218<\SOkeDQU\ESC3N,G;Vc\1021876\36686\1105626\184950\40308\985949\r=\ENQ\r\SOHn\989266!\1071974t*4\EM\11972#\"WX\19955\&0\65597\1025129\1015733\GS|`eG\t\45092HLQV\185506GT\SUBAabm`YX%\133515`5\n\ETX\NAKd\\\"v\DC4a\1074627wD\SO.p\no\DC1\26548\1035252\1009606GiF\USSB&\983669\STX9\1001621Ls,\bz\ACKI\135150\37163C\SYNSSI\EOT8r\SYN\1036452D\1032426Dm\1093716mb'-\182631Jj\15678y/W\1037979"}
testObject_Message_user_12 :: Message
testObject_Message_user_12 = Message {messageText = "\1023786F}zz:EPekT\190209\EM}&\tz\59056#lB\bTa\DC1\DC4\47542\DEL\50795Jb1D4\NULP\SO8\1110527\176752\SOH\US&A-Ez9g\DC2\f=\47736\EM\FSS^m3-GC\SIc\ESC\19085B\1080263\100850L\ETB\1073607De%zk\137503pG\SOn\SYN\ACK-5t\ACK\SUB\\\STX\1034353\DC1\1016950\&1\994234K\DC1R\STX*pY\146424>\191056\NUL\v\96919\1060913ivDk4\1102702|,\fX\1083890JrU~;9d\62852SM4h`\v )\133070F)\b|Me=\152560\EOT\ry8o\1050647\1012239\1042151PP0=\tg$\44548\f\168061*mL\DLEoa\ESC\6100\12202,A\RS\994480\1054776\38237\ENQ\DC2e@\1003245v]`$\r\35956BNS\DC4*_\1073054?\nq\r\DLEw\60709\DC4\DLE\a~S\1073465\&1G\1088375dx\1076108@\100882z:Yo\SYN5\94515Z'\ETX=\1056034|}l\1001578\&55\1097507<f"}
testObject_Message_user_13 :: Message
testObject_Message_user_13 = Message {messageText = "\DEL5\DEL\r\54710\USp`8j-\FS\DC3\vr'o@62\1081198\159400\174669\ETBfo2\1056557\\$\ETX\DC18.\a\183786d__o\DC1ME6QC\1105717\&5e&xl\1081329v\ENQ\8560\998387\&3wT\DC2\1074461~sA\vZ\SYN;\1048727\178758\SOH\997231\121064\134186\31308\STX\fn\SUB\ACK\141514~fts\v\985602\178872px\ETX0\SOzX#\187660\&4{-OzW\"+\1080912xdkQ\999275n\CAN>k]\STXm\48460E\ESC|>\ETX\ra\r\US\SO\996167\STX \r.i:\EOTg1\31760\27679\DEL/\181957\EOT\a\DEL\187472\5770?V\EOTJfp\1013813\1010058\178962\1071492\ETBMq\\w_C\NUL\6450F}\159318\ETBF\1012496\&5c\1091270\1090369}\SUB%\1042944\DLE\1016900f\DC3"}
testObject_Message_user_14 :: Message
testObject_Message_user_14 = Message {messageText = "{J\65314jJ8;\29880\45926\161068A.\1068416[\RS\149348P\146363\&4\1105011\NAK\EM>%\32851A]\154274\STX\".q\83188>9'(\134885bVcv--u\35004}~\CANHv8KOnc\18447_\n\59611\1008071c,\r\128247Q\54757\41879E\178883Z\185481\fAz\1102432\4578\DC1O\1022596z\CAN@!-\SUB}})\1023270\FSy\NAK\1015003)=!-\22805\&0=mY\40645\DC1H$T*\1034328z\191378\1013661"}
testObject_Message_user_15 :: Message
testObject_Message_user_15 = Message {messageText = "\SO\28930{\1636Psu\156612XF[o|[#G\v\ta\1103835\r\ENQ\1089782j\DC2-\989930V\1012080.c\140466.S\SOHfwV\1077792\&9\v%e!\DC4K[8fd%u"}
testObject_Message_user_16 :: Message
testObject_Message_user_16 = Message {messageText = "8\SYN\1071713\16473\14352\1096751\bAj\SUB)&\1024894Nw\STXNb\1004671\153551t\1089617\ENQ\39358%#\1063747a&\132813^\SUBpM\46290CdvB8m\1066188\CANE\vf\RS1\DC3\DC2U\40204Ztt\1101501E\DC4A4T&\SIl\1097343v\1001343\SI\NAK\DC1\155047\34743\DLE\1047519\176610?\1074701\32852n\DC3\USJ\1038388ZiqU\1064894\b\1010646q\rRw^D\ETB\DC2qbX\45835U_\1048324\\ZX\1098718Y{2|t\"m(*\1091865\1012259_\DC1<\tDaI\187357\nZ'FZ@\1062588\1001017Pk\58478KO\GS=\b\139318fd\1060652SW\\WCX'\181565V^+DQ\100932\125135\r3\ESC\SO\v\177575\&9Qw\49267\1110903\RS,\100830 \1111547,C\999891y\ETBw0Q\1083947\EOT\SOH\STX<\nwY\SO\74799}\SYNax\DC3[-\177296~\"O\46316\53775\SYN\146349\&2j!\1081310\62388`\1068931\145029rPX\ENQb2\NAKr<eY1\EOTpy\SO4~"}
testObject_Message_user_17 :: Message
testObject_Message_user_17 = Message {messageText = "f\27151\CAN\ESC\t\NUL\1050154z\1071719x\994275\DC4$?){b-\SOH\v\20506sv\98049#V\n|x!\15066aM\16256\SYN\ESCXi\f_U4\1010895(D\SUBa\1030390OGG\FSK\179895&^\DC4goB[\1069372\1002247*\20009c?}\DLE\DC1\139765\184736\164105\SOH ~\1085061\2026\127280Z'\1051477d\1039491s(\ACK5\1028788\DLE\1027525\1025396\CANiVPs/\\P&t[a|lZ6\ENQ\34498gQV\1021140p>&z9\NUL\34922t\144311~ND\1100954\&0K\v/IQj6]\EOTR(f\188065\1093678\EM\1110953\9014\US>\1025349S\DC2\FS\1039405\SUBP.\1088291\DC2^7\f-\DC4\57891w;e\10731\SUB\NAK\53632\SI\1049806"}
testObject_Message_user_18 :: Message
testObject_Message_user_18 = Message {messageText = "x\EM\46370"}
testObject_Message_user_19 :: Message
testObject_Message_user_19 = Message {messageText = "K5jC\98539\54145~\144655\SI*LJ \33056z}\1104610HB\SYN^\1098654q\1072047\FS\fP4l\1061357\42281\988352\992938\1007207>>rqB\SO\1033165\ETX^\991001w>\1105923].b\1110995\1001847\100110m\STX;\\P[\1040383\1100452P#\r03\SOH|u\1037450Ir\1028407bwFv\SUB-\62808\16076:\EOT\SIWo>Dk&\3485\99401\EOT U%\b\26213>\992344y9D\1069210\DC2Uw\150616v|s\1009174\ETB\984639\1098499U\171962U-8\1063556x\1019474\178686\176175)K\f\ESCK\DC3\n\1003854\DEL\1026427\124955~PJ\""}
testObject_Message_user_20 :: Message
testObject_Message_user_20 = Message {messageText = "\SIFo\1015219\SI\1075968\DC1\1021774\DELBJ\DC4\ENQ\1094045\NAK\SO/bb"}
