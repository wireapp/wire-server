{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PasswordChange_provider where

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
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
import Wire.API.Provider.Service.Tag
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_PasswordChange_provider_1 :: PasswordChange
testObject_PasswordChange_provider_1 = PasswordChange {cpOldPassword = (PlainTextPassword "V\73007\29314@?\SYN\998259Fd\1055215\DC3.\SO\b\ESC\1018210\SYNK\DC1\EOTj@1jo5;Dd\24949\1088307OXC#$\143009\23153\ENQ\155598\38407k7\1012746\DLEh%-ur\987041Fhx\1032610\1077757\&7\151877a\178074I\1057581_U\SIYzk\b.Q\1013579kPB\156054\DLEIz\131901,\1106014\SI\DC2\aBo\"K\n'\US#7a\SUB\STXb\135064\DC3\185367\RSZ\n(6\19006\&3#K\DEL\1089180c\SUBTIkM:\1065272\&3MW.6eF\fO\1011249]JLi|\NULi\a\ETBI\NUL\\\96342\151632\b\ETX\SO\22392\1045757\SYN\EMV\NAK\1038957>\165116KpY>j>V\98894\1015461\1006006W\CANXu_C\133966W\DC3\EOT!|\1061728DW\GSLmGUb\1043729\CAN/r\SOH?,\146001\1061299\&2\USU\169684\142720h\afMb{\t\a3kGET'\b\SUBdJPg)\163208\1006299St4\SUB0k\SYN\DC4\36527aR\138772F\1042658\tF>\1090139\ETBTV{\71099gd-\133049'R*\1106140.\171798\SO%|HTk]\RS\141269\1958\1083591\DC2\SI@\133084s7}%pX3$\USe\NUL`yy[-k\98696\SI\70433\1100329\1073756\1040048![5w\1063846\ETB@\DC3S\1010405!\40915~Ea\1074589\STX<\DC1\1103868\ne|\1104406\DC3A\144561x\ETB'l(Q\141092=5\STXI\b6\158436}\"e4\nQZ>\1072750\RS\v=\1092408\143278q\1070390P*-R<\143601\178306\US\1112485xI+\151405\40202\SI\78315^*\1018758w\151413\40550D\43140\1101178~|y\1029719\139669\SUBu\1081167\23637Lm/\138145\1083976\61288\&7!\DC17it\1110442^\STX3\29157\1018262>\DLEzdBM \1095261\990101\1099090*Cpa\175796\132034\1065516\SYNF\t\36088\n%u\162693[\f\RS\USd`!tL\DC2\1087904dxi\1082181\9361\1025105BvC[\38439%H\RS5\1064729\"\DC2$^\150813rRxT\98759aG\EM-\1078482\1094765\98704m\",]I\rac/\993124f`\CAN7\186955^\\=A9\DC3\b\173018o\12090\r/%\167368\US>\59104k\1052572\95358|e\RS\SUBS:\1013861\98898\1107109\1005884\5253\29629LM\SIa\1075489'R\r\DLE\128078\143050u$/\SOaJQ'F\11524Rw\177412r\44764b\1032701\180309\CAN\185296\&7\131673\173418EY\1026635\SUB`\ESC\SI4P\SO\1013119B\20773$"), cpNewPassword = (PlainTextPassword "(lkq*D\14880'TmU\1088138k\45890\n(5V\73677DW\144469+L7P\1016138s\SOH\1039685J%g;Qs=6e\148417_\1080547\37931vOx\CAN\n\1098989\83356D\135502\1042592\DELd<\986146\f\STXRupz\1017630\65892J \DC1y\FSx\1051892\a?X\"\f\145531*0\ETXIyTW\SUB\"a\ACKb\DC3@\188839(:+p+\DEL\SYN\SI\tc_+\ENQ{\DC3\1112961f\vg\bM\20210\SOHf<\20029<\SYN-iMZ(5~\1088119\1065489$@/b\v\1012174v+bk\149654)\\R\US6")}
testObject_PasswordChange_provider_2 :: PasswordChange
testObject_PasswordChange_provider_2 = PasswordChange {cpOldPassword = (PlainTextPassword "\120992g\1105204\&0\1018599qxS\ETX\SUBd\ETX\ETB([\43586\r/k'/\1056774_\DC17!X\1088800\69237EU\990631U3\1065459,X\179343b9\SYN',\"\1001526hcV\DC1\ENQ\188098A\1029382>b\1014167\65569f\1054034\51432\1095095\&7r\138119\986397->S!\996285\68058/\128475t\ETB\n?E9\"\65570\ETB\ETBN?)O\FS\62481H\992740\70082D0\GS\66728#\164553d\SUBO~A,g\74491ws?s\f\57366^\GS\NAK\CAN\98729\1051359\&9bZU\64090F\179270\132223\b\STX\1112215\38239\1011719\143718-\142538@\30335.]\1028797\1093197\"\NAK(\a(U\1071669\1046547*\150835<u\SOH}4\DC1\1022792a\49629v9s%@\1021384\991298\&8=#L\ETX~\49358OT8u*@z\n\1078941\129352H.@\DC2\72790J\DEL\120034+f|\ENQ\GS1\100585\4400\CAN\1049158\144299?r\1047060\STX[\176203p\186176\1054528J\182524}E\33603amh\NUL\RS0s\DC1J8\987070\178901\144096\NAK\CAN\1067164|\165246\ETXH\181471K~y"), cpNewPassword = (PlainTextPassword "\1039882lT=\SOHSrE\1103480$5\35625HJ`\1100956\DC1h2;|\1063366-q\1100064\18697\53420M\FSX\14385G\1053757\&9a\183108D\1017061\n\ETX\174716-q\DC1|n\1058977vB8<\144462\53943\STX)V\n\ESC\159129T;\31129a\DC3$mQ\SIXL\151626@Q\USlA\1039984\ACKT)o->\t\\q8eF\DLE\b\20125\GSJ3C\GS|u\NULKu\EM]\1060689QF\19114}\STXC\v\68054\146321[\STX4\DELK\1074452\ACK#6RD]\1040473dX_\NUL80\GS\1090008l\USkL\NAKgDZEAcpll\997825\SUB\FS\ETB\FS\n=\1099119LXfl\1038800@\65550/\n^\\\1087890KD\ETXf>5B*\n&\SO}L?_?\DEL}h\1030725\&49/9\vF\SI'Z\160819(\54446\1013879^1J V\9587\SYNsN-B\1005956TY|=\NAK\r\SO5\172082ZOU \138969\&0\1063669\50638T\132502k;H\1085705\ETB\42632\SOH\1085276\SI\1011002.dfkQ.\995822w\997098W \1075926\138376_\1028277}\55160[s\EM;\"\1102919\&9]'\t,X10\174505\DC3\158634\1079943\&4Cx\GS>\1001021\1042988$K\NAKqCk\a\DLE\171745#\NUL\171853\f\"\185768m?=\1038690i\DC1v6\DLE\a!\STXw\DC2-\1017128\DC2\ESC4Tz?\1092864Jc&LG_\7734RtP\1027358K\78749\147164\182887S]V\SYN\STXDJ]~\DC3|\180155Lp'\SOQ\1048295\FS6\1015173+2q\157032T\1043030\&23\143103[\DLE45\1065391 \1093400\DC3\US\ETB\ESC\NUL{5I\30966=hb8\SOH\GSR;\1033218~\24659")}
testObject_PasswordChange_provider_3 :: PasswordChange
testObject_PasswordChange_provider_3 = PasswordChange {cpOldPassword = (PlainTextPassword "[\DC1c!Y\NAK\fyAi\1041938\a9l\a\DC4\29283<ZTXm^\DC2\1100034\&7y\51455}\148015\&8*\SOH\33957\992426?\1023038=6\NAKk\n\169530\\\30243\1095834$7.t+&3\142261\9893F\SYN\1025149\\\EM\181851\SOJ\139817\1064679\&4\FSK\41792yX)N}\SYN[\NAK\184862\EOTJ<\USI\1092493+\50682g;e}Z\65594Zy;\v<tB\aVtys\ETB\DC2@\v\SO|\DLE\SUB\SOHgW\DLE\27591\1066288\53383\ETX}\98385n\38038\&4\EM_sP2\11456\1049862O\1051912\DLEw\NUL\NUL\DC1\169999A\EM&0\SOH\58351~\1020147\EOT\1082766\47870\991721\US\1012209\133503\32445\42120\136520\DC3\51993\CAN\SO\28940[*&\FS\ESCKus\b\fZ.\1014548\1077735P\1097012Np-\1072875C\1047428\SOM\vh{\159564*|\EMS@Y@\1020927ATj\1093687\1079673Sf\26780\US\1094332\&7E\1090001\13156\33616\v$;w<;{VIy~\fQ,m\STXd\1004145\&9fe\78327,\1058523\1113379\169897\143639\1029289\a\tpqY\57583q?I-w\n\NAK\1088213CCX\188063^\1113792V\1092899i\SI\ENQ^4\15947A~\n\41940(R\f\167271Bd)_\44846\&90\8839\137622xedH\53767\163733\44645[u%?)\v-\"v\ESCZFQ:\168654\165078\15321u,P|F\1047101b\152672\a"), cpNewPassword = (PlainTextPassword "\r;2V\STX?w\nm2{41x\"TJVH\134307E\173573\SYN,&\DC4P:hKKQ\v\f\ETX\1014772\ACK\24561\USl\1074392\1011403\NAK\174454\f\29731\DLE\98665\&5\24959\&0=`08~\GS\1020575SV1B\EOT:1Up?B\59947}<\3669\ETB6,n-K\SOH\1067078\77892T\vXYD\30076S\1096284#\SYN\1038677B[\92335\GS\27662+\"}\9411dQq\\\\{zi1bn{\70450|zA\v'\180842.sJmI\39983F\\\186598b\1010427\1063762Ikla/R\EOT\1107270\vk\174248\1072272q\vpv\984408Q\f\ESC\FSV\USVS^9H\1090689\ENQ((`\23172m}hGgAzN\1014868\1006454 !\159835")}
testObject_PasswordChange_provider_4 :: PasswordChange
testObject_PasswordChange_provider_4 = PasswordChange {cpOldPassword = (PlainTextPassword "\DLEP\990335kq\DC1\190429\&3Qs\1058137L\73978\169085\n\a,\nc9@6\SI\1008744\1049664\nc\DC3\184974^c9H\1095911^A{\STXXf|yP\1056257z\1039010*\r\1063026RT\993345\&7_\987991\aQu`iw\171742\ETXj\EOT\RS\1046543b\1085398-\bxBI\49225\1015356L\ETX\190599\1100564\NUL\DC1o\1036622\vI\GS)iz\154924xB\RSd\174137'\STX\GSdm\fgSx\992720\1075172\&8\1027670\ENQ\994466\25301\1028338\EM\173597\&7q\1086960%Z\SO\\\1109914\1005040+-g\61120mI\1070451om\DC2h\SYNaP\19681RZ\1050208{8Oz\a\998775\&4d\134356\1057996Vre\DC2N\EOT\1024237o\tZ\152883d\1089287\b'~\1071708=b\35941mH+\FSM~\986957F\139239\62054\178168mN\1073211\187011\b\989944\&5?\1090446fU\1091788fM\SIV\v\SOHj\ETB9\1061658!Y\143063GTU\1071751Q,\DEL\10292\NAKJb\65208`g\17341\av+\185098 \GS\65247\1062067hZ\DC2\110746\a[\176463\1814\&7Z\144060>8B\1066872qu{<5}\1056347IP9{R\1102935\1071418\&3ot\r\1079180\t4\ESCx~\986204\DLE\1070230\ve3a\NAK2Zf\b\172316H\1012247T\161558\46561\ETB\28668\169808/=<m\RS\RS\SYN\SOH3/\1109121\1021713\CANqR)\DELL\11080\FS\57482\83100d\1047106.ohH##FA\1046458\NUL\f\71341p&<30\171671\183086\1023525\1104707al\SYNxy\f\RS\SI\1021523<\SUB\EOT{(\145403D*\146212c'\31091(\ESC\SO\SOHr\1081482\DEL8\ETB"), cpNewPassword = (PlainTextPassword "~i\DC2\39816\SI\DC2\1113596\DC4q\ap\180947\v\9617@R\SYNSv\64786\FSA&\EM\59456f\143713\CANw\SOH\SOH\ESC@\1034982\1019862>\NUL\54577nV\SOHX_\SI'\987291-\GS\ETB\GSo[\EOT\145110A\1080101\1098864WG)\1005975\&3\EM!MSLiVd`\v\1089001d\139951P\f\77917%\NAK\143313\DLEaN\1098080As`ChB1\1099405!\131466.\1100860.\ESC\bD%\1073705\r))6M\1031752\67308_\996338e,\178093d>c\1101567;kN\1110780\999986 /o\DEL\175374\1092155\987111\&2Vj1$\"\14668XdV\43435,WjRQQ?HQ\1097680\SOH\GSu Y\FS\CAN\164434\992522\1111869\1041862\SI\n\1039484^9\SOH'EEZ|,foB\154655.Rf\EOT\rX|Oz\ESC\1077440L|5V\ESCR\25593Z\50827\1034223\"\1022229\1018320\DC1+\CAN\1093174P\t;2\28766\1040246\r*\STXa1\160047o2Z\"B\nz\1010862J\148935*\fOh\SOHJ^\988980xD\1109414\SI\b\1008554;\DC2\165614\DC2l\t\1017136\78078!lS(Y\r*\63569-\22669\&8\DC3\ENQ\41748N\7485\1073891NA\\\1089440\ETX\ESCrWMao2'\1056375\ETX\DELQ\68431\ETBO@\1106790e\177949iZx\1003463\1030070\468\3137\&9\13491\151385\1029546eNgA\NUL\r]\1089667O\"\DLEK\GS\f<!$@\16580(\NAKtG\SYN\GSQF\142627\148790\GSF\1042386p\DC3\US{\DC1(~T\DC4\177523\149229oI_\SO\CAN`\1019617\1045555\ESCrpQ\EM\136929\SIF\EM\140033\GSed\v$\1079947YZ4\25442\132404^_\DLENQ\1054837\b\GSh\NUL\ETX\1098847\ESC1%\GSnmRc\ETX]td\187964:.PU=Q\1067761|Ik\133186\DLED\ag(\991084\EM\\D\1068790\n\1104730Y;<W{\1040480\1049534G'}&\160851\42909\&5z\v\1078101O\125185\a\50934nI9CB!\10191\42159\SUB\1030133")}
testObject_PasswordChange_provider_5 :: PasswordChange
testObject_PasswordChange_provider_5 = PasswordChange {cpOldPassword = (PlainTextPassword "_\ENQRL\13157\135297\DEL)-&G\NULFC\1013705\120612\DC2hx\170566r/\1040326,H;!1\149613TaO+y\1034107\26334\70288\ENQZy\162430<\GSkh4}A s#Ur'eTP9\STXN\92275Bd%\f}\1103757v\157766\DC3f +\DC1C\DLE5\41207\v\USj`F\SOHf\EOTY/\SI\FSau\179670\f#\nFx\DC2+\b\83333\23330\EOTI\DC1N\1010447\SI\ETXf\DC1NCb}y\1050616\1011924\EM\RSm\144064`iQ\b*?\aX\NULx9\155624\nM!7\GS\DLEl6/\8867\1069530eA\n<\1051997b \SUBQ\EOTl~\999487\&4\1111651\DEL't);\EM%\62788zD\GSv\129058\169607\1035603\ENQ\"i.m\12454\160385\SOH\DC4\111235D\1054439\126625\RSYso#\1015639M\132135\1076291l\1090187\ENQn=1Z\DC4v(6\1105033\38696i\1078470\CAN\CANi\11163oEm\6779\a\DC4\1826\USlCU k=A\ENQwS~ol^e\a\DC1F\1094594?f\SUB\NUL;\1010942\153929\r\SUB_5\FSA] \95874\GSaO| \CAN/8\95032\1041523E}"), cpNewPassword = (PlainTextPassword "\RSR\EOT 1i\DC4kP\1054369{\EM\1036649TdRGEJms\SIR_\SYNtTu\993143t\\\1018388\US'\57730]\1061798Y0\SYN\np}\SUB\990277\141523\58318S;\SUBs\191070\172668\&9;\986283\1071919\&432\1075471\1057158@\120586\993249'|*\1075614?C'\DC2\ENQ\1107794Q\188726\&1\t1\18533\DC3-\8638\&6t\62058K\NULv/\DC3\153104\ACKx\ENQ\72294\\%4`\ETBi?E\1062727\1024889\47785\1099630\US7\67585\&3^M3M2U\v\1016233\CAN\fo4$YJ\1102099\RS6S@V\1098802\EM\DC3\1651\ACK0|>\94311#\127522\151811G\1000615\&0\149709\1097521\53344@\53483/xjz\24440\151154\&9%1H=za\GS\1106047a\n\1090133\&0\1094112\STX7C\1106191\GS\"/VC\8253]\ESCu@\142126\177420\1040515\DC2\119137\1096109\nd\1003289z\FSDzU't'\1002679\&8H\602)NK\158543V\NUL\ENQ9>\133619}\EOT\NAK\1076333\94036\&2\DC17\1055056\1070660`\1073554\STX\46928\1014073\1037957|9\DC4\1048292aMii\989484k\24963\DC4cR\ESC\1003545\\{\DC2\b\SI(3\49924\tn+Lz\"Gc\1049035g?\9607\1088074\13592\1026608^\1009465\a\164980\ETB/\\XY: \DC3mQ_\GS\v\EOT/C\159967BDxF\1058916\DC4dg\ax:#\DC4+\EOT\35825a.rQE4\1084799GD\ETXX\1017144X\42017\985863IQ]/S+3\1030208\RScAs-\985466\1029737\158365x\DC1Y\158740^pI?9\166083\11192Rg^\1086735o\ETX\SUB\1052223S\\a_g\SOL@\60837s-\SYNU\1072769\171119\NUL\1005596\162825Dnq\DC4\DC2\v\1086456\31257\DC26j\b5+P\177093k\"d_s\EM/\1013778Y(q\143175\&1StF\989408D\29679/\1626\STX.c&-p`Af3Gj\FS\1112849\&4#\SYN\165976\ESC\180413\DC4`\NUL@\139987fJ,qFv\ETB\ESC|\r\1093854\DC4X\1042600cpTXpf\vf.$\RS)\168891\&8i\SUBQ\1044865g\1043171vUE!FP\1085287\&3'2\1080027g\1111908\39118\145755m\190237P|}\ACKEeGc\fd\EOT4\tDC9\1060012\1034484\100055&(\ESC&\132940~\SYN\DC3\43584\1009608\172646C\SUBr\SOHL\EOT\36457u>6v\1089892")}
