{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.CheckHandles_user where

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
testObject_CheckHandles_user_1 :: CheckHandles
testObject_CheckHandles_user_1 = CheckHandles {checkHandlesList = (unsafeRange (["$:\177996","S","\1048930\1054592\ETX\997811\31018[\135466A\"\4855P","Ot$E\1009099o","\SYN\EM\1008484\DC3\1013210","R\ESCop\1123'#n","\1072400\SUBF\\j^\SYN\34063\&3@\95129E\997135","&\ACK\EOT\1031033\ESC%cC\38595","\NAKr\t[\aR5c","vm\177998\132520\SYN\46070\EMkN",".\1043998q\1070577\1021192\&3n#V","\DC47*\NUL\nu\EOT\SYN","I","9\151493~@\65681\US\GS\GS\b)ucd\DLEe","\STXS\167259[\1105113\CAN\1000966(","U\1029481\148009\SOH^\NAK\13099G$\19844","\60105-\DLE\DC2l\n\STX\24074H-\ENQM\1067368\f\"","\1088767\SOH\1087302@F:\169908\43250/\b\SUB[","\"Rhn","V0U+$K#A8","'=DC\1109021","2C\25493[","","\vV;s\NAKy]","dS%*\73681","J\2153#\991571CT\49074L",""])), checkHandlesNum = (unsafeRange (3))}
testObject_CheckHandles_user_2 :: CheckHandles
testObject_CheckHandles_user_2 = CheckHandles {checkHandlesList = (unsafeRange (["\51340\1098872\1110469{\129152\121427$[Q\1055312\149223\SOH\\w","\49427","@'\SYN\182692%P\f\r","9","\ETX\1099278c\98579jl","\64759#C\FSh\151491\&73\993806\NUL\1067087ujZ","(\1091841\ACK`\a\US\DLE\121416\DC4\1084766","w\nY\29978\USL\1064213","af^yde\40390\SUBym","\94854r\FS","j\171318","\173384\DEL&t]\RS\1035881\313$\ENQZ:|\DLE\17285","\NUL2\1058597!C\SUB\172414\1114053\991428\133715U","\ni\1011149\147918NA","\70052!d\61564\SOH\ETB","","\66009]0/\184783)\RSuG\136298T\135416R\170085","zg\ESC\RS8","f>\1066771\CAN: \STXa#0\DC4Wa","\"l\RS_\1107686\EM\1012891\DC1\DC2\13750","\1082869du(","\97650N\ENQAd>\EOT\DC2d7","P\t\DC2Q\f\DC2D?V\158861nEq5","\146299","1=\NUL\1004696Rqo\131277\ETB","\GS","\"\ETB\1113783\&6F\ACK\1066823l>\94502\178011|wp","\NAKl\983996!\13860\152420\39430\170935-\133742\SI","\1090344\1089808Z}\"$!5\ENQ@\ESC~","\27270F}\181155\SI","#6\95555\RS"," V\1005918","\1048915\123140\1076067\138953\&7Iv5\142611+kHC~\DEL","\983203\\gZG"," :kP\167267\998828\&9^","NqH\n$","Q44.O\1007808jW f\ETX\\","V","\CANe.\SOH$7}\DELBV\20647/|8","Z\RS","'*","\"\DC2","\146601k\77913$mbv\b\137126o","J8\20421h\20196\1048650\170538Y~;'Z6<","","\SI\SYN\161329+,5","4\169544nslL"])), checkHandlesNum = (unsafeRange (6))}
testObject_CheckHandles_user_3 :: CheckHandles
testObject_CheckHandles_user_3 = CheckHandles {checkHandlesList = (unsafeRange (["\SI\r","4m]\a","\190189\135237YG1O)o\172652","3m","\DC2^t\CAN\1081909*\FS","\1010587\ETBbt\DC1\1023646","\SI;K})","\DLEk~\154637DJ\SOH\1025911UK+\US\SO","\1075261$p\CAN!\1012328","\ETX\US~\100269x\1073087V","C&0H\1082972XA2\f\46832\13210","M","Y\USN|m\1038601\1053368mUJV\985077#u\111355"])), checkHandlesNum = (unsafeRange (10))}
testObject_CheckHandles_user_4 :: CheckHandles
testObject_CheckHandles_user_4 = CheckHandles {checkHandlesList = (unsafeRange (["H\64936\ETBu\DEL=\DLE","a\1028694\95020\1053684\1080646\33228TH","\139523I\NUL8c","f=4p!C\93807]\175941:\FS",""])), checkHandlesNum = (unsafeRange (7))}
testObject_CheckHandles_user_5 :: CheckHandles
testObject_CheckHandles_user_5 = CheckHandles {checkHandlesList = (unsafeRange (["\SO\1096699\DC2","\74963S'\163854\1027926`zg\141920 ","","\1048786\146911<*9s"])), checkHandlesNum = (unsafeRange (3))}
testObject_CheckHandles_user_6 :: CheckHandles
testObject_CheckHandles_user_6 = CheckHandles {checkHandlesList = (unsafeRange (["","\51116\1041047","-\SYN4Nw\184552\STX\185186\1014765\&7\f","Z:e","Py\be6\\","\1045766\&2\DC4\\\v:\FSSd?\189291\&3\GSKY","\DLE\1069254\164611","\DC4F*&T\95682E\20042j\31121","03V3)w\NAKh\1084931","\1042430fL3M\189728","0EM\999280\135504l\998555ef+","\US0T0\v"])), checkHandlesNum = (unsafeRange (1))}
testObject_CheckHandles_user_7 :: CheckHandles
testObject_CheckHandles_user_7 = CheckHandles {checkHandlesList = (unsafeRange (["\63514\21471","q\136601\EOT\DC4u\1047731d","\135004%C(dT\DC26\165151\&2\NUL\40390\1072287\7538E","\46168\144185J\31265A\24079V\ETB","\97367\NAK\EM\ETBNk\5604|\175961N\78083sE","\RS","s","\v\1035589()c\NAK\1061257\GS\1041519#S):A","\21516~P\SYN\1110087w","\185215\"\16066(\SYN","\DC2\ESC#\1047785\SO","\"\1030731a!\1013489\t{q","\nq\9287 \1040026F\78629Ei\160217%v,\ENQi","W<-\US\190097\&6\71123F","","\NAK\DC4\31435s\110757#]8\DC2bH\1112542\1013922C","\63337\ETX\992483\RS","o\41610\1016496(8Sa","R\1040513*e\1011356","","K_gX\1073811","\"\53989S\4382N\SYN","\21767{\DC4:\SYNw\143719\991338\b(t>","i","\NUL-","v\1002661(v&<\v\DC2","\14262\\Q\1103440W\DC2^\ESC\US\1037024\GS\31742p\142694\&0","H\SI\996377\1033373HT7\11190\1025838"])), checkHandlesNum = (unsafeRange (10))}
testObject_CheckHandles_user_8 :: CheckHandles
testObject_CheckHandles_user_8 = CheckHandles {checkHandlesList = (unsafeRange (["\t\13177ChQUdp","A\SOHVgA*\ETX\151401I","","\f8\62890\&1\985632{","","o\50077Ip)\1080682ie\t","v1\96085\149495]\139313\152093I\1002572\170335","\991685%!","7jS\20670\DLE^\\\n%\US\DLE~\1025849(\1072496","\nJ\61975}\1034252\SO\8813<\ACK:7","AK\1023131Ye:\ETX{XW\43409F","$","","{\f","\1039261\ACK(","","l","-","\1080248\1013815\178823/","\142067u\159230-)\DC4o","\73456","y}\SI'pO\133085\&2T6s\1607@X","\"H\60296\1004580\52382/i;wn\45239I\NAKG","8\187359\DC4\1092532\1003697\ESC\133336J\1006815\1096763~","\62398,HnE\27819>*\145229\1050708^\160026\153719z","~\CANZ!\1043721\1015984\96729j,Ve\"\37085\59893","0(\1088202\NUL","\1024668a\GSt8\SOH","\NAK","?-_.|\1070240\1094441\\.","\1033220\179475\170168\EM}","1IZd\144760\1004178","\21074O","5\28702h\986160mXoQ9\CANN\SI'\997351 ","j3\1003290Tl\1049751I\1091409\1080075\FS&\994775D[","","Cf\1046294\&2,}\n\1102288\11307","\1053699\DC4y\r\160751\STX\ETX\STX","S%HO\13536y\a#\159803l{G\DC3h","M\997157y!\\\40103n#B\1074877\151277|17","@@EV_","\73792\&8I\160615N\163567P\1050596\132931\21126\b\DC3","BS(\1000681\187823 \STX\DLE ,Yu\STX","\NUL\1025640\32093%sc1sBNbz]"])), checkHandlesNum = (unsafeRange (9))}
testObject_CheckHandles_user_9 :: CheckHandles
testObject_CheckHandles_user_9 = CheckHandles {checkHandlesList = (unsafeRange (["\172992Lz\20410\143941\1010758\v\1113340\SOH","e\\\SO\NUL("])), checkHandlesNum = (unsafeRange (7))}
testObject_CheckHandles_user_10 :: CheckHandles
testObject_CheckHandles_user_10 = CheckHandles {checkHandlesList = (unsafeRange (["@B\10086\US\1027951\EOT<di","\153952\1075838\1013520\GS.03-F\1068220\RS","!]\190660\120035KA\EM\1025121U\145106\34307\DC2\ACK\t","{w\987388:b\FSB@\39771\163289\1084037\US&\RS\173359","\ETX{F|\7762\&8<\165371\173844/\DC45\\Q\DC1","L\7125\&0BwJ\US"," s\1094461\SYN\EM"," \139261U\25106h","\1110101","\3006\140223a","i$\"\141836","b\1089122\1110005","\135993)h\146621\1007224-\3604\SYNn\31645T\r\1048101","\1063855*","\1081703PA\23809\&7\7627O",")=\1010048ts{_X\20404","\DC2#f\29107/","\994339;\97256\151151","n\CAN\14639* !<\f","\36750\1054051C\SYN\101066\&5","\989652\1015335<\992444\NAK6\993238\169997Nu","\STXSQ\ETX4\148196\190134\SO\1007135","\1075632q:rd\15316\SUB\1040427n"])), checkHandlesNum = (unsafeRange (2))}
testObject_CheckHandles_user_11 :: CheckHandles
testObject_CheckHandles_user_11 = CheckHandles {checkHandlesList = (unsafeRange (["t",")R\f","\1087837wV\29981{\CAN-(\9633+\EM","\1088251L","","[T\1048225\1002849R\SUB68MIi","K) )\168621<^QQ\1003642Y\995956\30733\EOT{","h1\\$\1053770\n\DC3.A","='\48887\1002282","\1001470\&2","\USM0\4014\a^\rR\v'\187377","][p{\1054911\EM\173058\ETX","\7203\&4oz#*","-\31766H","\RS\DEL\SI","R\SO\1021969\CANz]\STXV^\SI\GSx\NAK","(|DK\SO&[\b","","Y\RS\NUL5\to}r\SYN\1104096n","g\1067566\1067484>&a6\ESC7rC\1065450\3789","\58433i4JG+n\ETX\CAN&","\ETX \EOTu\CANYSe\158687[6B\2047\143108U","G$'[xD3\n\6492\181839\997012oq","QU%\147099m\61268","\45613\&2\177984=\t\SOH\RS63m","$X\1001243N_{\ETB\48977\&1 \STX\ETB\1302","V\1002635\156571\47550'6\182019O\1056807\163468D\DC1\145181","vCv\74963V\STX\1076079\985296W\41270\992107Uu\63108","","Em4t","+\161926Yq\990951'\a n\19588z]o\1069119p",")4\bU\1067463\ENQ6>)","\US+~","\183948","\DC2q\9206\70855*\1092426kgL","\DC3e\146506p\191047I!\1057526i\1077124*Cg","\DC3\989689U","v1\t\182747.<iZ,K@","3\155556.^\189848w;\1014900^"])), checkHandlesNum = (unsafeRange (4))}
testObject_CheckHandles_user_12 :: CheckHandles
testObject_CheckHandles_user_12 = CheckHandles {checkHandlesList = (unsafeRange (["go\b\1044337\DC2tA>\1035276-","}\19398 \154488gWH\30992D\1064842z\CAN<\GS2","B^\1076297@yrGi\167338\DC3\bkD\n\DLE","&)\1069221","\STX \1077048\&5B&M","\1037392\139761\169865'm<pMN\166855\1095607\ESCH\1060256h","a\189181\"\1098025\154098\EM?m"])), checkHandlesNum = (unsafeRange (2))}
testObject_CheckHandles_user_13 :: CheckHandles
testObject_CheckHandles_user_13 = CheckHandles {checkHandlesList = (unsafeRange (["T}\US","mY\14811\FSk\1021032}","\60471\&7Y\1016540gi|","","\ESCY\SOH5{I=","/\4940\173106TB\1073023\163868hH\EOT/\38451\b","\b\49392p=\1003539","H\1109925\1002326>\DC2cu\ESC"])), checkHandlesNum = (unsafeRange (9))}
testObject_CheckHandles_user_14 :: CheckHandles
testObject_CheckHandles_user_14 = CheckHandles {checkHandlesList = (unsafeRange (["\"Tcr","18=K;=\DC2*%j","im\1111680\SOv\984038\ENQf","","\173432'<\161224\vA\1058109\17954/+\ESC\NAK","\SOHR~\178511","\GS\ETX","RP\1071869\NAKP\987932\42815$","","","b]=\US)t[\SI\1090793\1093859D\153074zR\ETB","\DEL","\178341p,W\150364\176020","\1035454\1057162(","C\1025744azR\1044150\51651#\t\n#;c\ESC","$Aa\165227FR","pf\98398\37141V\EOT\1086937\1038696\DC1\1021771\175713\&1Q\1087084","9\ETX\32726\27592xo","\186514r","\159001BT9k\RSB\NULr","\CANEv\\\1011324\ENQ%`Nfve","\97751b\186733S\31782\&5\135639\STX","\38844\999836\1007118]\36299\SOJi\153614\194894\v\ETBN\DLE","7Ru;P","","]:\179995&7\DEL\33655;","\110832","\b\DC4\1067606)G\177381\163883","\DC3T\120583\f\171598\1025335","\917597","&,$#*@\ACK\SYN\18946^","","\1038642l\t","\996237\n\64870\ETB%\176238","+\EOT\151621q\41270\DEL","sWP\1074875Yt\ENQ\SI\ETB\ESC\1056625_\DEL5","b\4374\EOT>\CAN\64302sN:rt\ETBH\t]","=\49390\&7\EMY\100403\64779kK\ETB\1091118\151634V","\1105969le\170199\35248_\60377","w\1013764\EMz8\1095773)\ETXD\148144F_i\RS\DC4","\151614\4664","\DC2\SIgF\41342IX\ENQ\137519\1027638]}f%","]R\49100","N\30301uwv\b\1013620]","\SI\986245","D\995476v1Q","n","\RS\1050264}pYP\39508JW\1074759\SUB\1004385","","\n\NAKm\NUL*2\ETB'1\1021458S\DEL"])), checkHandlesNum = (unsafeRange (6))}
testObject_CheckHandles_user_15 :: CheckHandles
testObject_CheckHandles_user_15 = CheckHandles {checkHandlesList = (unsafeRange (["\1060779/\SI\1101793D\\\83240\b\SUB","/:\1092072gg","\1022859)-\EMH\1102045\DLE=\ETB\42458}\14964","m\DC2","w^\ESC\RSS\ACK\1103130M\1102321\1029393o\nHY","e\n{=\1103360L\GS\132260\&8\142154","\917868\139693\57434\SOw","\164614\1033076l_\3197","\1106131\DC1V_","\DC2\175825\&7tN~\EOT\51411c!4",">.*\SOH \61475\DELr \n\1033881[)\142539\1033113","v\NUL]","\41754cVi\1057293\2268\&8\US","\rO\1094821\173521c\156410\EM#\f\146518\f","`\NUL|E","H\SIW\ESCT\1009602","Gn\1078526"])), checkHandlesNum = (unsafeRange (6))}
testObject_CheckHandles_user_16 :: CheckHandles
testObject_CheckHandles_user_16 = CheckHandles {checkHandlesList = (unsafeRange (["","p\31867\ETB|\ri Q\CAN\1060630","\ETX","\53368y","","PSF\13598![}\139756\SOJ;","E\1091947X\FS3.Kv\1111670I}&Ex?","\74999c\148238l","\DC2`83","vvnV\1015271to","f","U\29673\33717\1089509","F\1060089ot[\5012>\STX\1071279Id","O\RS\1018806O.nv3q","","\STX\1053719vE|<8\NAKf\1054787","(MFl\1076544\DC2j\f#\1002055v","\171201r\SOH8\986896\1003268\&7\1063173gl\1074127h\1084788V","kl+","","'B\83252\52364","\1039689","#^","\31779Db~\1033723\132242{s\CANkh,r","\135722ZpB.>U4d)\ESC\1044808}\1070214","uYL|x\139414\83259\1027389\t3H:\1058472\1063637","\a\155436T0b\NAKH\",+*\DC4","u\RS\ETXejY\121167R\47707\1459J\ETX","","\67136\EMF*\SO\v","\128856\ETX","8<*RNoc\1046286","n1\EM:~\9522","\1078793\SUB\DC2{K\1049063Vp\ACKO.U\991243\155163=","r=\1011094\28902j\137761\&3\1068848","V*","\25851b\1015987Ln9o","xE\ACK\1025525\1110293\&3i\187784f;Se+^","?E\RS.KScH\SITF\SI\127257\DC2","\n\1059798yK{x"])), checkHandlesNum = (unsafeRange (8))}
testObject_CheckHandles_user_17 :: CheckHandles
testObject_CheckHandles_user_17 = CheckHandles {checkHandlesList = (unsafeRange (["(6Rhp\4119j'Xk_u\154967\r\133289","3dLu\78562\&1\\2","J]l&\ETX\49479\&6\1095885-\SO[&\1047430\1048896"])), checkHandlesNum = (unsafeRange (7))}
testObject_CheckHandles_user_18 :: CheckHandles
testObject_CheckHandles_user_18 = CheckHandles {checkHandlesList = (unsafeRange (["","\986406]\1089680E\22157","\186528\1108061\vy`TM1~;","\a4W\1082806}\172038\1001230\57713\153081g\EOT\ETXw\RSs","\147077\ETX?\GSt_P\190598\RS","H\1093976A\NAK|8\a,d","\181778\bMi\STXm>.\990469R\EM","VZ\tt&A\1074585\&1\1082409D\1107327","\1106612%JTl\NAKj\GST\987670v=]","I2\n","AJ\SYNc+\1068007\GS","y)\NAK*\NAK(l\SO\1070778\1007532Izb\166931\146691","[-~q##\ENQNU`M\1067317\a~\RS","\ETB\1053465RAL\f\60841S@\57344;r\1039664u\1091190","N/\1008346\NUL\DC4\67159","G\1002186P\US\t\STX=\DC1\SYN\RS\"\ax?\GS","",";3r\GS\NAK\26270\133033\SYNa7#\SUB","I\1021994UPpM\EOT\987667m$\EM\r","38","\1086482\1100438$W8NY\1002865\62582\ESC","7\"\1087736","\SO\187255A\ETX\983363","\1057167,Ke)(","44(wr\CAN\FS\1041803a;","(","\1057705F\189331\1086238;X\996130S<v\1083967@","<\986852\auv\1093625\1002636\&1%\1092452\188896m\66434","'6\n*\NUL@,\999664\EOTI",";?\f\t\1009390\8187hx,up\DC3","\184520","X\ETX[","\60927\45649\1072319~\1081998D%e\983944\SOH1(\EM","t\987060Z\RS{]\174566`R","\1113101\b\EOTN\STX\RS,jru","\1027299C\DEL\989227\&5bLK<\SUB\984597G\1031458\&9","\1103002","\r\1109448\12004\SYN7\142660",""])), checkHandlesNum = (unsafeRange (5))}
testObject_CheckHandles_user_19 :: CheckHandles
testObject_CheckHandles_user_19 = CheckHandles {checkHandlesList = (unsafeRange (["FJ=H\177441|\24190\1011334\41090\DEL!+","o\ETX\148809S)X P8D7C\1090102\&0","\SOt\1068256","Z\r82s\EMi\GS\EOTj","Q~","Q[RYl\58093t;","\53078vMT\1031258k-","#\NUL\ETX\"\176582It\93843","2\1102235W\DC3#i\SYNQ\1072836\1109053","\r!;v\1052647=!E"])), checkHandlesNum = (unsafeRange (3))}
testObject_CheckHandles_user_20 :: CheckHandles
testObject_CheckHandles_user_20 = CheckHandles {checkHandlesList = (unsafeRange (["\1094458\1054959\NAKO\111000\1016575\991244'#\DC1\141620\DC2\SI","\GS\60832\98713\1018608Z\a-\136651NiV","*\1047494\STX\ESC\63424(^P1","\DC3{(\999876mJRAz1z\97309\&7","w\135186\1021257\149157\"{\18320Y","c\SUB\SIHpt","MIf&Z}([U\DC4\STX]","S\1094420\1049826\"I\172859\119536 I\1086616lv","8\9819B\57375g\1060900Bppx\74094\DC38\100469","\tc","F\SUB-\USg\DC1~\1030039\181531\181945\998752uJ'\1113777","\155957\1006313dMl\1081087m&)s","\FS\SUB[\139072\132095*","\34036\&8tc\f\ETB\162268\&5\189631\30553\STX\1096482","",")I\144993\132066\68333G\ESC\SUB\r\1098723",";Az\51362","Us\DLE\1056202c","","rI\1058804p$\ETX>\ETB6a\50413K\148805","\a"])), checkHandlesNum = (unsafeRange (2))}
