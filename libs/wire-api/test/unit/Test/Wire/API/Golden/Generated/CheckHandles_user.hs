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
testObject_CheckHandles_user_1 = CheckHandles {checkHandlesList = (unsafeRange (["va\NAK\985849","5IW\16772\&6\170595\FS\EMa\\Y[\1109366W",">\NUL","}\1017207\a.Z","\1026841","9\61245\1017292?;\986141H=\DEL5","\1067466\26501t","x\SYN\DELD}\149512","A","\172578\ah\RS\172533\SI\1093628","]\DC3iFsQ\GS\1005353>5\ENQ\r(>","7E","w\1100002H'\1090392\1097934q","4\1081677{\SUBx\151471s\1034283Z\1072847?\v","\990658\167666\1021706\1018747\1038008\EM\1019304\FS\SUB\1000837\1001344\111211\ACK\EOT\172006","r)","\1059687;\DEL!1\1080538/\917976%\1070909<\1013978\171126~,","?o\EM\1042050\917580\&4`[\14188","\FSH","a$\1088557<\156653.)f\STX)\1015467\1015111\&3","\999802X\1054448\1067641g\tjt","{+\1047369L\1104193\RS\SYN","\ETB9?","\NULB\63335","tG2M\ny\992847\&9I","\1023024;\83364\SYN>7h\159506fq`#P","w\NAK\164867\129283\187049\996994*","-0vflN\41265\34572\RS\1032597hiZ\1011284>","9\EOT\151621'\1100229d[*\SI\15975\b\t|","R\1043862[+\1087955CX\1055823","hEi2O($d{\GS\26896B\SOH}\15357","\98871","r\FS\DC4\985195\SO]\1050797\58264\DC1,\t","VU\180876O\1052926\&6y\DC4LqG*e\1108057","\SIE","","\FSi\69899 2","w\57749j\1008786\1090648\35469S5\"?"])), checkHandlesNum = (unsafeRange (8))}
testObject_CheckHandles_user_2 :: CheckHandles
testObject_CheckHandles_user_2 = CheckHandles {checkHandlesList = (unsafeRange (["6\70809\1086148\CANP\33080","\DELE{\1057483\SOX\\\177437UXI\1045473\SUB","\1062047-\22282d","\28404","S","T\1059517\23415\177195J\1035173-\1011525","\1108340'\STX","}V\DC3\SO>\US\162317\ENQN!C4\1082224","\985258Q\194652\ESC\SUB\SYNY","\SYNF\78237ZC|\144637","bU\SYN\1095606\fK\FS\f`\ETX"])), checkHandlesNum = (unsafeRange (5))}
testObject_CheckHandles_user_3 :: CheckHandles
testObject_CheckHandles_user_3 = CheckHandles {checkHandlesList = (unsafeRange (["l\168748?z6In\RS","M\1072934O;\SUB_","\28553e\rY,n\nJ#'C\RS\993964","D\26134mK\NUL\SUB==\n\DLEygA,","Ya\t3\DC1\17734\1067156\&3x","_\v6\t\vz\18096\992507\GS\t\138999+","","\994326/\1090945\1034997\162378\RS","","\SYN","\a\1045859ql\RS\GSr\ETX44\SOu\USl","\SOHGey\74386\n'/\27197nX\EM\US","\STX\1112208\136604I\ENQ\50264\EM\\\b\1097023Ugr","Wj(","P\v\995047o\153836cK","\1014275\&65\SYN","\a\SUB\ETBB\1107391","\1107270?x\30746\USU\DC3\54575AOF",".Lr\STX","NK\988678Ibyi","t\146915\&4\DC19Se","\43040W\1033048\"\1089408h>\1104713zX\CAN\ACK\168296\1008868\&9"])), checkHandlesNum = (unsafeRange (5))}
testObject_CheckHandles_user_4 :: CheckHandles
testObject_CheckHandles_user_4 = CheckHandles {checkHandlesList = (unsafeRange (["[\1020533S\1024099\17875"])), checkHandlesNum = (unsafeRange (5))}
testObject_CheckHandles_user_5 :: CheckHandles
testObject_CheckHandles_user_5 = CheckHandles {checkHandlesList = (unsafeRange (["k\1056348H{\a\999577!an\ESC","3E\167864]\153382,Qm/","t\6320H\b9\CAN.\DC2\a\SIY","M\1002603`izS","\a\DC4uz'l+L\1112897J\t\f","g","e'\SUBLT\1047018\1053512P@\NAK]","?\\\984110y\DC1]\STX\1057771\&5\SO","iXG\FS\158447st.\1080560`"])), checkHandlesNum = (unsafeRange (6))}
testObject_CheckHandles_user_6 :: CheckHandles
testObject_CheckHandles_user_6 = CheckHandles {checkHandlesList = (unsafeRange (["{j^\SYN\STXQI\1000534","\163073\SYNT1e)\1019814\32241%\97767","\1074562\1023672aNh\DC1","i_","\1028896gwnx@K2\\? \DEL\1010882","\131873w3SoX\FS\24001w\FS","9\171475","Q\21618f\ETB\f","\DLE\ENQ>\1030301\DEL","2n\ETX.\73941\DC4!","hm>%\CAN\SUBj","f\ENQ\DC2\DC3G\RS\1013988 \ESCG+\SIwI,","\SOH\140108&,\tL']iJ","\1093100Iv\n\SUB\1015301\34290\&0","\1044288Co\tZ\DC3i\US~","\1087269`\1005176\DLE\92506\n\7622g\46750\1047372\\","ge\EMrV\ETB3\SUB\998311V`\ETX","^=%>\138892.az\USEe\156675\30570n\RS","^\EM","\1071429\CAN1F.h@[*\166688P","\1096928\&8G\a\49550^\67229r3'","\1099708m>\\\DC24;\1053093\ESC#","\1024252","T\1110676\1084089","2\SUB\58920&e-sh#zB\1085003","P6q{\NULM\135944\998261$z\1002378\ESC'J","H,\1032997-RX)\48817p\DC2\SI\18338\&8","\a\SO^\186243\1046082#","19\n!\179679|X1^J","","\FS\SUB\996908D\CAN","\134346u\984815\163872\"U\1052942o\167312\RS\CAN",",F\f@\SI","I\4684\"\37711v\fx","\SI\95198C}\74607\n\ETB\r","i\163216A\NUL","cc\FS\ETX2\ETBD}\bL\NUL\1104191|","\SOm~\40232\1013187\60854\47582\SUB\nT"])), checkHandlesNum = (unsafeRange (7))}
testObject_CheckHandles_user_7 :: CheckHandles
testObject_CheckHandles_user_7 = CheckHandles {checkHandlesList = (unsafeRange ([".1\173545\SUBu\SO0\ENQ\DLE^u","}\1054329:","w`\"\995243g\b\1062532t\b\185588WH\STX\34471P","I\1045402\1026377\1082387F7\FS\SIJ\\I","KL\1033747\111126\44915\1089717\1081347\1077531\25521w",";\FSp}\152572\95602\NAK$)o","1\ETB{wXN\1061651|y","\t[\DEL\1002790\GS\1072243\27195\vEP\DC2$\1067551","\1005921e0\SO?mn\1064743[b\1018578CI7","\92563\58832\SYNuw\990890\FS\DC1\EOT&","!riwn\29210","\ENQ\160872*m \164625\"I\35592\fd\1073506\118952:","\ETBm\a\1105870","","'A\989842\1003583n\DEL\997068Z#\n\144784;/","\165882|","\1112902_\1104877\&0|b\1095186","x@\FS\ETX\42371\1096200e\RSN\71437\"","5sS\168961\30256","","B~\187461]y\1076265\989701","","\DC3\ETX\189712\DC1/\74468\a\1030179\&2","U\SO\5034\25247\1031609\1064671\145642\EOT\1012097\SIs\64490B\34780","!\DLE]","\DEL\NAKm\1061410\&2","\RS'","Q\DC2\ETB4YJw","[","\DLE`VK0q\ETB\991503&\NAK\SOH","\1088936(","\1043395[\FS"])), checkHandlesNum = (unsafeRange (7))}
testObject_CheckHandles_user_8 :: CheckHandles
testObject_CheckHandles_user_8 = CheckHandles {checkHandlesList = (unsafeRange (["\156834\DC4.","7\1052776\1077250\GS@\DC1\ENQ\SYN\\2","QtA\347CIz\DC1\171632g","|\EMRF","\1081946\92756\30841\v","\NAKZs","\US,%\a\GS\59853\SYN\RS\1094713{","8\188119\SUB\171414*K\10253?\DC3K3\1004783^\1047631\1057921","lk\1073104\SOH","\NAKG\16880\179309FR^IU\ACK\118807","ka","|ob","\1017625\v\990112ci\93965\&7YsQ\1101803\SO\v"])), checkHandlesNum = (unsafeRange (10))}
testObject_CheckHandles_user_9 :: CheckHandles
testObject_CheckHandles_user_9 = CheckHandles {checkHandlesList = (unsafeRange (["9O\FS \1009219\100124\172161%.\CAN?WMt","\133996\&9Y\1031667G\NAK\1028086","_\ENQ\STX\NULWY","\156746W-.M\"ty\99808\&2\97053t\97274[","\STX\99837V9xN\GS\CAN~\1104194%","v\62134\132991\1023312\46104:}\1044581i:|","0\EOTC","\RS\996620\EM\14323\DC2\152349\ENQ(K_EL,","\RS;@\EOTZ\46109\n\v","\51075N\DC3kq(B\999554\SOmX","}","","zyW\97994t\1047887\DC4\129521\136043\1025911\ETXUH","X","E","U7GQ\SYNX\FS\v","PW\134272\1034857u","\SOH}\tdg\184059>Ex"])), checkHandlesNum = (unsafeRange (4))}
testObject_CheckHandles_user_10 :: CheckHandles
testObject_CheckHandles_user_10 = CheckHandles {checkHandlesList = (unsafeRange (["B\178789\&51=\36158","\1107510\ENQ\1062206\1109413\SI\1042410q%\r","Qv\FS\SYN\1064821,","\15850\986974Gi\151443m0%","*A-!r\t<e\1107172X!b\ETX","\NUL\r\b\ENQ\\\1050391","vm\DLE'3l\r=5\1031422}*\SO","\ETX\f\1030526&\bu\78434x\168330\&6v\20195\\\DEL","j\1026974\996365","\r\FS\100587S03Q;c\1055049J\b\1086204\1087813_","\vf\5776","\ETBcZ\ENQ\RS`8\1082548","Z`\1017893*\1104126t\187280@}u~l","\143608\1470@\97415","Q8&\ACK\178773\138363Zs\DC2i","?\tojue","\178481&\f\1009976\1052064|\147691","k","\17038\r3\1002234\1050515RHG","\34529\r\1071886\ENQ-;+\NUL\28435/[G","\51702\a\ETXx\ETB4{Q\180526\174362D}\59659\&6.","+i\68920\3937\n2C=\47819\1062984\fr","\164291$or\SOHuG"])), checkHandlesNum = (unsafeRange (1))}
testObject_CheckHandles_user_11 :: CheckHandles
testObject_CheckHandles_user_11 = CheckHandles {checkHandlesList = (unsafeRange (["","e","\127753\ACKC","6\SOT^\990622)9=\GS\SOH\ESC\146167.F","w,S\151972\1056154C\nc\DC3Zx?p`","`\42485<\171445I","&\ETB\ETX\96622r\985403Mc\EM]","\DLE\1037318\DC4?#\36374\DEL=\178369K\NAK\172858\144036",",B/\DC1?\r/W(+0P","3R","\1028556","","\ENQn;\1088050\23829\rP\SIm\133998\1058678~\1035902\&8","\ESC8)]\f","H\1025937^+\1048253\1032031\SOH\SYN|R\SOH\EM","\1092368\1111051\ENQD2K","\134788\EM","R\1110139/s\SYN{\1105273fe^\SI\"\28294x ","\1039839\SUBId","","P>\FS\1021351\33712>\FSc\1030701\&5\DC1uGp\179707","\100417fd\993825\SYN,\127042\179357\998936-)","Z\94766\CAN%\"\FS\NUL\1021484[iL"])), checkHandlesNum = (unsafeRange (9))}
testObject_CheckHandles_user_12 :: CheckHandles
testObject_CheckHandles_user_12 = CheckHandles {checkHandlesList = (unsafeRange (["\1012831'","\1063108\v#\1050186\992126","\DC2RhV;","\1002927\ETXP\DLEx\ETB-Y\1080361f\1027399/","z?M(\"d%VD\GS","X\992482\v\1077237)\SUB\1007098H_\10319\GS\1084008","g","a\DEL H1\ETX\1097956\ESC\DLE\146963]\US\f!","9i\ETX7ab","\CAN\US*\DEL","uD1","k\aH\1108735QYy\SOH[\r\ENQ\SUB","v?KD\63597D\6982u\1070980","\133675gL\SUB\121121}!KT\DC4[Pvz","H&M\41938\983898\&7\47203X\62501\1069067G\b4Z","\"\v#\1021811z2","14#&","\1073925r\EM:F","3\1051708T\1011366^","\1108349\\\bmS~Iv\FS\162052\&2r","{","\ESC`O","\54847D e\SO^\1063231g\986003\153419\NUL\136532","O\ESC\1038018jHd","x=\EM\ai\a\DLE'\149058=\134066-v"])), checkHandlesNum = (unsafeRange (2))}
testObject_CheckHandles_user_13 :: CheckHandles
testObject_CheckHandles_user_13 = CheckHandles {checkHandlesList = (unsafeRange (["\163927\EOT5cC8\r\bPX"])), checkHandlesNum = (unsafeRange (1))}
testObject_CheckHandles_user_14 :: CheckHandles
testObject_CheckHandles_user_14 = CheckHandles {checkHandlesList = (unsafeRange (["y","\1091245","\137220{83p=B\1061525\SYNc_\DLE","\SOH","{","f\CAN\1050483","qm\149243u:\ETB\47734\122899\NULl","\1106636@>O","Bx\DC27W","O","\1015461P\NAK\1106986t\185352uB|\1100566g'A","\145911\166309=\SOSA\1097476\1006688\ab\r","q\137240u\17457&\1032002\DC2ht0#U","~","?TX\FSi\DLE\158603(","\1074295\1074899X\1021410V\46322\1107270'2p\1104607\1021470M\US\STX","I\GS","(1\US7\EOT-\1112197O\1029967\&7\985995fu","","a\31951/","\ENQ\154965","\151679B\36903\128513\ETX"])), checkHandlesNum = (unsafeRange (2))}
testObject_CheckHandles_user_15 :: CheckHandles
testObject_CheckHandles_user_15 = CheckHandles {checkHandlesList = (unsafeRange (["","^\1046012g\SUBJ\131894","\\\37637\n","\DELf\EOT\48310}6de\18089","!\1040405\996341\179414\11983\ETBjJ\44596\181293","F","\tt\SOHB\DELf#_?\148293g","\b[/6\f\v\FSV","\986619","f2#+4h\26968\&7\RSr\SORp","V*\NUL.jQ","h8Zq\15408\ETX\ESCd","","y\1069294","]\ESC\1105442\179698\1046044J^k&~\169247\12456`Hn","","\ENQ\128443\1096855%-0","\SOs\1061878T~\1064206\GS","","7\1108118\10256\1069796bDC\DEL\bQ\EM3","|R~+_\3550\1015841\&6\v\8103","\60334v7  W\1023352gj","\b\DC2upaUrK\2304Z","iY`\SO\98114\1001886W","w\95978\ETX\31964\168448","\DC3J\1018693","","\GSL"])), checkHandlesNum = (unsafeRange (2))}
testObject_CheckHandles_user_16 :: CheckHandles
testObject_CheckHandles_user_16 = CheckHandles {checkHandlesList = (unsafeRange (["\n\SIn","oQ","(M6aL;\1019347\97851b\NAK\5907\SYN\1036258","u\STXv]@.\18615j","s&\SOH\v\DLE ~","\DC3!F]\RS\19154\92497\1073951\157417\160556\t\1059190\1034500\25364","\1018410\136467E",".J\SUB","\170689&\1015186p/\DC15\82959/\1029695\GS\14580\&2I","\ETX\DC3","6\DC2\77864~v\fT@G\1075420_","\1001992/j\1012574V","\SOH\59153\t\1104237>\126479","\1094084|\b\1039667\159739\SI","\SI.&N\136443 \\O\1028851oCnh","\1017246T\1068114",">\DC2\t","","Na\50093\988659\131095IJ\\n\1028669\t\r-\r)","{\8548\EOTL\997803G5","","=","","F\\\SIB"])), checkHandlesNum = (unsafeRange (6))}
testObject_CheckHandles_user_17 :: CheckHandles
testObject_CheckHandles_user_17 = CheckHandles {checkHandlesList = (unsafeRange ([""])), checkHandlesNum = (unsafeRange (5))}
testObject_CheckHandles_user_18 :: CheckHandles
testObject_CheckHandles_user_18 = CheckHandles {checkHandlesList = (unsafeRange (["\1079630","\1111808","n2\155191;\SUB$cc","\USY\28350\985651\ACK:\16357:=hB;\1023783l\DC2","Z\1024929TI\99953JI\SIh\\","\1069712L\r","F)\1060901I\EOT\158021Y?\DLE\1064292&\n\SO\1018502","?iQd\1021633\983102@\1020923xT","\164468\1013129T*\1029407h\1032877F\v\US","fBH@\f\1083565\EMy-\US\138634\\z\154928t","l3Wn\1101043\r\DC4\183889\1000090\1009404Ro","u\EM\179171\&3\1070316\11690\59210%o\1011589\DC4A","\1084823q{eeB5\999705/lp;","\1046982\1046710t\vQUO\987767\1030344fa","\DEL=\STXY}h+\b>\RS\160611","`\DC1\FSI\n\1011983","\1019839I\DEL\42256\988566\SOH\ETBw\54950).'\1058704\177528\31977","","\1029885","\EOT\SUBK\1061222X*[","Xk<\SO\1018788-\STX;O\97447>\1007411","\RS\97787a\133246.{V@x\\CG\1109301N"])), checkHandlesNum = (unsafeRange (7))}
testObject_CheckHandles_user_19 :: CheckHandles
testObject_CheckHandles_user_19 = CheckHandles {checkHandlesList = (unsafeRange (["?\SI,\7549",".\1112178\v9Y\NAKix",")","\DC1b{","\171707lpMa/VJ\1073914\EM\1036556Pi\\","H\1076007\r\28173$V\ETX\1077685\991367T\1113992\13110","n\145437\SO\1010142vu\DC13\US197","xg/`!P\148284\&8L\SOH$|\DC4so","\37762\152920>bv","!8\1062873\&9"])), checkHandlesNum = (unsafeRange (2))}
testObject_CheckHandles_user_20 :: CheckHandles
testObject_CheckHandles_user_20 = CheckHandles {checkHandlesList = (unsafeRange (["b\1017701/","a\ETB\CAN#\ETX\1060824\FS\DC2Ee\r\1029963\34403","*\1052676\31765$h=","FO:2","\44870Zf\n\DEL\ETXM","[L\985439\59037\148269",".(C:\RS","*\1088378\USP\DLE","\ACK","\DLE\1055695","&]\FS\SOH_H=?\CAN","","Z\69607\a,\59915qby\96880\40482\RSS\152739rn","","Q\US\999906F\1048763\16518\v\RS6_D|","w%\1088089\69996_\DC3\n=v\1067432b","\DC3\74090\&78","\SO4\1034289\RS\37993\1017377\ETX\EOTs\DC4z","\STX\SYN\US\SOe\1089362\991588p\GS\ETX\SI","Rp\94417$\DC1`\f\174477","K(M\DC1\27073\ACKU*\1046114R\1051258|]\1012921%","q.\a\EOTi}C`","\DC4U@Ze","\USM\n\4830F\855\&7\1077056\GSc\""," \DEL\EOT\ETX\EM\DEL$\DC1 _\v\CAN\SYNs\1046976","kj=\8320\139349&8;","+\SYN,\187385kf\1113818\143677K","a?","\52068l\"pTctp\95997\STX","V\ENQa\v\13168I\1092970\a\FSs^X\1021025","\\<\11270\\\131802`"])), checkHandlesNum = (unsafeRange (8))}
