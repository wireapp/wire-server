{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewProviderResponse_provider where

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
testObject_NewProviderResponse_provider_1 :: NewProviderResponse
testObject_NewProviderResponse_provider_1 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000011-0000-0042-0000-001300000038"))), rsNewProviderPassword = Just (PlainTextPassword "\"\GS\1062922\&2\1099136\180679 l\95903u\991354p\151752Q`oL\1102490v\125217\ETB\n\SOH7*\SOH+\78743[[Q\USr7c_\1066827\153323\b3ud\1060149\31864\SI[7\148791\GSo\t\1085697B\68431[W\SI:^M\1007048qC\165533\96525\986565u.{O\70285i]\156337'\1046419S0v\49168^N\1032493;\DELI=\SYNLg\DC2\166971\&5\1060363P0t\1109851\a\ENQ<\fy3\tJ~#]\r~\153959\1030349\1017781Y2\1084241V\SYN\164638\v\18970GO\137051\ETB\1070651tF\1004213/\ETB'\997962j\169574\ESCa<\1005048\1075548\ACK\1012837%[A\vlTM[T\FSj\54244\1074959\SUBt\1043904]\FS-\1063076\v }\t>y\DC1\49946\NUL\1032969Ms\SYN\128929m\142783S3Q-B\v8\ETXW8<.\RS\1028335G\26457\1022941\CAN\"\2757\1080582\989208F)\131576FsU\1108965\1064629\ACKY]Ku\CANrx\16709:\1038514}`\987741_%g\1088608x\181050o7c\999000\f\DELr'*\1097364lY\t\DC4d\182041Y\1073674\f\EMc\1041375M<\1007369\138501\119091\131461.+k\RS\DELCKw@v<iaKs\1070962\1019467R\1107090\&6\1059785(\CAN.a\1070857\1086669}'\RS\1040929b66\DC2j&BT\1059446\29890P\ESC&\49783\ENQ{e^>\SYN\1044737]:\163861\132159@kjc\SYNF\1090413F\187173]j6\49644v\1086191\r\176091\bpj\1107632Ko\8693]\41082'\SYN\SYN\r^[]!|\DLE3\FS\1031208\1112463-1\131246\1099487K\98653\1089904\a.\US?\\\1103083Ro\\bf\"\16880\ETB\143115\&3(@>(\SUB\EOT=\1030998\US\SYN\FSr\ENQo5Je@+c\CAN(\1045180-Yw\1000525\990713E\RS9%\EOT7)O\150158c^\ACK\48327\t\SI<\83085%\STX\991571>Ni)>4\ESCBV\53561\1026853>R``Dp\DELqLjC!\175107V\178004\DC4\12197nl\DLE^\ETB\1062250(\7328\&3\4181R2\152578\1101742L:\160321FU5\96232`\STX<\SIF\8748q2\DC4`-2t&\DC3\SI/\1062755\bpHs\52100\&9\NAKv\DC168*\169819Y\1110190\1036631\1029971\27196c\63501EG+R\121507:\CANg\100590P\r\SYNb-B`\rHTb~\NAK\DC2!\30578~,\1048592\SI\bf\DLE\983617Q}\29703bh\\&\FS7J\53316\ETXXZ\bDq\r\RS\983961\SICNcYS*%\SOH<<q\100865\1107476\159807\ETB\62235\ACKa\34879\99208<\CANY;=\DEL\1107216\ACK\168411qvN\CAN=e!\SOH wE\bld\RS\2065D\r@)K\1103371|\62183!\119571\167769\1089153^#\ETB%9\19657M\SYN\180933\GS\SYN\127100`\94480[d#Q?\ETXiy\1036975i*+\ESC,0t\CAN:@0\1023549AX\bn")}
testObject_NewProviderResponse_provider_2 :: NewProviderResponse
testObject_NewProviderResponse_provider_2 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000063-0000-0010-0000-000c0000005b"))), rsNewProviderPassword = Nothing}
testObject_NewProviderResponse_provider_3 :: NewProviderResponse
testObject_NewProviderResponse_provider_3 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "0000005d-0000-003e-0000-003500000080"))), rsNewProviderPassword = Just (PlainTextPassword "\STX~-pCI\1038899\STX\1052472'xj!\1070344F\ACK\4985 \EM\CAN\163412UA\NUL\1005970y\"\1070993[\DLE5\f\160732QGz\CAN\ENQ\52466\987405M-\1033012>\1008691w\16204nxZ'\NUL>\985465?\1007932E\rD\1003431\RS8A\161608\144760z~}\1080761\1081529!7\1076048\&3\r\DC2\23058\STX\985807\&94\1039537~^!hp\SO\1072348\178147\f\1029772\td_SD7\SUB\RS*\17705e\n\ESC\92951-zW\1018087j\1072504\159235\tR\166871\&1W*d?\153363\f\DC14\97318\SI\SOH\a\NUL\ETBR)C\1071984%\96682wL`\74525f,V\b8\133224\987541AJJ\EM\1005000R\DC4\1047617w\SYN\ETX\t#[n\38605\1108599F\v\ETX\tW\137850\SOHBC_mm\157958y\166588\57747F\18374Al\1024236I<.v\50949\&3a\EM\NUL\DC1\1091151\95854\61683")}
testObject_NewProviderResponse_provider_4 :: NewProviderResponse
testObject_NewProviderResponse_provider_4 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000074-0000-0030-0000-00430000005e"))), rsNewProviderPassword = Nothing}
testObject_NewProviderResponse_provider_5 :: NewProviderResponse
testObject_NewProviderResponse_provider_5 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000013-0000-0055-0000-003f0000004f"))), rsNewProviderPassword = Just (PlainTextPassword "\SOP\ETB\8771<\DC4I\172934_\132895;pb\DC4A.\1035805\DLEc\190335\17999\&6gtH6RP.\988427;\170077VsC\170384!\FSNd\DC4\a\1043080k\139428?xT\998596W\1090083\1025403FYA\1059465k\12728\&2\2997O|K\RS\95797%t\nV;@\1110142\1014997\DC2\1038346\1109657M\1098137\SI@\998182P\r`\45787\EOTe\1052639S&\ETB0mo\NUL\156290\1038619\1089700@F\aG^\131634\&3t\v\1040639sat\140126S\989311P.\CAN\54682\39731\t\169613\SOH5c\"s\143053i\r0\143811\ETX\ts\ACK\SUB\DELK#>4^T\SUBrsXkZPJ1\1110342&a\46749\STX\163951\1052328#\1019944C^\66443\17276\&9D`#\SO\1020649J\98550\SI\153166V\US+\1023875\156614l\ESC\1062149\19850\FS\ETB\177614\158850J\54577\DC3Zp9k;\CANdmzeI6\190863$z-FBDy\DC1e\46158\&5w)\1068521\47048\&2A\26084@\SUB\1090077P\1009608tBhr\1004341\RS\ACKp\fy\1107153\1003529\DC1\29331\1000217_`N;]{gj\r\SOH\1113897\1078146'\135516\FSc\n\CAN\1045728#R{y\tG?]\SIM$\146238<OcH0>\93795}E'\ESC\69653\1089051\GSLi\1089473\fs\SIY\SO\64528<\SI P\STXa\DC4\CANkTS\\yZ\ETB\b*\141187\94052\983264\17924\"~\1106337~dN\43532\170433\ESC\143076XI\STX\168723\42811\&0\1053190wu9\31834\DC4\GS\SUB\FSX\183043@+sEGR\ENQ\ACK\1102947\r\NAK2H\1087201Q\181201}\1017314)\ACKG\60985~3sWzZ\46039\1068060\SUB?\168558\CAN\DC3\CANbl\"\SIC\168799%\1072521$*?\151752l\1077404\&9\EOTNp\132882@su\174832\SUB\t\a\EM\SI8FXP^QJ\n\DC3]L\188557\DC3\1094156\4994h\SO\atu`\1477\1097028\&5>=6W\1017834tJx\DC1{{\172169{\STXq\148932r\GSy%P\1096853C\1045357()\54539\&4\SUB\1104246T\\P\CAN\24851\998891\NAKP\99451\985129\&7G|\1094037\1064776\DEL\DC3\72711\1094590\SUBv|Y\1107266w\171592KgFPXR\1022863\RS\ETXk\ESC\ACK.d\169274N%i,y\NAK\1113699J\156184f\1085545\&3P0\1081262\41880\155518\STX%SV\t_G}\ncr`Y\b\b&D3\1027928s\GS\66884$\nOBr\31094\r\"\188056\1000785d\10109\EM2I\993645'\DLEGYs2\990334\DC3r\f\ENQ\41688\&9^*k\1021487F\1092823G3*\EOT=v\DC3v")}
testObject_NewProviderResponse_provider_6 :: NewProviderResponse
testObject_NewProviderResponse_provider_6 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000017-0000-0028-0000-006c0000006f"))), rsNewProviderPassword = Just (PlainTextPassword "?Q(5x/\1100538\NULxS\ETXk\SIg\986249\&8\f;pN\NUL\1018206m\t\118801;`yB:O\1058386*\1011235`\78178sX\1021033y!R\65515\&1o\1081263~\SYN\177915kT6:i\td \1008899\1016854s\r\vH*'{\nw]lA\153918\ESCvA\2972\&8\NAKK\1024628K\39180M7\1028751E.\121396=\983124\rPtbe\1087206A<\1099981\EOTf\NAK^{\SYN@\t\EOTc\1048747O;\1020486\&3hoQ\1030755\f\SIn\NAK\ty\b}:\STX\DC1\STXf|1\986844)\987427\SYN5'9\149480(9U\125077CRwo|\1063163\163902\n=Hj\1084152bn\a\n/\149811\1005051\ESC\36961\1068962\9687\48800\EOT\111338\DC1@tCN\n\rzv\163236\&8C\1105164_\69680HW\132162D\179775\&5!\US")}
testObject_NewProviderResponse_provider_7 :: NewProviderResponse
testObject_NewProviderResponse_provider_7 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000013-0000-002a-0000-002600000010"))), rsNewProviderPassword = Nothing}
testObject_NewProviderResponse_provider_8 :: NewProviderResponse
testObject_NewProviderResponse_provider_8 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000012-0000-005c-0000-00050000003a"))), rsNewProviderPassword = Just (PlainTextPassword "\ESC\1087322\1070754)g\83157\NULk 3tZo->\r\FS\EOT\ACK\6791\1056190\DC2Uq\r\1036552,;[=\132099\&9\DC4\1107110\DC3&\ENQT\\OQ\r\946\RS\50520'kC:\DELe\1060809\r\22577_\1110368\41407h\39058-d\36632\DC2\1062999\DEL w=\78603\DLE\1000887Np!\165728TS0V_\EOT9\994532\186625\DC2\1043442O\SI>_\DLE>\1031675/|\183471-\33452<d&\1098340\992364I\996967g&\EM\GS[\t\ETB\1006213\STX24\147122$\1043906\GS\180340t\187605QE'\SYN2\8000id\DC1\1032292&\144839~\154887\163996\n\NUL\b\1033257U\ETX4%ga}\NUL*\1056383\1018344\"\1014412\GS\CANMV\ETB@\RS\142622\155953Z\1106935^?\DC3y\a\"\1080542\1098388t\178238:n<\1032214\1086463\FSY\1082060R\39859\1024922\NUL\SIT[\1079806\US-}\182866\1053830Dlr\NAK_\53042+4Uf\n\37060u\67182z\EMY+/\1053620z\DC2O}\USq\r\STX\"F}\ESC\r\SI\DC1H\ETBm\989990OSb-\ESCU]\ETXN\1003101\135753\RS\1083983s\183811O+d,\US1\1112227dh,NT\990283Z:%\63940\1061496\DLE\1078654\1009724}\t\57710\SOd[\DC4\15525ql)_\US5\b%K\1016209ZC}N]\1011987\1020120W\"\b>[(cLu=\995877\STX\11809\DC2+K^Q\1057737yu\1085127\1022302%\1030201\125137P\DLEO\aZR%\NUL\r\1043392LI5\1016718Mu4\1112488\tu\DC2\ESCa\1069214\NAK^\t*\DC4b\65927\48378..\ESCZ\119959k\31874rMVO\20130r\1032196%.\ETX\1040562\t\DEL\r\bv\1097209]\183511<N5\FSV*%A\DEL\1106169\GS5\1088615B\SYN\12600('\a2\SUB%j(^-~\1084266\1113315p\166226\154427P\1087698T{XRt)A$\STX\1015829b\1027427\186461 \STX\v6\FST\74028\1013142?K@-X\1054415\986051q0\1074667\18447x;\b\179357GY\NUL^hc\1075834\EOT\aY\RS,\DC2\46902\DELQ\1099253x_1\DC3")}
testObject_NewProviderResponse_provider_9 :: NewProviderResponse
testObject_NewProviderResponse_provider_9 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000066-0000-0051-0000-003b00000066"))), rsNewProviderPassword = Just (PlainTextPassword "ftc!\1047490\DC3\SYN\164034l@\1051560\157162\1003282cK\986029#\SIA\ESCO\57692\1041500[Kepq\\\ACKP\DC3CDz;\US}e\CAN\64487\1062955\DC4\34388\DC1\"rN\1071975\1012287p$v\26100\63258\1077845\GST\1077366\\\rw_\bx\175915\1086014Qs\1066155\ESCl\181330NB\ESC\179400\&4)\1006117f|\b\1097183\188374P\SYN]?n>\ACKl\ETBMt.\"\CAN\1064135fx\t\143139\SI=}E:kgOU\1012398\SI\GS\"\175436b\\+\1083951\1080279\t\FS{a\"~]^\1061369Y~[.\ESC)\171179Ec\FS\"h6E\"}2'E%\DEL\DC3g\DC2\EM4\1102650\186675\162369p\1098153m\USN+ c\CAN\167822k\SUB=\172963Cu\v\NULAA\1107980\151549\&4PX\ENQs\169214\&3|Ppo~T\1088274\ENQ\SUB\SUBz\EOT>\f\SO\127117rrJn\1029841\1046635\99376<cj\\&\1030040U\1113393\159684a\31715 9T$|~9\26217\DLE\1082517\\o/\1003051\30587l\126979e\SOja9\185242/x\f\SOd](\1033336u\9355Z&?\136137gZIz?\1026102\1076769\1114053P?~l\1058842\"\r\137984p\100554\137359g\166296\b\1074520\NUL\NAK\1092405/\53990qj~l\38397N\EMU.$;\1090774\1074976n1\54294\f+\ESC$f\NUL\64577\&1\DC4)\b\1020279\1094497\DC2\179757\DEL\f\SI<ZJ")}
testObject_NewProviderResponse_provider_10 :: NewProviderResponse
testObject_NewProviderResponse_provider_10 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "0000001a-0000-0021-0000-003d00000029"))), rsNewProviderPassword = Just (PlainTextPassword ":\983183\1073699;\99356\178463<\1034603dj4G5\32350~\NAK\1039600K\ENQp\169576(G4\SUBD:'Z\f+g4\CAN}\66440\&8H\1084830\1025200\111343\&6\1100072+Ah0p\1023298\DC1\1112862\1108163\ACK'\RSKe0P\152868\FS\133520\SI\61549\SIK\165103\1066948\77844\&2Zn\994886\1112545x\SI60(e}DN\137633\181303\SOS>\1102356m;A?\NAKSI\188447\DC3!\147438\140276>\1046560^\US\20141?\aS,L\987744f!2\ESC\62479\990709\EOT (S\1034073\&0&VZm\1046218\1028348\178555\f\1079530\171184\82976\1050456+\RSDh}\144891 \DLE\156704\30205b:%\rR~r\FS^iE3\EOTiml\57490\US)\186070\NAK~\127261MWc:C\DC4\190290\66738_ l\SOP\CANxj&}o>\tB\46228v\161173Q\166264{a.\SI\SI\EOTP\997770\1111400LG\1054619-OD]rD4Z$\SO\SOH/x;\1029626(F\1108881\DC1Y=\51773\&5\132386\SUB*\167650\NUL\ETX_\ACK.\v\"o6$^\DEL}\\\44841o\DC3\ag\DELh\97018QkzEU \984926\1050196\ACK\ACK\151292Vg\nQ(\1011082\26150\11056`M:^X\ESC\1029284\71910eFh\"\1006102\148015\113781}\ESCp\1029714\&4\1066288-:\GSU]vYQ(@<\DC4'\142315\ENQ8\3395`\1029412\1033693}F\1079880\ESC\ETXw\1021937R\USz\162016\&4\EM\ENQJ\1022308\22945\36848\NUL5c\SOFKc\1103313\1047254\DC4\SO\STXo\29187qV\SUB0k\31339]\983353\f\USN\EMd@\1111955\bj\1008446\1109847l\DC1ye\ETBGu!2Gn\1063728\984423\FSZ\US00")}
testObject_NewProviderResponse_provider_11 :: NewProviderResponse
testObject_NewProviderResponse_provider_11 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000037-0000-0023-0000-00320000000d"))), rsNewProviderPassword = Just (PlainTextPassword "\994155\DEL\SO\1092685\1033053;\1064T\USL\986368T\128503N~1\DLEjMcq\1083804T|\1052774\1111083\152458\&1\59710\&9\DLE\11653\EM\CAN/\FS3K\177287\1055889\189880\165104b\1089001\DLE\1069617\1080942\71465 Z\1083593^\EOT\DELB%\STX\SYNH2M\NUL\tl\f]\183386\SI)\132502\ESC5\ENQ-6\\\150815'\993462qpV\1039849\&4\1043699.\ESCTzW\1012504[ql\b\SOHp\1080925\1090320{dL\\\1106984\FS\DC4\175493Im \180718\STXW\a\1017811e\bW\99878\SOq9\t\1068974\8453\120472\177443MHT\tC`\NUL\38443\1036748\FS\EMZ\179803\15429U\1054187\1079524\&0\26657~n\1088053e.A$\19380\25601\rM\EOTPEotAo\1082541\EM2\990352\FS\"X?7V\SO[\177191R\1013254W\24463o\ACK\187397R\1011962O\STX\STX10h']\1067622g\1033663\1001175\ESC7|yZJ\149868\SUB \54451g\153761\47899[R.\985958_\991927\169928lK`>\ESC")}
testObject_NewProviderResponse_provider_12 :: NewProviderResponse
testObject_NewProviderResponse_provider_12 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "0000005d-0000-0023-0000-00780000006c"))), rsNewProviderPassword = Just (PlainTextPassword "!\1079475\1091723|T\DLEvM\45460\DEL\ETB_\989771u\134027\120529\1043092\a\139102\DC1\NUL\1073093\DC4w9\DEL\139337\83273\DLEbS\"L\65415\992570V\1018760\1095202`Z\39589\1016296\1008243P\ENQS['\DEL\EOTf\t1V\v\1067978\r\180779\NAK\1110189\96198\&8q\1103496\&0\DC2\73781\r\\1\DLE?R9%Ub\1069798^z\SI\SYN2*\ENQ\992907\DC17DZ/\1058319\72823\1014013\EOTS\29878\987521>\162952\"4\1020512K\190632\169057dT\1091318#@\ESC\ETB\1039129)\184389m\RS\DELAg\1111332\26041\ETX67\ACK\32457\165513\187938\ACK\1006133X\SI\22649Y\DC1YY~\184107\984317l14T\1105304\&2\13294\US\1049311X\1032261\ACK\SOH\f\ESC\GS\178493[w\v\ACK\EMA\1005752\1073016\ETB\986006\ACK,\SO\NAK;bwzY\1065363\vj\DC1\1064782{7\ag\SOHIY\1019764\1108122\&6\47577\&5e\ESC\742S\EM\1091400:(=\DELNBm\143790\1031657\14827Y@L\1090381Gx.\1006380df@\1025809#]?\SUB$\r\NAK>k\181295\ao\ENQ\DC1G}_\1110343\SOEt>U\DC1!\DC3\1082974\98320gi7\1037058\1052916\1089643\CAN\"l\NAK\RS\CANI\SUB\1079023\110694\23632\SO\1001092\1032260~'\\\146509\&7\99860Z&_=\DC3\NAKK\SOH)3\1076005[W*b\r|h\1069214e\991873E@@0tPpW\1113938*e&;\987340m??l;\5243o\140623\1033873\27322Tu\65427&A\189964Yo\DC1>\1105303\a>\v\DC2A\fw+m7b2{\1034602cr\fv;\SUBe\DLE\34861wA\178524?p7kq\1039446")}
testObject_NewProviderResponse_provider_13 :: NewProviderResponse
testObject_NewProviderResponse_provider_13 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000038-0000-0009-0000-002f0000001a"))), rsNewProviderPassword = Just (PlainTextPassword "\DC3\EM\"\151001\EOT`\t=\189750\a\SYN\1011386- X\1002164\ACKZ\179349[\34066m$*\13748HS`9A\NAK\fir\45063A\19554\171127\STXtz+\SIw\95252\1055028\&4\EOT_\171283rs\1054142e\29925\"\"\42600\NAK\4926\1092706*\GS\141880;\NULC\n\7209\a\50010+\7503;\67322o\fI&\1090689M5+x;\SOH\997674\ACK*7\RS\SOHTM\1009127kI\1010594\&0K$}n\1984\1063608\&51\DC1_>>\a@r\ACKA\ETB\120505XZ\DC1v\992102\40701\1043343\&0\\\ETBP\21107\EM,\1010981U\1033186\SOH\CAN\1088234\&7t\164689\1078029\1029039\191308\25147{\v\RS\ACK\DC26\1025923\72150\CAN0m=%\1039517Ih\1068269a\11178>\153827\&4!W\FS\FSD\vE!\NULVv3]c\59043\1260a9\f$|\47777\95353G\1039288m\SOH\SYN3q\EM\DLEM\ENQs)\DC4+\SIO\999057\FSlg2p\NUL\155208\DC1\194840\STXGU\USZUX\152231\"|M\NAK\SUB\137243;\aM\a\39097\&3[\1008160\186676~>\1107945\1045746V\\\1102981,&5S@\159486\1017636U.#\SI@\1097367FQ9\24968\&8Ji\999647s\1061806M\61888\&1<Sq.\"6v{Mk\EOT\48201\1097129ow>\NUL3Yi\v$B\ETXG\DC4\ETXy\1014188((\f\1113969\DLE\194999\&6rO\SYN\66903S\DC2\1053180'EK\135109Ile\161792d\SIe\ESC\STX\1013203\DLE0]\ACK.5E:$wW;\a\37932)7\DC1Zw\152956SE\GSLP\51928sq2\2002\52338}\23246o\138372\&00vaD\1061314M\132060\1030577m|7\153061\162526\\\44532\49265(\STXW\994413d\1058145F;\1033091,\189199\b,C\ETB\ESC,\1101241D;\24007\161489\v6_ GA\ESCY\985565\DC1\27499\a\DC45\USJ\DELe\1069338\DC4\151564%<,<~\STX.\DC2o]y")}
testObject_NewProviderResponse_provider_14 :: NewProviderResponse
testObject_NewProviderResponse_provider_14 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000005-0000-0045-0000-000700000051"))), rsNewProviderPassword = Just (PlainTextPassword "$5\157942GN3\176821\SYN\144746\SI\FS\128696$\SO\154009\DC1\"\t$e\1040719'\SYN\135587Rsey\144682\1028721fY")}
testObject_NewProviderResponse_provider_15 :: NewProviderResponse
testObject_NewProviderResponse_provider_15 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "0000007d-0000-002d-0000-004e0000005a"))), rsNewProviderPassword = Nothing}
testObject_NewProviderResponse_provider_16 :: NewProviderResponse
testObject_NewProviderResponse_provider_16 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "0000000a-0000-0043-0000-00020000003a"))), rsNewProviderPassword = Just (PlainTextPassword "mB\1051967\45355\SYN\1010479;\DLE~idmN\1028419\t;i~bW\DC3H\ENQ.\DC1\172107f>T2p6js<a\DELf\SIU\DC3s\1016088\1107407w\1109218\SUB\1091569ns\172605\38318\1087511\DC2W8J\a;Piw\b\SUB\ETX\986906\DEL\1003069Z\2233\DC2o\SIf\1090674\ENQ6w^\FS\ENQ\65338\140250euU_vPR\r\DC3\FSziy\54418y@\1111189\178639\DC3kZ#\991360\NUL!#\129393\39275\1019133:'Y\1068464\1026299\1020744->9H!/>\1032085'85JJL\60231q\n')#\20824=CO\RS\ETBa\1023577v\9545\24919S5\"7B\DLEwvy^\1035535\995299[\1095198\EMT\b\EOT\DLE>/L\34772W\174284}}ce<\SUB[`\FS\1106609\DC4\1032063\33328?\USj\986118%u\992118\61048\49241_Pc\54285Lbq?vW\1104604bRtq\1016043'\1099591Yl/\1109378\999247\&4\1108304]3\\\1019101\1078262mQe\US:\SO\"UI \US}-sE\191321\125237\54486q|$\1103963\1007383H\STX\158671:@1\168949\EMNS7w\1071671Q\ESC\143211\147060\\+\RS\f\RS\DLE(<?\1056396\1054420'\1053914\US\1086992\1025680\23390\48827iOs\SI\SO=\ETB\n\175084K%=\NUL\SIwz:\26091W)\ESC\SUB\b\1012051T0{K\ETBv\986741\RS\1022854\1008088\120161\DC3V\15891}^D\EM\188801V\1072429:$y\134749fBISn\DC1\STX\NAK\1028650)!a|\157268\"\DC4V xT;\46359io\1067594\n$q5[\NUL\8226\&5\NAK1\\\1027503\995690D\n=\146975\SOHtQp\\\1030343\&0zOo\137604\1035585p\rfw\1074168\ESC>f\71446w)\1018955\ETByuVtBx*\1010892\t\DEL+\144400!\GS\1066976q{<eZL_\23028\a\GS[\GS,Z\29898\ESCUj4\SIR\66447k\59809\1028459yY\1110776\"\182917\992708\&9_\16529\1058860\&4\SO~+\23011\rC&V\1090269 \SOH\1038169\1058448\"\51344Z\ACKyg\994540tnC\20006\1033527i\169855\4832\NAK_\1071883\DEL'F\SUB\1097942\&1E\44569\1053113D\14896\SO&\984367\187079\&7\nE\1088570\174966\&8\67811\&7\a\a8\167202Gkw\DELR\rG\SYN\1066336\SYN\27999}\1066090\1109565v(")}
testObject_NewProviderResponse_provider_17 :: NewProviderResponse
testObject_NewProviderResponse_provider_17 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "0000007f-0000-0033-0000-008000000023"))), rsNewProviderPassword = Just (PlainTextPassword "i\162255\1040833\DC4a\f;K\71132y1\1098500ADa&c\STX\34003|")}
testObject_NewProviderResponse_provider_18 :: NewProviderResponse
testObject_NewProviderResponse_provider_18 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000028-0000-000f-0000-003900000014"))), rsNewProviderPassword = Nothing}
testObject_NewProviderResponse_provider_19 :: NewProviderResponse
testObject_NewProviderResponse_provider_19 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "0000001e-0000-0039-0000-000700000037"))), rsNewProviderPassword = Just (PlainTextPassword "\\\SUB\136086a\191150\40828N\1038195wX[\1003542S.\EM<P@0\1009250\177844\182393\40425F8xM\1065493I\68124n\RS'\156218L\179191\4328\&1*('\CAN1\1085004\1050398*,\EM&\187791\170142q\1050472\US\1070500@Vjg\1087165%e\183712m?GA\21084;0\54702\1060035o6~ \1092285Vp\1044678T\18864\ESC\1016208[\159449\&3\DC3")}
testObject_NewProviderResponse_provider_20 :: NewProviderResponse
testObject_NewProviderResponse_provider_20 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "0000000e-0000-0068-0000-00100000005b"))), rsNewProviderPassword = Just (PlainTextPassword "\1016627|UY\1023624\US\\O\1002412'\\Z\94900^\1047515,\1113957\1058200\1111914^\ETBpa2\FS8\ENQ*\DEL?{\45436j\SYN\1012276Z?\986332\DC2^Y}n5\ENQ\62066\&8\1014297V~G<%,q]zV|(\DELuf\NAK\DELN\DC1\DC2\189924_\ACKWz\DC2s\NUL\1019292\142762\137174\1084452-\1096766w\SUB\nF\DEL=\146008nDwC\r\rf\r@\EOTUc\64620\DC3_\n0E\DC3e\ENQpd\b\CAN|oWG\7840~\ENQD`@(\1039132E\994622\\+m\147596^\\ge\25609A/\1064691FZ+9A\GS-[YKt\ENQ}_\DC19\b(\136301\DC2\SYN\v3:Hf*)\1049069\155585\73084\141959\1065183\145611{3^Vf\US;3j'\SYN(Vd&1\1043946\1069976\38428k\DC3p\DC4\1088789\1001825\SYN|^c<Tm\1040803C?!\DELG4\DEL\NAK\DEL\1222\990220uv\"\1019110lSs,h\v\182809\1010067:\EM\1033233\182670\\\t\DC3\1063480\DC2?[\1008349\&8jv\996428\DLE)\190017\&6w\DC2\\\\o\31498.\EOT\NAK\166841PzJ\FS<^\57782\ENQ\28314{qK'\985355\&9*D\169871\ESC\5283jH\1068357\1073683-z\1090542u\30983#>\984170nk\118876\24375I\GSD\DLE^Gq \nG1\DC2\SOh\165022c \1007802\1091551\137561\tbf\178935\51144OV:\165170\24151~*a#v]\1069769\17563\26037\1102332r|\US\FS\996281\EMl\1111109l\bm\1091280_]-(_\119585\RS\39177\f\\\34385\1050333gN~\41180AV\DC1\139213(t\ACK\1017980\&6\52455\83191H\1002344%\73797\SO\SOH\1097765\&0N\1024718\1100621Bwe\1060254\\\ESC\19328\DC4(\4198\54882\DC1N\100519'\48314\1076594w\STXj\1077672_/EN/w\SYN`\1001634\a\DC3\SYN\10090I\1028257\1035354\9744!3L\1039104\194926\92482$!q6\FS,{\1087643c\NUL\1049418xImbT/\NAK\138905dW\1045723\39345\&6h`t\42841A\8199D\ETB\ESC\1068804?5\ruyO\1035482\152071\DC4\182185\STX\\nt\DC1/aoY\DC4\1095687\1078654+nr1r)\CANm\US\120037#\174140\1055981\1025803-\1000818\a\EM\1070632\25240DO\DC1G~\1080163\SODq\1084405]9\377\1104918\ACK\STX\1056594\ETB\EOT,)*\142135A\ENQU\NAK5`KJ\ENQ M%\ESCX\38676\1046985Y-\FS\131521\&697\DLE\63892r\NAK\SO\1020211\1048534}\NAK_\1028338WF!Fr\1023021\b\39328p~*\38104McY,h\1070721y\"C^\1009588\46209\21515\&8\NULq\DC4p\SO\168669\CANY\1072542\a\1028969K\1002795\NAK+\186351A\53808yO\GS\ETB\STX{m)H>nnQCJ\49637F\ETX\188928\r8\vm$cL1Ybb<n\137937=I\1102980\v\CAN&\1071497\1015084(\SOHws\5448 L\RSo \ETB\ENQ\EM\995557s!\FSl\1017573\t4b2B0\8755B\1077317\vGp\r\1110551\97654\1015840F\1081480\SUBsw|8f\1106003~\1056207O8\GS\ESC\rC\DC3#m]&\100379\v\SI\10321SdvC~nHL>\128515TB$J:=\17153JpV3A\135121\188023K\ENQH\"HP\ESC\134840G\US\1077778\988121J\1107712b,((\1099840\GSFC2T~L\146529V1\32613\DC2\151709\&9#\EMm_+\GS \STX#\NAKz\DC3\186026O\16244![\1114002\SOHEE\EOTWM\DLE\ESCBz=SoQ3B\ad`_\b\ACK\CAN\STX\74596[ \STX?\1022300\1011186\95284U,Q\a^\188093S1\DLE\1039936w3|ZRmpEA7A]sE\5664")}
