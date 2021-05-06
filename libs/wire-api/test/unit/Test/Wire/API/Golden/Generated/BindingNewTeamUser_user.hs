{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.BindingNewTeamUser_user where

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
testObject_BindingNewTeamUser_user_1 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_1 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\1078138\135794\1061247q\34595\1026088mkxc\1018533J\185055\DC2Hs9\989098'{6\65483Gq9'H@~\SUBj\1055349\22756y\45208\1007043-@j\33196\DLE\1094303F\126089\&7\1048034\DC15\1074249@p2\b\54546R~\166549\DC3/\1023593K\1054794AJ\DEL=\CAN;{\131814\34733\98222,V.\1012361\1091455\DC3\1068033q\43303c\n1\n\DC3:cml:\1096142\RS\a_\v9\1110948l+X.\156581vl#\144785|\DC3\140470xJ\1104071R\\e\NAK08f")), _newTeamIcon = (unsafeRange ("6\176131\"v\FS\DEL]\1035176\DC1\DC30\NAKj\EMU\27821\1003682\a\GS@\170788\50209Q\1089056m2{L\ENQh\SI\173052\EOT,\1015638\&4)^JL0ah\183348\73056\12880\32269)\64108igG1A\DC3\f\172639!\a3\SOH\DELU\54608L'\GS\1061871\1106663r\1100102%fE\23142Nb)\SOHm}vk#\"-;4v\ETX\NAKT<jc\1028896\99670fj\USwwS&\1023173\DLEU\989510\48908O\\eS\998483\&6\64956\r\SO\1004597W\1016191^\174121\ETXLDb|B\ESC\94851\1001615\b\152182 70\1007739\144184+\158022\DC1\39031\SOHg\DLEG',\t\1037794\1026530\f\ETB<\DELQ[\DEL]\SOH7\38041v\SOlg\DELS\1061653X\50499\"\126065|8so~\\6f\1017490O&d\"\EOT38\1001284MR\v\1092231.\1093782\1042080\1067287y\t\1033056\USf\ACK\SUB\vQ\1087576\&1\1083949\1031731\10989\&1SgF\37317<\ACK+R\SYN`m\131848z\1071449\1057145;FGG\ENQ9kJ\DC1H\"\SO>\1113403G\983793A\GSA2")), _newTeamIconKey = Nothing, _newTeamMembers = Nothing}), bnuCurrency = Just ILS}
testObject_BindingNewTeamUser_user_2 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_2 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange (";p\984019U\br\DELBc\FS\NAK^\1028099oJ/K\94291/&\984199t\998430\1026629o\67330G\179920\100508 /\171666\138299w\DC3\1019425\NAKV\f93sVm\RS\10673z\GS\1036463\986112bs)\SI\1039248^\b\71905N2\33828\997556>^^\1033246\&6\4034CZ\148649\1056106+@\1097216\173990R\vaV\DLE+\1062856\&4\nP\62093\&71\158116\1031947X\1195\SOHRd\1102662\1033271&,X\178855tv\STX\\*Ru\988214d\62671\1019408 ]x\37599/\1025905SM\DEL\1018908\&2Hb\35372\v/R2o\fF\28235W\DEL$<qD\US\ENQ\29032T\ETX&\1033784x3femtI'g?\DC4\21806=\US{8\a|\1075892U;")), _newTeamIcon = (unsafeRange ("M-\1020975\8207\&7\DC1\1035134\1017917\1069045\1091487\1026911_Jya\194764\ACKj0\1108032H \143048\&8\156612\rK=\156953\SYN\1056703\\)6(\1103130\FS6)8\th\a\113757\166667$\ETX\ACK\1082886L\64021\\\1072195OpY\DC4\74434W\121343\145934F\137631\&5B\168079\ETX\DC1h\140370n\EM\151845\r}=}7MqV\b\158209a\165936#_\996941 \NAK\1028278\GSo[H\rj\SIhD[\DLE-FI&\1003532P$\SO|G\SUB\GS^\986465+\32109\&1\ETX~\1029715C\1065849x\175560t\179150\DC31s6\1033701\1045242\SYNV[T\1074702qQJ\1103141\DC4\1087863PKd\60605\DC1^n\1069962\1052188\1009376ZM\GSB\ACK\121335\n<\CAN#\ENQ\US\DC2\ENQ\149908\1108108PA\1037853V\993785\53891\70053\1069336ex\DEL\DC46\NUL\1100346\1027404\DC3y:u:jF\178602LK3\20454\DC3\1347\&6\999809(2\1093731\r\1027127=B\EOT\100424\994301(v\SI\";U\65375\1027579u^;1B]\1054792Ve\GS\22123uk\ENQ\990474\1105246\997150|\b9")), _newTeamIconKey = Nothing, _newTeamMembers = Nothing}), bnuCurrency = Nothing}
testObject_BindingNewTeamUser_user_3 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_3 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("7&B\EM\1047933<Nvq\SOP\64892\ACK3Bq~gL\b@c\175538*E\1067987Y$}\1001683\&3\DELr\nWF\SO\DC3_saoW\188054C\154857\1085997\NULvP}\991763JLV\179289\1028257\1106530\987887{;Z\t\ESC;\SOH\DC1v")), _newTeamIcon = (unsafeRange ("\STX\190088\a.\26450\53211c=7U{\ETX'\1067557\1094121\1027480inxO\1085599\54940\1073262\RS$\54968\168025O\bkxN]t\DC3\1033893\&0\1077777\187926nu\191076P\ETBwI\181841,\142181U\1000172\f\1051374\\\83251\DC4N!*MYv\DEL4W\EOT\SOHcC7\\\64633\1471lq!\DEL(D0o&`\NULd\US!\SYN\b[\994158'\DC4\FS\1072761\CAN@QxrI\48174\ENQG3\a\60178M\f\28315%\30673\1007909J\986036\"S\1087943\1089584|\US*\1109275\1076423\CAN\1006059eN\1106734r\145120\\\1104362-\EM\\L\FS\182161p\SYNP4~8H2mh_\99963\45342\1008075TF|fO\EOTz\CANWh \177810\143725wz(")), _newTeamIconKey = Just (unsafeRange ("\\4h'\GSoS\t\\Z(T\ETBu\DC2%\DELuCM\132961\1076800`2\996095\155615\ESCM\n#1\190872#ksGIY\983315\SYN\1003025!\7897BE\DC3j \146645\DC3\139674\SOH\ACKq\62563\DC3{\1079660,J n\33746&8\173855anTXB+<\ESC\NULXC%\\\167'@\1049588R\1014950\99496\DLE\r\DLE/\67292q\no\1014455\r\DC4s.\1055892\&3\61229e\44829 &Pn\FS\v&\161514yUf)T\DLE@\985904Ed\120879\24266\1060324K\16096|\1000369\1082954@\33048PiX>@\NULBp\SOHf`\175037*v\36250=\33663p\n\132727\SO\150462(\1060551\1015275v\1032497]\ft\FSk\119903\1072546\t\SOH\16318\f^%\GSO\DELw>_\vCT\DEL@on\f5V")), _newTeamMembers = Nothing}), bnuCurrency = Just BGN}
testObject_BindingNewTeamUser_user_4 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_4 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("G \SOH\a\1001237\50773\1093298\ENQP{\FS\153600\v\1091188i\1026156\NAKU@\161184\DC2_\NAKIW\185187Y\1066864X\RS\986706\DC1\20342\1010657\&5co9\DEL\n\1088805;q*7\b1\1091865\177350\8526\133791*\1015830\1093841\voC-\"\1030732\US@CE\1106549\ETB,\fun\994483X\139389a\178184\&45\1015171/\128529\CAN\SIhj\EMsB\1069669'Hp\ENQq\131159\v\51639\164753X6\CAN\\+\DEL\\h\34906E'5\1085044g\1100430^\STX~v\1023712\ETX,f\33505\&4\b# \EOTw4\ACKy\984731\&2\ESC!\144794\29690BO'\FS\176442D\DC2(Uvd\43946;'\1108409\EM\1091011.\EOT^9\119581\984930\44522V\FSYiD\EMa*\184746\1040540u\ETX\DEL\59825\r\1074294\EM2]\120424\r%\SUBC\t\1004595\1101070t1V\1030522\GS\994234b+Ru;N\NUL_~4\ETB\1032473O,\1026316\"a&\DC3\144367Z&\v\ETX\986394\170544\154644k\1043712\&4\139488^]H\997091\5631\&9E\1044322\77846\ENQ\SUBsN\SO*")), _newTeamIcon = (unsafeRange (" \vd=u\100257\1112528R\EOT)B{\1097406\SOH2\EOTc\SUB\60360\19058z\47377x\1033700\11327t4w4wO\DC3\RS-$`1? \aU\ACK\ay\n\1065364wZ\134306\175568\DLE\EM$\48429\989930p\1040718fDD \1055758ZA\1000723EK\DC1'\988393_9(\STX1yZ^\131895%\1040868\2138}\DELtI\120256\145196(tAIg\1019645\1087206cb\1089084\&5Y\172425\SOHf\173420\&6\143721\t \v\"I(./\28471{\ACK\ETX\tk\1055648\SUB\SUBGA\1110669\1038900\128257\1044509-<w\ENQFb![\EM\1058594,\1033588!P\1107047\EM^!\EOT P\1104356\47516\169856\35776;\ENQ\"j\ETX\DEL\1002172L\ETX=\EOT2e|\1071987\ETBse#\163327\1057817D9{<\1042374u\138203\165090\EM\1058289\1098942O\GS~s<\158523\174491\1011351\STX\10514\NULB\51391\ESCx\1075378\1093359D\SUB\1110088f\985103\CANd; B\100650\\\71216\SI\152034]\1011159\994198\GS\54304\&5\72348<CA8k")), _newTeamIconKey = Just (unsafeRange ("sUFPu\141757\1093767\NULL-p\1010033_ojzd`[\1265x\ETX?5r\SI\RS}D\ACK\vaQ\SOH]\a[\DC1\1076306$\ACKrD\1111417\SUB^\SOH^p\134789+l\b\1051933\986803\42512!\1063308\vC@|\163373y\161403\\\1030901\8668\27189][M\GS\SI\FS\ENQ\40184#3UAk\STX,\STX\SYN\1000902!k\SO;\DELo\1060531\1056329\1071643;4O\GSqN=")), _newTeamMembers = Nothing}), bnuCurrency = Just IQD}
testObject_BindingNewTeamUser_user_5 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_5 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("&ip\SUBj\DLEz\fCb3\25419\FSN\158764\32536\155411\43050\&8\988708G\DC2q0|D\1065867w\1095337\132710\72307%U[JT8a\1082208$BG@kz2\EMG1\CAN+\141400n^N]\1091464\EOTT\1025543\1082709\DC2\1109361x\1098088\140162t\12379n\1091623\&1\57489:\NULd-5\DC3:X\1030145O\DC4\CAN\149985d\ACK\FS\54963\1051432\v/X\NAK\GS\1033380\DC2U$!q\1011972[*\SYN\SI\tH\1042054\&4\FSdzBqj\17482&}b\1046699\US0y\66913\\GTU|`\tc5g\1083264WO\ACKO\1003708\t,\US(\CAN\RS\1010395y\CANNS^]n\STX\t\bH\ETXcU\95502\150594\97378\DC45a4\1085891p+i\95809ZCUC\1007671\ETBf>N} \DEL\DC2\22719\7092D+Y\ETX3\183408\&1MF[5?\ESC\98212\158009\1101412\52318$\1037908Do\183196.,l\181008.\1081252y\165018\1008850U~\DLE\aI\14331\66484\988195\ETB:")), _newTeamIcon = (unsafeRange ("4S6\GS\997941%Ac\1067236\SYN\989677EP'\1081547O\SUB0*p\b\134457\43294\&6]\f\29624&Z\1094855\ETXL_o\129054\1012211Y\179248\1066489\171241|\1015505p\49807U\70174\SO\DEL_{10\t\991002*\1056711*\184204\FSU\1101330@^\bp\47777\ETB\74519\986341\180765\1030943\EOTz\43946\17104\ESC\FS\9405\&6\f\t`\1043790PF\t\1094936p$\SOg\988961#<\DC3#a\1042896\"N\1039612*`-\NULa\6440:\v\ETBx(2\NUL\99914\SO\DLE\n\SUB\FS*\SUBkv`\DC2,p\184152\&7x\127119\&0+\1044632d\167472\EM{3\DC1.la\42454\ESC_\142260c\t\SOH\FS\1004611s\DC3*")), _newTeamIconKey = Just (unsafeRange ("a+$ ,\1108834DY\DC2[Z_\183924q\EMb\190933-R\1089072]ZTdl/\\\US_7\NUL(\47681g")), _newTeamMembers = Nothing}), bnuCurrency = Just CUP}
testObject_BindingNewTeamUser_user_6 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_6 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\1099630\\\59828\177368\SIG\ESC6\1103533\34484y\186225m \74013\FS1\1054178\1098124G[/\1111866\&6\ESC\171669\DC2(D\993214\1018649\157505n\20344\tz\16057\999052Z\996903uCh\1025420\CAN]\1050922y'N\8085QvQ\n\GS\ETB\FS!)\144720\\3J\74254fG\1001743\ENQ\ETBHDZ=mY\175181\163405\&0oT=Y)\ESCr\SI\18552kq6W\995977A\60535\ACK\FS\SUB\150066zz\ENQ\1031091\1099279")), _newTeamIcon = (unsafeRange ("\ENQ\CANT\158381\180058n&:\ESC\128312v~\1040822\NUL\160083sA\1097446$&\4111(\USy c\152818.\917545\1080804\1100450\v\DC1E\23982Cg\DEL\150390g!\ETX\NUL\1054902#'\1104475K\DEL(\1061832\1049936!r]G\SUB\1015757h%\1085025}d\1036257\&7pw\no\1016371X\175124<U\187095\171407\DC2\DC2>\1006658\1061400\r\1073470\1109840\ESC\SUBMbk\rnb\DC1O2cusk\NUL\152515\ESC\989550+.\SOHl]\1076476Yu\59887hHH\1052631&\f\1077227n\179424Vn\DC4\FS>\10737\1068316\164761\27286\NAKW3\1046366\137008\NULJ;\189943\&2\SI4\DC1X\1033893\STX4w\1111981}\aF\ACK!\CAN\EOTZ\30102\ETX\24806l\40504] nm\DC21\NULuI\1051336\DELe'\985168\1109308\999089\1064198\16042~P6,\147347;\ACKTRE\NULe\39676\1085470\GSY\177670\&9\r\148375sXCG\NAK\CANd\NAKWf\1003132.$\97073\ETBE~\95348\\O27\1014265W|\"\rW\GS\1088352\9169\f0\176106\1011600cs")), _newTeamIconKey = Just (unsafeRange ("\DEL_\2555/\1264(F\92345\EM\RS)y&\1093108w\RS,\SO /Z\EMXAe\1028653\aEm<\1028392\13980Z\DEL8\1109152*H\t\t\1050838\f<]\1036140\&7\FS@\ACKP\176922\150440\DC2eO\ETB\ESCD\157771\5692\46362ug,\t\ETB\ACKF9\RS\1018115\46125^\155863$\1113090\ACK!")), _newTeamMembers = Nothing}), bnuCurrency = Just TTD}
testObject_BindingNewTeamUser_user_7 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_7 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("J@@\1056286gs[\29099\t\69409\EM\993234\37903\&9[K'h\8984%\35520\156163\US\983430\29999=\STX(\1013120$c\99518\DC2V$P")), _newTeamIcon = (unsafeRange ("\"%\SYN\1020566\173590i\1112500\179106lTC")), _newTeamIconKey = Nothing, _newTeamMembers = Nothing}), bnuCurrency = Just RSD}
testObject_BindingNewTeamUser_user_8 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_8 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\20443a\DLEiU\NUL\1073720+\1089227=\f\996056{$\r\fX~o'\fk4z\126494[\1069130jn\RSX\10721DGe\STX\1004874*\59489\1068233l.X?'\158713\n+\151266\\\RSN\138882C\DC3\DC3\ENQ#\DC2\SUB\72325\tW\1069999\CANvmM\143320\1041084\SOH\STX\151937\1106269\CAN\1111708\&3P93W\135241$#\36029[\t}{K\1049624<@jI\1025779\&3\f\NAKm\1034121=L=0{\1064136{PG\55032O\1072979\EMx+#/2\10058xW\DC3\DLEUo\"5\a/\1025711\&3\99344B\78204\998043\1039987\992802\r\136834\&8@j;2\1026756\155496\&7f\DC1\22157/=\DC2S\ACKDv&z,=e\995043\&8\1073865\47794\&4b\ESC\1077387\1078863\993168qH\66292\139132\&3#\SOHO\1047649 u{\DC2\128226p?K3NLu\1086555u\ESCV{\SO~\DC2pHfN\19008z\97784]\991933\1000773Bx\121291.Ff2<=mt\ETB_\DC2yK\1104246v\STXG\1109799f\1069112")), _newTeamIcon = (unsafeRange ("W\1080751@\f\71079\r\NULS:GA_\1048191-\DC4DWvL\15422ovM\DC1i.Z~\42130[!x\144794<\1015008\182764\1056872\DLE\n\DC2-,l-\991238\64894\1054344\60807\"\59310\CANI~'\135320\&8\STX\1080220\148499\v\1037719Q\71047\1107564Z@i\21234Z\f\SOH,B<P-`4)13=,\SUBO|\16182=\n\1014897\987369\986693m\1022759lA\1085094a\157988\184958k\STX&`\SOV\tb$\187608\97351v\1014017]\EOTJ76\r5\1035255\1014716t\128620\SYNYE6~j\ENQ\136844H\1017663\&3[g\983722\167920J-,\ETB3\\h\ETB\1091756#\ETXOi \DC3\NUL5\183063\&9\SUBc)&\1053497\157680j^\"\ENQY\183259\50762#V6/O\CAN`\1026037>-b\119537|dx5JX")), _newTeamIconKey = Just (unsafeRange ("?\152658\STX*\170836o8>o[\\\134029Dm")), _newTeamMembers = Nothing}), bnuCurrency = Just BRL}
testObject_BindingNewTeamUser_user_9 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_9 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("4\129078o+:0\148018g?\100555\FS\vi\DEL\31419\ACK\SO8!\be\13566\158431\152146j\1099327\SUB$\\i_\EM\10663\&0\ACK\120622\16534/\US\18344\DEL@vX \1097016R\984800Q\STX\1106212q\ENQ\US\r\1110934\46262cPCXK\28407xu}j\1093951\ENQ\169119\20476&\1046520\&6\CAN\135111\1012919+S\GS^-V\v*jJ\DC1\ESC\t\EM\ETX\fWUb_\182700\&3\1091109\32842|\1110174\48721\ESCX7a\rkZ\129106A{\61420d\SYN\1026498E\SYN\993522P\CAN\DC4\EM?\1059140a\128453Gj\a[5\1088280FM\STX\189199\1081679/\DC2\1096050\ETX\19533\412\182492\t\ENQy_\50564?0")), _newTeamIcon = (unsafeRange ("\1079497kUY\152433")), _newTeamIconKey = Nothing, _newTeamMembers = Nothing}), bnuCurrency = Nothing}
testObject_BindingNewTeamUser_user_10 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_10 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\1098451r\f\1043215\DC4\GS\1069748\\q\120079\EM\1051099\1050680\51923\1070543\163745{kd\ETXk]\DLE\USX\128003y=%\1092838<\995901\38648\1044670 8\990053t\135108t\DELt\7554\ACK\FSN\NULb\9980;\1096930AVY0L\\tfJ%\DC3~0\SI\"p\ACKy\157913Y\26077\150664\1064613a\189271\NAK=%=\SYNJ@%]\153876\16652!tJ\b\DC1\v\52029\135294\DEL\994577\132543\96552\n\987893~L\nH\1090480j;\NUL9>\123141jD\"5\NULa-J\1014690_K3+HW\"\160362$WaN\184172\r\729\11010\ACKs\1031876\&7%V\1113329c\1109232\&7\r\1058401IKPa!q\163551J\SID\183592\\\rS\1112148%J\127001:\1013287\SI\STX\DLEP^=&\1002800-\1005151\&7\1068161g\bj\DC1bX+#\8124\EOT\1105035?\DEL{Z\1108389\STXVQ\NUL4&\23126l}\186693\&2[\STX\155898\987869\SOH~&")), _newTeamIcon = (unsafeRange ("w\67631_Y\SOH\ENQ\DEL\ETBX9\vbc\v\1012559\ACKd\\\94786p\DC1A\3913\vr?q\1054523M\153343H\SYN\17444q\t\158694:^@$\27675\GS?RV \1020966\SUB\f/c;\1085173\137409\&6\SOH\\U\DC4\r\153533\1060812\DLEI\1043526g\DC4\1100098\177562/M[\GS\1070072A\1105192{\998128`k'\1074825\1006302\&8\1048677\DC4\\@R>VBjY\1087583\178048\&9\154277\1025910`\SUBmo\1101031WB[i\1017462\1062702\169592\1082022\&4\162425y\187835\1031742s\1080247\40330\t\1018555\fy\169303!\15774\1018440f\1106542\\\160034\15729\1051765\1097242w<;|\SUB\SUB\121019\f?,\175516\SYN1\1047705\ACKt\179592F\CAN\121229\b")), _newTeamIconKey = Nothing, _newTeamMembers = Nothing}), bnuCurrency = Just USN}
testObject_BindingNewTeamUser_user_11 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_11 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\1097647%\DC4\DC2z\987378\135232\&7\DC4\EOTx\1010876\71870\1071299\1040829\120099\NAK\134887q\r{\162352\FSH\1075329[\ETX\NAK)C0\SO\100617\NAK\CANG\1084142~8\97438%\f\1110991\SYN\73101\DC4\NAK\178945\DC4\ETX\3861\ENQ2{x2H\158759l\SOHu\157424\STX\DC1qG\SYN\1005419ym\148469\CAN+\ENQG\1012666\DEL\aO\1084249r\178501\\\CANEdg`\43489\SUBV\1067293,3X}\1050762\&6.\DLEs14")), _newTeamIcon = (unsafeRange ("\t\51225V\127533g\1079622he(/\22387R\51495SP\SI2R\SOg\990464c3\1113054U\SOH\18575\138175M\DC1\53363?4F\DC2I\74376-\97850!'\DLEV)\SYN\US\21767}\ACK|Y}+PA\a\167393\ETXL=\aP\58454\917822\1089834b|lK\170158\998382\144396\ENQ\1006443\DEL\aC\126247\EM\\M\v\nZ\ESCl\EOTe\RSlk\SYN\151903K\1056210moX\44517Y;\SYN\ETBs(s\28604\63987\SI\EM\171029K\92725\GS\ENQ\146911X\NUL5N\DC2IS\SI+Yc\SO>Igb")), _newTeamIconKey = Just (unsafeRange ("\23558c\181596\1098451\STX\183168\FSX!Jh\995207\12436\1053388g\1036938\SI\EM(\1030693Y\n\19430\1036031\1049865^GJ\1024298eG\1040751V\US\aFC/A3Zu\168485\1050665\b\fYd\993039'\STX\DELv\1109571\166874+\DEL\STXC\100839/),q\SYNr}`7Po 9}\127174aa\DC25\a\26926\&5\a\1076159\153081}j}\99782\RSV(|3*a\EOTe\ETB\1050486\&3\175295|R\ACK\r\155433\RSsu\128425\1056363\128939\"\177972\DC3\1054980z\DC2\DC1D\169741t`\DC2\34102/v\160606j\1061289\ETX\SUB*\CAN0\1110672\152028\1006719\26790xfu\aV^\36632w\1013688/Ol\SUBk\DLE\1060476\GS[\74115kS\998711\1034353\FSo(\63184i\1104532\181839Fw\24793<\99490\DC1\1046566A\1077008cW\1010799\NUL\993931\983300ud\DLEv\173234GS.\f\10899\v\SI\FSz'D&We\SUB\ACK\3857<=<\ACKlHu+|\35157d{%Ko\ENQ\4958\1057881\t_\FS\STXF.\ETB=9")), _newTeamMembers = Nothing}), bnuCurrency = Just XAG}
testObject_BindingNewTeamUser_user_12 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_12 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("x=Vc\1056404V\\\25009q~NC\EOT\993707\SYN\SUB\ACKQ\23086\39930\&2dvW\ENQ@\ACK!ngK\153225P\US\53503\1063132{j\1013193*\1079380r5\bIYB6A\f\SOHA\1073312\CAN'E\ETB}i)J/\132175\nkdS?8w\189899|Reu[\STX49\1048591\43150_\t\3881/=\177596g^\DC1}\1051120qJ\1027286tL\985312\&9bgXb\DEL?\1072501\DC4\DC4yH\"\30678\168793\154966B\"/QMx=NH\ETXv\1111977VL\DC4g7jX\FS\SUB\RStn\DEL\ACK$P\ACKz\1098744\RSMNm\DC4\171829UeM\1063297M7=&\1009465\1069661s\44860. j\83207cl\879%\DEL4Fk\1058892 W^\EOTM&q")), _newTeamIcon = (unsafeRange ("S\b\ACK\f0n\177931T\SO\NAK]\\\"^/H^s\SI8P~&i:\21032\DC3~v\6482G\NAK\74778VL*\990244'\99473\n\ESC2X\ETBh\STX\163615\&0=@\1058396j|u{T\EM)DU\1076362;\141826S\1086727j\1028085\DEL$HtL\1036636\1103640\175883,]K\SUB\RS\SOH\NAK\SUB\DC1")), _newTeamIconKey = Just (unsafeRange ("\NAK;\SOH^udFy\988303\SI\ACK\141021\1096570\3734\DC1|\ACK\1014912\EOTle\ESC\61938?a_`=\41110A\6955\US\153330}M\SOH\DLE\NAK\EOT}\1012232\1094054y<@\153551\NUL??]!9\1109578\DC1\629'1(\178414\NAKl\ETXA$v'm|\172846\GSmY\145977\1037491B^\1006988\ENQeI\NAKz\t\NAK\152952F5{\f\163713=J\995686\165974v\NUL\DC1\14754b#X\148528\1063314t\ACK5\nO:\SOH>\STX\1057957\149780,j\bCqpzAm\f\1098250u\DC3?\145015-5\29428F\SI=P\34739\145687h\ESC\39463d&\179647^\DC3\139501\NULIj\1042004_VtD I&\1104951\STXa\70454gE\r6\74247\1034152ke")), _newTeamMembers = Nothing}), bnuCurrency = Just BMD}
testObject_BindingNewTeamUser_user_13 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_13 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\RS@\19150I%5\DC16|*3\a;\1107696O\162212\STXn\US\74890\SO\1104767x\ETB\65307\SOHZ\STX\1045104#\FSCLr|\DC4\FSE\ETX\162588l\GS.^S&\SOjTb\2138rvZ\1032735a\140340\&8\1050898\1024201d((\73900U!\1021712,\EOTn%>\1033629s{\nh\SOg,\SUB\SOGv\SYNb\119006\ESC\DC4\1045345{\131443\182881k\ACK\"\1049638~\13026=\188479g\41113\&1}\145225+Q\DC3f|s\1042140%r\132053\987849q(0\1112552\1101994\180274\1094615\USg\183309\57420\4451Lv\1075746V\42867\12762\rG\1037154/h\ETB\ENQ")), _newTeamIcon = (unsafeRange ("\NAK8<Z\120622\v4\61680\160792b\65608)3\STX\1063608\1020054\ACKpik\rd\1103309Cp\DLE\1050571M\NUL7\SUB\29030\GS\140015\172761\ESC\DC41>O\DC1")), _newTeamIconKey = Just (unsafeRange ("gu\129309w")), _newTeamMembers = Nothing}), bnuCurrency = Nothing}
testObject_BindingNewTeamUser_user_14 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_14 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("T^\1109445\1015437^*\CAN\DC2\b&t\RS\n&a\n\vH\1105825\ETX:\DC4\71225\1051523\&0T\FSlXk$n\n_\1041030\vj+\STX\ESC3\1041983s-\ESC\t\ESC\CAN>N\aZ\DEL z\984453\48526\996265=\ESC\125105:\1075159\1109305\DLE\STXd\US\DC2eS\ESC\187153\26431S(\RS5}\1107805b\DC2\151061@\ETX\1081883\&6D\STXt\f\985101\EMoMT\58584\173555s\ETBlo\DELMF\DLE\1017244\&8\"Y\1018440\180102\EM\70687\1038550q\1024384yD\170900Su\1106275UzEG_bh\a\1903\1030598\52043l\183048L\1057696g\US>\v\\\ACKN\to\25137G\STXq#P\DLE\137260]\1059288\DEL\DC1SEvf\1082748~>\17063\170410i~O\EOT\24946\r\163109Q\52751\152705\23143cd\1071240m\97478W\r$>? \b\127948\DC42+p\1110416]\GSb\1078219s-\188271\v\EOT_\DLE\RSl\EM\DEL\DLEX\FS\985828\"\1065939\118805%o@\EMM-\STX\1013322B")), _newTeamIcon = (unsafeRange ("R\DEL\1059275\185431\1079045p\1042072z5\NAKVd3\1084160\t\1111108\ACKV^HB\120990?co!\1079617 \ACK`Y\1068646\988876\&5\1078316\rYtn\43948\NUL\SUBD\DLE&\DC3\SUBU\DC3\94109\SUB\NUL\179241=T\n\DC3\446FE\146213rQ%2H41\143407U&1\DC1|\61856\RS\a$\133739\1000247\t}\a&(T/s&Y\1106030\nE\ENQN/\SOE\5521l(E#N\1012328[pH\DC4\194642aR\b\"\DC2P7V\f\STXvHd\1022255\DLE`LP\SOH\ETX\SUB\151719r\1082276E\1051664\33641l:\1017043%\69879\t\DC3\ACK@\188053\999170y0C-XZa8\1006863\&3\GS\7839>szi\ETB<\ETX\1103419\1059917\1087489u\65188\996122\&2\ETBg\n\64003+\EOT\SI\1104273}\NULNM@8\64802\DC2\1113408\&4;\66305#\bqN\1046210j\1086906gZpI`\174257^\\\DC2>:\NUL\1014736")), _newTeamIconKey = Just (unsafeRange (",z\997990o\US\n\27483\&8\DC4\vGiQ\996035JvWk&6Y\NAK\ETX6A\1087873\1012403*\ACK?\SYNcG\1024908\33220\SYN\EM0\t\30786\1099120\ENQK\t\1013487 \1005976;\1111268\"\173941'\EOT\DEL\58859\31280`=\r\RS\153448S\18194~\n\t\DC4r\140848\DC2\157646\35978\&97A\rI\1038035\36126{Y!=2\99021qF,\998619rS\66293a\1048371p4C!9=\154753P\DC3-\ACK\NAKD*\1061186$ie\ETX \SUB!ol~m.5\4052\71269\rB\1083474\10489\&3~*\150525^_\SOH\1067956']p\158045\141612\1029900\1014145\&2\GSB\DLE\1010225Lj\160836\1105156\44933h\991853l\SOl\STX.\DC4|C\b,-\64171\183744\34470\DC1\GS")), _newTeamMembers = Nothing}), bnuCurrency = Just GNF}
testObject_BindingNewTeamUser_user_15 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_15 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\1008773\ENQ\1043804N&(i+94b\1081224:\156068\1066385\STXl/+^Y\ACK\EOT*\1024891\1079164x\DC2\DC1\DC4\149605r{?r:3\985736\DLE5^,8g]\143921\1059588\&2?\83060[K\92937Pr\138184 o")), _newTeamIcon = (unsafeRange ("nF\DC4%\ACK\19565\&8\NAK0\24446V\187869@s\188217Wxm\DC4n\1055357\b\FS\1037895\1069930]\r\nR\95592\v!UE\32902\1110731\1109761\ENQ\188365,\999544=\SUB\182800PIG\t\DLE7\1060462[a\1080311\a'd\188040\1041672ua}Hr\996810\987422%Ol\990997%\v]\1104411\r\1019027Uxu0\DC4\CAN\162396GT\ESC\EOT8_aw\EOT/X\161694\v\fQ\STX\ETX\1071423\1086820\147385k\1033348AC\1109751%X\ETB-\46299T:-4QV\DC3I=8>#_\64492\26441A\21723\985556Q\USb&\STXUk\1095847\57385\ETB\NAKV\139061vu\45796\137039\166730\a=%f)j\FS\GSJ\1023583")), _newTeamIconKey = Just (unsafeRange ("\SUB\1060814\1082242\174560B^J!\vr\1031885B\991591b\78278\182868n\rgp\NUL\54615<%!\1072282C\ETX\1076158\SOHd\1038324\&8&m\b\182329\39749 V\1023658.\f\DC3'\SYN\r0M\1011882\SO\RSH\992335&5\132802 v\1051580\&2\49654\SO\1064886\r'\998086AcqH\aGDWh'#M\DC3\41686o-4\99489\163097xb\1024260\DC1%qNV!Bx8\1107408b/DD\DLE\21728\1108827\14208p\SYN\984606oDOAT\1029776\1013aS$\NULkG\EOT\150998\&3@vu<]\CANf\ETB\CAN\94443\1033134\1087701\1093475\191014\DC4\18402\n\"\\\DC4\SO\8710\US\1011495<\DLE\1034933#\SO\49723G^\ACKqO\ETB\twV\1064639S\1041369j}\1014754\1001368\ETB\1025278}@\NAK\1044982\SUB3W6\ENQ.;s-\1032432\1045292$\USO\FS#:\14463@'\DEL\b\1030724")), _newTeamMembers = Nothing}), bnuCurrency = Just INR}
testObject_BindingNewTeamUser_user_16 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_16 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("l\1076272M\60238\DC4A\165534P\1079225\176789\1096522ol\b\n-Y7!\EM+\133952\ENQ\US\NAK\169094\141829\EM\1016753\24980\v\1104324\&2\139352,s\163709\1019074\&3%iKkB\FS\994247P\31500\SO\DC2\\x[\1078019.\189638\1090551\1050017<}A1\DC2\149582t\DC4\GS\1013201\GS\1001104\173234\ESC\RS'E\1016773\ETXizo4\182008\64166w>pM\152092\1090679iE\63648H\SO\164450\DC1M\1038799a\STX-\1053135\f9 \992877h\134478\GS\1053983\97258 \176278g\v\a.^9%&l\162890\22661\STX\996675\6401\ETX\161639Z5\1007528Q6\1071422d\NUL\13737rxXQ\DC1]'\\\FSX\DC4(\GS\24910\151767\73939nKQZ(=\ACKNn\178624,\1003591.+\135347+N.\SYNS1OADF_\SO\GS\62782Rv\1028589A]e\7804\190675t$*k\NUL`h<{\140829\1098248\&6\NAKp,\CAN\48214/\1109250")), _newTeamIcon = (unsafeRange ("ymnq:!R\"Ss\EM]vN\DC2wCC\RS\21449\1080323\100479\1105539\&9]\FS+Q2\1013655\rc\t\ACK|+n/\t~\ETX6\ESC1^\STX\EM\1045435\1093439In.:\SO3\ETBX\54945D\999119p2\1101928\1087630Mh=Z+\ETBf4\1105229\&4\t#z\1018040\164905\NAK\1031049~zhx\GSdL\DEL\1110419\RS`SW\GS\185226\r\39268\GS\177227\1093145\32991Piy\DC4\b\DC3\DC3\DELe*sR\172510^{ToN\1091019\39398\&3T\ACK92\"]1W\137715\992990w\140642'k\SIO\20109\93991\DC2\1017473\n\59322>\1002248A<=Q\1005503\DC11k\29193\34240\SUB\DC43E\74306P\1103949b\NAKuba7\v-F")), _newTeamIconKey = Just (unsafeRange (";\9685\1023222X}\DC3\1027988Ieu \1002180j;\1086051/\bX\48839\bE7\1017829\168368s\NAK]vV\1071044\\\DC2As\134202$=B\97560E\148763,\GS \47123\b6\1008678tqu\nGw\v\44980\23526\189869M \83155\SUB\nR\997502o|\t2>\t\188807\FS\DC2<Ww=a\1060403]\16050\181199.)\998740v\47475n\1091991\145036\180332\ACK\68323k \30986\&18\SYN(\SOH0'g\29322w")), _newTeamMembers = Nothing}), bnuCurrency = Nothing}
testObject_BindingNewTeamUser_user_17 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_17 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("h\136186|T\t4^34\NAKB.l\996026#\180396FJ\1027045\SYN\1005965\\\n \1109129r!}K{\41803\ETB2JRJ7GG}'tG\".\GSf\61568\ETB\f\999959\NUL;/@q\\\1048573\1002092\1091329\v\ETBkzG\67867M\1031641\SYN|\EM6\b/\f\53767p\ENQ'/\1051511H\1016870yQk5MFa\1052135\&5C\25564\92524$E\1028173;\1106L\129658h\DELm\1022299z\1038179$\1067675N!k?>\rT")), _newTeamIcon = (unsafeRange ("\DC4\1026606{blM0Yj^\1042161\1022615(S\132418\SYN#P\n.\26411[C'\DC1%\n\USJm\1049767\ETBJ\NUL\47243\DC1")), _newTeamIconKey = Just (unsafeRange ("R^E\34329\ETXi\\f\1050041$(\NUL\1049930#}\1100114\1062416BdJ\1074030UOe.M\">\bZl\41493nC\38268\163572\r\CANH\SO\152534yv\168968e\NUL\1000421\1098383dR\NUL\332\ESC|\GSM\135008[\1054Vn\DC3\ETB\a\rN\1107963j(\163592\ENQH\aNr\1047454'Qr\"\t\DEL\1090174\990750\74980.\DC2QAf \EOT+\1071671kOYu#By=F\f%Y\167059\GS\NULW[8d>\SOHu\71349\119359 %\1009295#1:\bU\69694\997132:H\ETXFr\SO\141283a/\DLE\1070027\\Dp}\f}B\ETX\1047633\53566t\SOH\1093604\119552\111221\992609\25728\SYNxE\1018592e\FS\DC2;\RSdM\998284m%y\54456\USE\DEL\US\SI}p~\1062857o\1104531Bhr\GS`\\\SUB")), _newTeamMembers = Nothing}), bnuCurrency = Just SZL}
testObject_BindingNewTeamUser_user_18 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_18 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("`X\92413P\a\1054848\vc\EOT\DC2\139886M2r\ACKV\144106xA\SOHp\1072533p\1012039@Mi\CAN|\DLEp+lG\b\SYNA\146350\1010420kKx\1072502y9^\55050\1038556])\ETB&:\1026459Xo\1033560\38274\STXB\a\138036\1088984j+*b\STX\1061390\&9^YGk\STXtH N2 }?yOO6\5765+d\ACKu\DEL0Q\DC4<\GS\1018788C\ENQ:\DEL@\ETX\178703\SO\1062367\ACK\1005202\142757A\1038120\&06n\1079890\DLE+^?OMR\EM7mmjl\SOczj6'C\1092903\GSB\b0u/\1085441\bc!eY{L\NUL\FS9\GS>vV0 \1089265Z\155264\1077097\&5s\1057031T&\US\SOH\1073591\SUBGJ\ETXm)\NUL0}")), _newTeamIcon = (unsafeRange ("L\1023952\ETB\137225h\132683\DC4M\144860l\34468y.\SUBW\DLE\tNU\b1\1071161\b\1031801\1070270\DC1\NULH\DC3\189084C3teJ\1018475Pr]\ACK\54712C\78407\94291XjY\168066Z|\ESC<\1026236\1094387\43663MZ\b\rI\t5~\CANW\STX2{\129477\1050514\rMZ\1024841K\64217TDal~\190119e\ENQ@H\CAN9\SUB\176322\46390\41237EpO#\DC22\1077521Cs6\EOT0\53368r\NAKRD$\999787XF\9984X\CANZ&\bU\1108831\ENQ{to\983879U0\49396R]jB\FSsv\1113070q\42939)B('\1102894-bK\\\983633\a#eXP\10378\"7u\"\NAK/\997877\63485\140160W\SO\1069882b.)\1028227V\DC1")), _newTeamIconKey = Just (unsafeRange ("\1079371%\FS**\tC\v\RS_\51563W\1053219<wO\STXfYr\35778\1074672-\SUB\67865\1030289\1074602\EM4 \b?$D\1094671xWv;U\38634\150616\987404&\152132&\167807mC!\123606\DC1:{L\72983\1008912`\1073137\US\1000728\37587~\120462y\DEL]\5904X6T{?6p\DC3\1034524E\SIl\63877U8\175024\SUB\1008563\1077973\bo\DC2")), _newTeamMembers = Nothing}), bnuCurrency = Nothing}
testObject_BindingNewTeamUser_user_19 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_19 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\DC2;\1021515\186187\SO\GS\990260\23126\ACK5]\v\101040\1096103|\177565Y-\1055889:t\GS`2\ETB?\1109496?[\1037666\n \8457\DC3\1078868\990353D\SIh3D\DC3\v5m\USI\50232\1050272\993544l\172513\1004869z\DLEt\"7*p\EOTpB\1027235\1047301vbz|\169900\bD\1113342\nBl\EM\21813W\96754{%\6934otj\SOXE\aINo\149497|E.~\100916\1068053a\1095908\&9N\15517;:\986361T\DC4mlD=Q\147770Vd\997172\1109914\&8[gA\a\1009796l\1083628GI:\SYNc\1031935\CAN\1090987\EOT2\131222,r2uY+df\1003165\&4\n\nN#\12415\17494\EOTT\SO\ACKEU\DLEp-\54039al\ACK\f\29658R\DC4\164187\152945X\vo\50893\1092694\1039200\48516\FS\1079126\1039424E\\\f5\b\66505N\"_e\165340O|\1017662\172489\188038\1022353\983902@|>k7J4\DC4+\RS\EOTId\1099700\ENQ\1014939\989996Y\1103515\ENQx\92880\1049463\&0#7$dl\99495Zta^4I")), _newTeamIcon = (unsafeRange ("\r\SO\GSlX\65516$m*\1010401\1102887)\1009161\&9..\43337\58517\1093901\ETX\"2Z\v\DLE@x9\NAK'G\US\ETX1P7\DLE1N\"e[w\172117b;!kg\EOT~\NUL]Nx\191317\&7}\1070441\71173\1056498\ETBPDaqmi\118825\v\ETX.\1050167\1079724\47756\1002955\1049077\&7/\a\38808\&2\1000014\1028373\SYN\171389\1104631e\179139\167483qV\19739= \179544(~\vR !N\92894U)pG\EMI\f+\SOHq\34780\&2|g\1037107\SUB\1062308tw\ETB\1004420F\DEL\95938my\20816\1039458\DC2vc2F\1011834H\166125\1013966N3WSP^\ACKB\158303\1067358$\1045315\1004189\ENQEV\43691Hhkz\EOT\1071723\USmj\DC1\29212Am&b\94517#o\1106534t\1031055\t\SOH\5876\1043828\133993m\1014844\&1[0So&\128822\&5\121403LrX\1085163\ETX")), _newTeamIconKey = Nothing, _newTeamMembers = Nothing}), bnuCurrency = Just LYD}
testObject_BindingNewTeamUser_user_20 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_20 = BindingNewTeamUser {bnuTeam = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("-v\1097595]d\1076956&=\DEL\134131\SYN\1088982\986334\EMX\989644\1102682\&5\SO\\u\7829\78806 \142783\DC2\976Jc+E\179641OmkT\1027807+q\134099\1015669\ENQ\1057084\ENQK\1038927\f\f_4iX:<#cQ-)\FS")), _newTeamIcon = (unsafeRange ("\RSc\134865*,c\1079867LV\140086h\1080274\182090\1109287rpx\1076627LL\1026086q\nd1\19335y\1023267b\1070518va;(\136786wL\"\161944'02\70507\164817B_l\1018756zFr\SO\1082632\65335\&0\DC4\FSD+qv\1023025\163083\51540R&inE\DLEyS\DC2>Q\1029105O\995745\1066585Ca\ETXF=+2,\ACKWo\161164\SO)G>W/\tqDR\NUL\nY8'\94486W];a\1038665$\1045816 (\1032877\1028008\SYN\133795gd\1002568\EMW\1017229x\ENQ\\\SYN\1026458:\1088960\b b\188928\EME\SOJe\1082673 k\ENQ\7953|@\1031891M2\a|(x5u8e'oF[WN\136091_z|\vLH\SYN\DC3\1090428\EM\DLE\n\158492\SIMx\78611C\NAKX!dM\NAKDc\ETXYfV\1028757\SI\987438\65299\140573\1085046\SIS2\ESC\b|4\1079708\176405\1024570\FS~h\149521NN2\DC2")), _newTeamIconKey = Nothing, _newTeamMembers = Nothing}), bnuCurrency = Just XAF}
