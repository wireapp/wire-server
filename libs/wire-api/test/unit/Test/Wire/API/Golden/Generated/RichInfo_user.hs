{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RichInfo_user where

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
testObject_RichInfo_user_1 :: RichInfo
testObject_RichInfo_user_1 = RichInfo {unRichInfo = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "^SxE\1107571\t\NAK\CANG\f\166048+;\987494\194627", richFieldValue = "\ACK}Y\1033181ab5F:xE3\ETX\61547\138371\98394?\34751ET\ENQ\ESC_/g\1017531"},RichField {richFieldType = "S+c\DLEM;U^}?c7\DC3p86H\aT}", richFieldValue = "b\1064928l\1025636(~\1083900\1034018~1Fse(\140775\&6Gjf\1069516B\1037093[Ty"},RichField {richFieldType = "B\157328\NUL\1045867QZ\ACK\70305\GSWee\US\EMB\DC4\b9X", richFieldValue = "\DC1ur\111055q\DLE%"},RichField {richFieldType = "N x^o0-\1078660\STX<^M\\\63345Lc\f\1044154L{\1037391ge\vk\1065482O\US\152035", richFieldValue = "\CANX\29241\DEL\vM\1064767Z\t\162409\CAN\1065321\9015\SIXK"},RichField {richFieldType = "\1069194\a\SOH\988850m5\135937'<#E\1087468\n%g*lA\33980\1024078.]}\DLE\147080\1025865", richFieldValue = "\987217~ex\63440%6\EM_\1057962\&6"},RichField {richFieldType = "O", richFieldValue = "k\100203\182152\&2c4\997066a+h|"},RichField {richFieldType = "t;\ETX5/\GS\\8", richFieldValue = "O^S7Ok-CL\"0Yh$Z\DELwd\vLRr!"},RichField {richFieldType = "5\SYN\1020543}\1097586\1020045_H/{\1007575N\183575q\DEL\1048147\EM\EOTs\t)j\ESCSx\1010954\1002536\&1", richFieldValue = "c\1077388|f\EOT\f4\ESCVt\nJ\SOH/MpVq\998518\EM]\21304\SOHwT"},RichField {richFieldType = "\SI\1075429\1008487;\11657\ACK\1052918xk", richFieldValue = "\1094961d\DC3\SO\132879"},RichField {richFieldType = "}nu\SI:|\a)`X", richFieldValue = ".\DC1\ESC\37826\99398\182910j\GS"},RichField {richFieldType = "hF", richFieldValue = "m\USLu\SI\b\FS\f\f?_\f?\422\r\STXk.S\RS"},RichField {richFieldType = "EX\1032773i#", richFieldValue = "\19702]r#"},RichField {richFieldType = "- w/\tT\121118\68485\144236\EOT\1028830!\1106205\210ue", richFieldValue = "\SUB"},RichField {richFieldType = "`&-e\61618!.\19027\&4Qh/6\DC4O\n%1Q)\DLE8$\FS\FS\SYN@", richFieldValue = "2\SUBV:- .$U\DC3_\ETB,"},RichField {richFieldType = "\183108^\1112617\65379x\EM", richFieldValue = "i *{\1102885[\49390\1072595P-\1075700\1096432\SOH\ETX\45502\DC2Z\1077309\1038520r\1102669"},RichField {richFieldType = "<\SYNq\153397\58856Oc\1006356^_GbyBy4ba\34563\181038\988230>", richFieldValue = "%GH.@j\US\\qAeZ\"\FS2w]CQL"},RichField {richFieldType = "\ENQ\US\1076848'|Q\r{\985568\1090962\DC4\146815\1059634\1005073\37631\"\1025224J`H'\ETX%", richFieldValue = "W\4357JM\22805>\SOSvR| l8\f>\SO8\ENQKrp"},RichField {richFieldType = "\147899\ENQ\DC3\US`\1080480\GSF*\GS~\SIVQg", richFieldValue = "yeb\44908G\997093hHe^d@\SYNukW\186689\b\ACK\993097o\35209\DC3"},RichField {richFieldType = "\a8o-", richFieldValue = "z\DEL_\1106484\1045348z'\1000862"},RichField {richFieldType = "\SUB\a%\nz\1049823sz\t\33737\156147H2EK\6638z{;\DC2\NAK\DLEV\135961b1\SO\ENQn", richFieldValue = "R(O\GS\188620;qm\158973\ETXD0J\95763\a{%"},RichField {richFieldType = "Z\137939\&8k30b\NUL\NUL'\NUL\EOTS\EOTa", richFieldValue = "\b\1110089cK?A\af&\1031035\f\1030053\&5\1060576\170444(\100102\a\SIS*c"},RichField {richFieldType = "\177965gyK<\f\1021830p\1045109C\EM\SO\1110886Z\28042\1072400\158943uz\CAN\1050890G9", richFieldValue = "|bQZ7\a\132427=+[+c\STX{\DC1R`3\t\DC23v\DC3\184203\1066860"}]}}
testObject_RichInfo_user_2 :: RichInfo
testObject_RichInfo_user_2 = RichInfo {unRichInfo = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "s\GS`|v\1009168K\f\tY\1051755Kc\57515,s\127399>\1090470&<S=\a", richFieldValue = "\1113068~\55152\164378"},RichField {richFieldType = "9", richFieldValue = "F\984896\135050u:Q\SOh\1053636,\26612`\RSfD\ACKl\21749\DEL*m5"},RichField {richFieldType = "zFF`\t\26001\SOHw\1049857i\995167\DC1", richFieldValue = "\44735\ACK\1076338"},RichField {richFieldType = "\1044985lS7+1e\v\45293r", richFieldValue = ")Th\1031107\57822\194777\"\1015974@df6\38848i#V6\DLE\10076,(\1098443vY\1068643\ETB\998844"},RichField {richFieldType = "\1024826\1058359%ds3\1040741k\SUB^Hd\143832QD\EOT\fu)TV\n", richFieldValue = "\STXA6q\SYN\ENQO\STX+D\1103217\1023517\DC2\1016431\DC3fG\ESC\140668ap\CAN "}]}}
testObject_RichInfo_user_3 :: RichInfo
testObject_RichInfo_user_3 = RichInfo {unRichInfo = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "%_\1043783@atVg\1092965\1068397-r/\SOH\a\frobL5 H\NAK\1073772\143032\ESC", richFieldValue = "NJ\1011539Q\44495g\148542W0\1073262\1083496o"},RichField {richFieldType = "\RSD\62328o\1078557>=4\1002973b\EOTNC\a\t\SYNF", richFieldValue = "\1002662_\111157\RS\NUL\ACK\SUB]bIn\181214f\73916\FSD\ENQ4\ACKgv:UhBP\138658Z\169690\SUB"},RichField {richFieldType = "\\\nX\1062797B\"{\tz+", richFieldValue = "A\22358\&5t\DC3\1067234Y\FS8\DC1Q\18025[\nG>FD\3337\38694Vs\SUB\53788\45690\ENQs1gv"},RichField {richFieldType = "y\DC1\190099h\996766\1058895(S\ESCL2\1043799\41381m{\6179|o<GAh9\156135G", richFieldValue = "\29246\1032403J(]r\RS\GS~\SUB\1098533&"},RichField {richFieldType = "N\36496b\NAKWa6\1111215Kw\US\STX\1006731~o\179850i9\25521\15723;KX\v\1036895", richFieldValue = "s\EM\ESCPg?\DELe"},RichField {richFieldType = "@7K\1089429\&3d", richFieldValue = "\1111732\98662\fX\1083493\1083249\FS\128698 \b8(\1112153\989669 \1765\DC1\1063531\55050E\1072823\994092e\SYN\1024667\18388\NUL"},RichField {richFieldType = "\SUB\ACK\187916\t'\121435\\ze\EOT\n", richFieldValue = "S\GS\1043622U\50992\DC2\USC\1111277s\ETXN }$\30538/\a"},RichField {richFieldType = "\DC2\3073\b\1099335)dcAp.d\154253CY\a", richFieldValue = "sCK\98980?, \167458H[6u\bw\133037"},RichField {richFieldType = "X", richFieldValue = "\997676\FS\63841\&7!\DEL\1076683"},RichField {richFieldType = "lY9&\7403\158067FiZ<O\1019911\DC3r\188397\SI\1108533/\SOH,!9\1045459,'9!_@\DC3", richFieldValue = "s\GS4\US\1063071\99854X\1029814\32318"},RichField {richFieldType = "\166069}\DC4U\ENQd\146352\1077312R\1100091\DC1\1032111.\94466/\b5z\STX'", richFieldValue = "(M!\SOH\\\fu\1008546=/\SI7lJ~"},RichField {richFieldType = "\1087433\DC4", richFieldValue = "Rm\1071844\&9\EOT>,%JeO\SOH=i!\1035164\144692"},RichField {richFieldType = "o\EM1\1078355\vM^U\SO\SOH\195055\v\1067192\1096597O*)nb\\;\tO\ETBS\n!7D", richFieldValue = "\57417\&5\1035851j\ACKK"},RichField {richFieldType = "\NUL]\43845\1061345gGY\189729xG8,V\NULMp\1057372\&0\50621\nMU", richFieldValue = "$oBk<M\27484@zcG\SO]\1105357t,\US\ETX\1091633Z~i\ENQ"},RichField {richFieldType = "s\CAN#", richFieldValue = "5\"Mm\18905T"},RichField {richFieldType = "Z\ENQq\1094081_\1075425\&5]\GSwR\5547", richFieldValue = "$,\1060928\US\SOj0 \183155cGuF9)C2\1110608\FSp\185916,j\ETB7"}]}}
testObject_RichInfo_user_4 :: RichInfo
testObject_RichInfo_user_4 = RichInfo {unRichInfo = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "\1024958i\NUL\36745h\FS\1042199\&5\EM\NUL\37709\fz\1043628\SI:,'/\ETX7B\DC2,\ACKl@", richFieldValue = "k%\SOKh=\1023025Q\f\169381F\t\1039172DD&|\DLE\993224=l\57924\1075997e"},RichField {richFieldType = "nU", richFieldValue = "\39860\187932\94025HY\1112735Y:N\985912\998441%xq\141919}\fl\488\1067488w\1112753\152077S\1019043\1043383\164630\&8\SUB\1015987"},RichField {richFieldType = "\131911CNSD2\"P\120097\176668\1001791\f1\ESCU", richFieldValue = "/B;"},RichField {richFieldType = "Y\r3\1085484[\1100930\1031869\47659\1051046\1005455\RS", richFieldValue = "l\DEL\1060430\&21\1060585QGc\1016935\""},RichField {richFieldType = "M\f\DLE\EM\DEL(.Q2X\21735,m\NUL0", richFieldValue = "Y$\1102469\SO"},RichField {richFieldType = "", richFieldValue = "B\SO^\137843J\1089928B\CAN\43922o\1050889\EM@\v\54733D\134079`\DC3J.kYuR\NUL\EM\\\1082463*"},RichField {richFieldType = "\17214+\1005156X'O\1023437\1102994\&8OdS", richFieldValue = "3\CANx,C\1029544\t\1074735\US%"},RichField {richFieldType = "7\127235\60422h\1014658\1009375c\132784\RSI\ACK\1105996\NUL>E\78744d\ESC\ahV\b\61121", richFieldValue = "\1092606\36956\988476-d\v\1097826+\1064489{y"},RichField {richFieldType = "i\27863", richFieldValue = "zJ\NULa/\1038229~\STX5\1029700\ETX\NUL\1069874\1020888\tv8\NUL\170638i"},RichField {richFieldType = "sp\SUB&xk.\DEL\1089381&\ACK\38544\1031854", richFieldValue = "\157081#\CAN\39851\"Q\1054547h8\74182W\DEL\1056016\118832"},RichField {richFieldType = "\1027613\EM\DC49nj`K\159493V{8\143760Dp/\98311f\83093\ACK(", richFieldValue = "H\a$:\138614>\RS\"\3507lLB\1092115\&5bN\CAN\SYN&_\148022\1009833\SYN\182904\SO\GS\NAK\STXK\159076"},RichField {richFieldType = "\135169ll)\83148V\175699g!\\WD\b\177764\177438ZS\1062978^W", richFieldValue = "0Q>1[\96186EbyLx\98190\25673A \DELD\9704\1064699Y"},RichField {richFieldType = "\a+\28468xE*3\1085271\991699\FSe\173464BC\EMm\148563'\FS\23583G\1059712", richFieldValue = "\40191uP\129300JI"},RichField {richFieldType = "v]K\34035\1044036_\16177Y\1060330\ENQ\FSQ\EOT\27395\EM\1039317q\SOH\77850'\EM\78432]\1077923\SYN\ESC\r\CAN\DLE", richFieldValue = "'\94355\194670W\SYN\\\26663\47092"},RichField {richFieldType = "$\18918\ENQjCz1\ESC\f\1088800F\27664;]\"", richFieldValue = "r\DC1"},RichField {richFieldType = "I\1006093;\162913\1048268\142299,()\DC2&", richFieldValue = "+\n\49540\1088526\1075987,a"},RichField {richFieldType = "\59754\&3\1111166_)RHe0%\13927\31328;\SOX\1035428w\DLE/8]\18495G\ACKa!8\174618", richFieldValue = "\NUL\DC4z\998855\1008492\">{Eiqxp\DLE\n*q$m\DC1F"},RichField {richFieldType = "\151070\v\US$#\b(\DC3[}k\1100450", richFieldValue = "\178066K$\vi\FSo\151242*J\158975\54178F\100260S\DLE\"\DEL9\na}\42637\50106"}]}}
testObject_RichInfo_user_5 :: RichInfo
testObject_RichInfo_user_5 = RichInfo {unRichInfo = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "\a2\30386\62010};<\142445gu\NAK\157603\SO", richFieldValue = ";x\"<\60769Rq\985127\b~=G\USEt\ESC\DC4\151403 \DC3\DLE\1007437\&5?\ACK\1002686M\DLE>"},RichField {richFieldType = "L\1067653\1040320\147700Z\RS\tO*\996488", richFieldValue = "Us\120832\132551BR 7\t\DC2\97935o^\14086\SYND\f"},RichField {richFieldType = "\CAN &T!\1110810v\EOT\1019274b", richFieldValue = "\EMf]\1010312fCvP\GSJz+\1061414\v\1065599<\1086983\1100212"},RichField {richFieldType = "\ENQ\611\b\CAN{\194655\f\STXW\\\1002399(\CANJ\DEL\1028589.\99684.l$.|3\46761N@", richFieldValue = "(\FSs,d'T5\24184/\992404\SOH"},RichField {richFieldType = "\ETX\1084694\19203M\CANGm4lK\1058170Y\1040206YiZ2sw6j\133532m\170543\1071988.\1031825\DC4D", richFieldValue = "2\1093105D+:\CAN\SYN*7\bi\166233\ETB\ETBHU\b)u\44513"},RichField {richFieldType = "\1097679O!\ENQ0\SUB\121366\US\985090KRxY\EM\SOH\DEL\EM\1096469\SIN\RS.2=\66712\1030808\DC4", richFieldValue = "|\17585\1052907\998634\173962[rh~ric\154456\29643y\NUL\SYN\989694[o\1069517cw\48970\990760\&6"},RichField {richFieldType = "", richFieldValue = "\1037198B.^Dn1K\4452\1071364\1020383\&3\SIc"},RichField {richFieldType = "g\993346i\1028162 ", richFieldValue = "aS\"\119132\1054345\NULNiUP\EOTy-O-7QT\49989\1010039t\STX\NAK2"},RichField {richFieldType = "\1014598\&1eB.h?\1034588` $N_].J\127284\DC2:`\1084258F|6", richFieldValue = "\DC4\53567i$\a\bl\33429}*?\66322\EM\24791\ETX@`W)\ENQ\DC2_\b1\ETXptI\1090992@"},RichField {richFieldType = "jgH", richFieldValue = "\a\1046212\&6w\ACKBiM\DLES\46610_y2\53100\ENQ\ENQ\27278\FS\61828s\94468\&1[~O\ESC%*\RS"}]}}
