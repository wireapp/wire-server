{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewClient_user where

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
testObject_NewClient_user_1 :: NewClient
testObject_NewClient_user_1 = NewClient {newClientPrekeys = [], newClientLastKey = (lastPrekey ("")), newClientType = PermanentClientType, newClientLabel = Just "h\rX", newClientClass = Just TabletClient, newClientCookie = Just (CookieLabel {cookieLabelText = "\47236 i"}), newClientPassword = Just (PlainTextPassword "\SI\166006z\1003756b_\ENQ<I\ETB\DC2\54628h\195056\917778\bXS//7C\1060040\141120/\1049961\23055I[\SUBIo1\52457\DC1;\1062580>ux7M\b\EOT\36306UB\r\SYN?~\1038704\&6\148405%\SI\150981\DLE\FSw\1002823M5d8\ACK\1108861%\FS\1035368F\40244\EM\30668d\1079791\65605\a\9456\ETXv\GS9%\ETB<C\GS\156570O8~G\a.x^\991855\1020607T(\ETX\174810\1081425o\NAKu\42784`\ETX\DC41\1070019\129057FH~s`\1054024?\1074942\1049588U\nl\160963ACb\DC1\1033964\1071385\8211\223]\RSc(\EM?\52424\143548\1010914\1078486\161766\41851:*\996039m\94052D\ACK`\152960\ETXBXe\154268I\1091377\1035892i\12012&\149391:GM\SUBR\1049761b\EM\DC2\1097373\USM\CAN\168708aJn\RS|Z8|\176314lK\94266j\n\1003765j\1005794(q#GJU\FS\DC4Byo=\996131F\STX[\1082117Z\DLE\DLE!ChU\1043054\DC1\1086345\ETXG\50229\1042580zU?\SOHD`d,\vG\1068514\DC4\31192\&8)\1008249=NNY\DC4'ya\1001207)+n\n\1078334^\172295t\SO\US\171179}x\DLE\RS\EOT\SOH.\143625Y\19849[\t\DELJ0Y\STX\"q\nJ8D]|$\1112658O\US{QQ$i!N(vu\135052X#\52848\39719\95589J\1062662@\SUB\1104204y\t\53123\USZ\1045263_\99002Y\DC2\993046\172405\DC32\129598t\37844+\1099055:\132818\SYN\STXI\DC22]U\1009657-\63899cT\1016901\1087179\129186\GS\47131Y\134220Hm2:\1040856\1018838NE\984371\nnl\985692\1039951\ACK\1108198\US\142742c\vTh\988714\28792\1010954\1107083\ETB+\"\ESC,1w\GSH\138436Z\1027065\2272-\16646\US7=\1104190C}j\EM#\ENQ~\DC2g\989434\nO.\ETB@d%\1024714/%\1077733\1045590'fnf#\7985ZQ\138109B\SYN\1042388?0x\CAN\42043B\160298\1107877?\1104508g*w:#\141277}\ESC\ETBI+\SOXf@;\1003502\NAKP\993802\94981i\SUB\f4\26221s\RS\994660\n\1109865\"\1057583\EOTx.w\19963\1046386\&5\67667\1037098\&3B\1022294a;g\988095\ETB\ETB\SUB\132318\120024C)\ENQ\177963f}\1057341{Kl$.fbnr\1048230ol\fD$:<brj*1\996764+CD\166886\54405\10867\1002064\141541R+$,Dz\DC4S\992777E\27343a0\1026240\NAK\45956\1068856\992146;Tml\988211\1059269\GS&G*\1077206\SO\SYN\\3\ENQv\DLE\1095896]p\164556\990025Z\ETX/V\1096889z\142214\984153\&09Z+j\STX5su\DLEhA;&\ETX\168377\988208\1059562z\1110847\1111647\147562,\1280\DC1\DC4\41618N\v*\164401Zp\SOH\ENQ(\1040356|\24318%(|tHU2\1052898\NAK\NAKW[\r]u.\1027586\148050\fo\NUL&\ESCqt\1004774\ETB\165987\USii\SOIYZ\SYNR\1074461\RSu\149480\49148rSO\DC37*\98848\1105368+s1\ENQ3?=Ci\STX\27146EcBg\138744lT\DC3\SUB\121272\153763Pb\159329u\nX\v\v\b\DC3\DC4y\1077258\RS69\"{\52823\SOHvZBCB\a\n:\1004353a\SOH<\ENQO\1017083\10583\1099134Pp(\6914\29593\97519<vY\1041106hc\DLE\1081983Y\183349R\145025\ACK<\1099325Lm\ENQ]'k\50580\1087949\DC2\1090044[\RS\ENQ\1048535\&5 &\CAN.\74862|\1089601q\US\1038142{l%a(0\1028465\GSmL\1029684/\1015226\a\GS\996522\24741r&#2#X^\42816\1055523Q\152346L\NUL\1027581rc\ETB@NJp\SI\171232F\1107610\1061198\996251@%\DEL\1019132\1081891Mh\EM\t,v(8)X\SIk\1335@g\SYN\1076962@\FSq:v2W\988681-r\183997__-;\141176gR'r\999350I+J\1087334L\1050547"), newClientModel = Just "B"}
testObject_NewClient_user_2 :: NewClient
testObject_NewClient_user_2 = NewClient {newClientPrekeys = [], newClientLastKey = (lastPrekey ("+")), newClientType = LegalHoldClientType, newClientLabel = Nothing, newClientClass = Just DesktopClient, newClientCookie = Just (CookieLabel {cookieLabelText = ""}), newClientPassword = Just (PlainTextPassword "\1099534&U\STXX\ETXGV\GSh\1061650\175851(W\ENQ;\1050693\1060644C|}\1005478\DELl[\t \1044682\1083144\STX\48289e \168680\1102401i\189918\va\24073\59626nWu(E\NUL5 \ETX\DC4\1100143z\NAK]p\"`\30592)sZ>t_\49990\&6.!j\CAN\153759h\1096231\EOT\66327JoWc\1044675?D\51073@V\SOH(Ig\1006480'\ETBE\1027361\f\140995\NUL\1055388\132612h*s:\161520\144469.\v\f\143742\1044995\917932i^\NAKuZc\995140\1028815\DC4\1046814D!\1007987JI\r\32954#>\a\SUBrk\SOH|DH\GS?\1100084w\DELfSV\1003259;D{B&\132617\DELf\1066848\98458)U\1110639\26281vd\ACKJ\EOT$u\34075q ^F\1013130\&1\176174C\1030672mC\168931A\a};)M\ETB^\GSS\DC2\19052\SOH\SYN\169500?+B\DC14\19949\1073836\SIY\73120\"`\998745\&3\SI\1022588h`\191122\CAN\985922\1094260Q \141813lD#\SOH0~X#*Jxg+\EOTX\1096835U\1065433SF\ETBX\1016415\v*XJ\STX}xK.E(K1\DC2\14917#bX\1013770\&5\ACK\64130P$\SI\101024\DC3\14959>(t\DEL{g\999056\37668[_\tM,3\DLE\DC2?d\SI\FS\1066593\"1\DC2f\990266\1033757\1099561\SI\SUBn{b?\156338\EOTx5i\1093625\t\6021\&2\1032778<=\v-~\EM\ETBeC\59385*0\\\98263\SOHL\62666\rR5\1085784\1087750i\FS\1055468\EM8M=\1044238 \1076925i\1062015\DC3\ETX\"Ckx*6AF\ACK\1221i\1088701\1036585\142643\&0N\5144@U\1026338o@\DC3W?3\a\1083486\15175V\47855\ENQ?_aUx[BD4\SUB,\46501\1024926\178748gI\DEL\DLE\1015364f\1110692\159643)\ETB\134636\1103995D\1108953p\1003623o\991419\2000y\EM:\1018333\&0_&\1069982\1051717\1024988\vIkt\DC4\ESC\71875\SOy\50760Aw|\r1=h0\146255r?q\16383\ETBo\CANma\ETX\SUB\1093817V\DC4}\1080566\176579\DC2D@\166020rfpV\159783gs})([z|\USn\DELV\1066027_8?R}O\53505rRi\1055733\ENQ|\SO\\\983318\RS=\16005\1005775%\CAN\164073\&3ft\50654\\*\NAKh\t\v\1063750fz*\985865\DC4U*\168424&Cp\ETX\NUL/9\SI\178625\a\SUB@S\15558\173416Yf+\1053089_\1066981,v\1007041c"), newClientModel = Just "\ENQ"}
testObject_NewClient_user_3 :: NewClient
testObject_NewClient_user_3 = NewClient {newClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newClientLastKey = (lastPrekey ("i")), newClientType = TemporaryClientType, newClientLabel = Just "\995972;X", newClientClass = Just PhoneClient, newClientCookie = Just (CookieLabel {cookieLabelText = "\t%"}), newClientPassword = Nothing, newClientModel = Just ";P\181563"}
testObject_NewClient_user_4 :: NewClient
testObject_NewClient_user_4 = NewClient {newClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newClientLastKey = (lastPrekey ("")), newClientType = TemporaryClientType, newClientLabel = Nothing, newClientClass = Nothing, newClientCookie = Nothing, newClientPassword = Nothing, newClientModel = Just ""}
testObject_NewClient_user_5 :: NewClient
testObject_NewClient_user_5 = NewClient {newClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newClientLastKey = (lastPrekey ("*z")), newClientType = PermanentClientType, newClientLabel = Nothing, newClientClass = Just DesktopClient, newClientCookie = Just (CookieLabel {cookieLabelText = ""}), newClientPassword = Just (PlainTextPassword "8f\177737\DELu\132890N\134151\144431u*\\50\SYNkXY\ETX\168223VqU\FSy\ENQ+\SOu,9X\GS\1047812S+)\",\1039890)H\t\98271\58624v\DC3sZ{\987677\1044950hKs|\1075116{\DC4\1110630lj\b\ESC@@]\"\1084528\ETB\1109113n\186103\SYN6=\1089825p\99376e\DEL\59059oM@dj#;[p\DC4`\a*\ENQ.\ACKl4)/~`gh\1039150x\138352\GS\97905x1\127894W\CAN@y,,]9\60110\21835d\SYN\30156W?4\DC3Vv\1000370\&2}b\GS\1071563q\DC2;_r\96765f\SI\fAe\28624\17195l\57802Qu\EM?\EM\ENQ\ACK\1057411\53998eD\7978,{\33376\1048212\20693&~\5340{\998617f\151095h\v[\1068655TI\987641Q\1071007l\146591C\\xG\EOT\DC3\1094070\1112848\b<\1003643c\t\ACK&\1017644Z\128746|D\989138V{-\av>k&L\ENQ\ETXSX]\1068784$66B{\99819\136419\1084713\26722\&3i\NAK\139916h4V\161226\994\SOx\CAN:\46082\36147\1007223\1064886/\EM\DLE_\DC1\ESCY\93804\1027627\1009262\r\STXm\RSjL<w(K\27856I2\1068272_\DC4l\STX^ByaQ\113799\143780`E~>45\136730\1033758W\7920Zs? Y\1106262V\1058800?\DC1u\1011918\988102|\1078233 \1077733\7236{\136179I\FS+X\149145q%K\136585g\12885K\r}w\DLEJ\rwU\1035591Rd\1096288P{\133002SJc\190792@\"wT\SYN\DC2\EOTq)\1023110Nk\5727j.\NAK\\\174078\1016878\fA[,1\50612e5\1005815\ETB)GQc\STX?\1074421\1029288[\15055M2-D|noXT%\162274\&9\ETX\986969~'me\1076983\US\ETXFxln\163431B]74Y\1070487h\52881\&8c\4387!\GS'No9\ACKt\1052650uqhLU2JDl[HD,\RS]\1017266ndVT\166999\33264\&6\1031756\&1\SUB\fZ\t<'\EOT`\1026897w\174551\NAK\28884j\166511a\180747P\NAK%h\93799vx\1091423L]\"\1112252$\FS\154142\1074400\SI\SI<\ENQ&\DEL/\SUB+\42351*l\RSI:-GVc\1072360FZ\1024843\EOT=$Q\187473\1097917GL\98907hA=.Y\1053776\EOT\SO\ENQ\b\1034758x\CANf\f,`.\ESCe*Cqfn\992911|Q\GS[r\1004445\120509\&3C\149491\&1Y7SC\DLEz^r1\n0#\153294G([O\GS]BSf5\30441B\1014851\43562\GS\5897[\EOT\DC4\11444L\154935*z\1813a!\EOTB.\1075899\US}`\DC2"), newClientModel = Nothing}
