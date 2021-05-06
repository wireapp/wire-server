{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PasswordChange_user where

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
testObject_PasswordChange_user_1 :: PasswordChange
testObject_PasswordChange_user_1 = PasswordChange {cpOldPassword = Nothing, cpNewPassword = (PlainTextPassword "\DC18J\\gxCG\SI\26069!\n\\_=\DC1\141784\ETBt\1061529\vE\1001867\1065440\&7\EOTb1P\1074264h\133813\1063933Z\\W#$MM\24703\&1>\SUB*\8539J\DC4\DC1\66680\1102572\1050995\1113262BO.u:Tc\\\SUB\64944\bm\EOT\1090443\SYNI\1110890\EM_\1047283\1108418o\t(\ESC%h\SO\186144S8\DELd\1010917J\1003002>a`\1004362\128313;/M>\64515\DC3Y\ACK\NUL=snRO\DC3\1033314J\ACK~\133618\1022094\CAN\GS*m\EOT$\27123\DC3F=\SI\ETX\157590\t\134202$fuW\US8N?-\1062886\ETB\DC2xD#}(\155144\EOTkh+\DLEA\b>y\995718'\1025983\SYN\989344\1113302ce\1037472\bY\32879\1069048\DC4\SYN\DC3\187275\72856o\DC1\999149\95237\ENQf.xk9\DLEn\bo\166377U \NUL\DC3\4493\SO\STX7\NAK\GS\137954(py\v;}\1106535]\1001603q*T\1043728\FS\1029294\ACK9\1033382d\5501\160453\134033\1084017I\159735\ETB\1077442\1006720\&9t\1031339cs\SOHV&qyx\STXjI\176124+'&,\ACK\97257\1095816\1087832\136943\STX\SO\1030056\DELMq\1036102F\SI\1091342ae\FS\92704Kv\148324IqB\1008481\ENQ\28985\860\987750cB}%A\156816:bJ`\183663\1041458\EMH0H\RS~\46308k\917883aX( \126521\126606\ENQ\SO1/:\ESCU\35071eG\\pi\1044690(\DC3\b\ETXZ(\25968m\"\1057768b~\137782\1090137*/\DEL\98580\996512X\187337\52553n6zQ$\SUB\azU\SYN\DLE\\rs4\DEL?O{@CQ.\ETB2\987762\&2\\\994490&\vp+:0")}
testObject_PasswordChange_user_2 :: PasswordChange
testObject_PasswordChange_user_2 = PasswordChange {cpOldPassword = Just (PlainTextPassword "C*4\1098928b\1088437\171907WeL=9H-\SYN\185871\&0-7a\1112111b\SYN\GS\SUB\64309\22162\1029911\ESCx\127142\SYNTm8\DC3v0D\98014\r.\f\v\CAN\1029356&\995349\95491\150308R\29168\1097487\996192Zec\119107k@Ep)\1046027N\\d\133900\DELt{st\33625\14663AV\137561k\b@e'7\1111459T\158804i\35245\ETX\1113378l\97288\"^ (SPv\1025909'|%\182019@\RS#\ENQB*=o,~Nq!Ni\SUB\134319\DC4h%Eu\ESC\ETXU\151899U\ACK\aW*}u\FS\n\1053577me\1104881>l $2\1090170K6\123172\13534>j4\1047075\n\1074272;e\FSe\145457u?:\n\156900pn\189033a\1086734\1027797\DC1\DLE\EM|C\38483R\1018366.4\1007671I\"G;\b\DC4\992599\&6qB94m\CAN\1051863\1031543I\rW>4{I Pq\152425z<\1004201Bh\2464qDVF\991554\1107859\ETX\1055352/0_\27708\1009157#\185325\1068951)o+\60600\97688\STX\tx\n\a\t|\1093648\162892qq\ENQhE&;\aa\\\n\b\151265,\FS\US~u\1049056\&28\1103044\179084\STX\ETB\1001233'h\1102858\63079\19389{\45844PH\142917\FSilL\41018\EOTf\1057554\22480\r7dva\1093979\146879NGJ\v\176619k?\DC4m@]QZck\181368Y<:\138536\986139\ACK-\aP\DC3\165215!\146955\1091658\a\1062411\1037797\19612\ESCG|\178942\138386Z7\DELs\v\US\986214K\985073\&62\1110732[`\1090919\13750\&87%\RS\EM\14643j!\1096993\1033003PW17Kgw\165807D\59062\US\151473`}\DC1\a?%\30313@I(N\SI\26742a8\STX/Q4\985743t\151828\DC3RnBlo\NUL\1103377mW\NUL\b\987051s'\SOcC\21441BW\163006\"-\1108699\45740k\b\r\1103417Z\SI:\ETBnYIM#\r=V\NAKY\NUL\1062546X\983983\SI\1044432=\ETXu\1012141r:\FS\43676fU\1037863\1027775\3210\11213A)Sg\99749\&55J\1100723\SUB^j\GS_7l'\1066840n^&\ENQ\ENQv6>\SYNE\SUB\EOT\1054127\n\172248rC\187743Y\1110454KHAP\\\SIH\1052202\23459\th\n\NAKNf}\989382vr\995811\1072448\&7\ACK\ENQ\ETB\ETB\158284\&7&&=\DLE~3u\1106949\&7\DC2!can)\ESC\DC3\997983a\USd\995883[I$\SOH_\rK\ACK=\ACKD^^\SO\1000089\&2\1070140\&5\SIf\10701\EOThdN\SUBv9h\SUB\ENQ\SUB\1101155b?Lbg8z0\DLEZ\DC2dyN\ESC\NUL\DLE \1031124\DC1B\DC3\SOt\f\1070766\ETX\DC3\SIU\138576T\66458\&7\1003021\STXE\\\1069280,\"_'4vM,H1\GS\26634`c\ETB\DELG\CAN\25918\&6\43671\151282\&7\ESC#\ACK\SO\FS\34239J\1062742t~W\135376nsD\1015621\176248\120034q\1014586?z\997205\46265\&6\985308\1013161\147452+\9368\&1,3!\ETB1W\SO\158113)\NULP\1008666\123171>'9\a;\EMM{e\62868,o\987095+,?`zvs#\993902\n\SYN\n\a\DC1naX#\181896 \1072574"), cpNewPassword = (PlainTextPassword "o\60255,\SUB\1024958\989962'7\f\1008229U\1070481+A\DC1\SOK,\1096026\59901$\DEL\US\v\1018701\989782\1011713\SOHF\1021856\155470\aWDa\24347/\1035091@\119237\1069534\1083157\181080#U\SO\fB\25031\SUBHi?\NUL7;\NUL\1103861|E\ESC\DC1\71100\190845q\ESCKm\189563\CAN\r6\180896\121123I>\78673!.\48844\RSEdO9\ajaUa\STX\US\ESC\40228q\166362hS\65699u\146675e;\1029872\DLEa\aXCoW\1065648\&2]Hyn\1026854~V\1033469\EMW\64550\1101100\58858$H\45972lF\1006333+J\DC1\CANa\36676BJ\NUL\181248\1076608\1079672^9F\DC2@G\1081094Y.\1113513\1030797\1082113\f^\"\1018034\EM\1113853\175479\USG\RSh\145788\SYN|Pn:\DC1\r&\46690\985844\&5sc\20291\DC3<>j\1084138\\~]/\1052972\1028998o#T>B\50620\1100498\188469\184588\153096\171394*i\190421?\6054<]\EM\NUL\1040017\SYN\170090\t!aZ|k?\SYN\1018604\1077724\58462\DC4\4723\1035171\1081668`\DC2\b\1038781&\SYN\1101896]\134393=\1006990\DLEDH!f}\DLE\40006j\160250:>\46145\1100290\r\DELF+\1016116\999116F\1034022\&8\1028051.AI\31305\17495|9zX`\68179\41956\1106457\SO\\\SYN\SI\ENQ\1080112\129502;]zW\170940\&9\DC4B\ACKLT\1113725\v|%\7899b\1003498\CAN\992935T\SUB\150641Y\43841\1097440;y*#\ETXKF\1099085")}
testObject_PasswordChange_user_3 :: PasswordChange
testObject_PasswordChange_user_3 = PasswordChange {cpOldPassword = Nothing, cpNewPassword = (PlainTextPassword "\DC4\22659\b/\f0\33190\DC1\FS1]\vS\44144\162912^")}
testObject_PasswordChange_user_4 :: PasswordChange
testObject_PasswordChange_user_4 = PasswordChange {cpOldPassword = Nothing, cpNewPassword = (PlainTextPassword "sHme/\20063R!\EOT\110704E-\138611\&3YP\1065566\FS\NAK&|-\1097570\&3c\SYNt\aFC:\SOH\1067141V\1031132\ACK!;\DC2WI\SI\1061386o]\151739\EM\992782\NUL\n\SUBm(\15313\1034106\&0\ETB\1111843\SOVj\145893HhE\138722sW1\NAK|b$\151403\FSqt\1090800\1062333Y\DC1\1009678\1022763)>\US\63463\DC2R#`e#n\DC4\1004734\1031623\SOBD\ACKV\160179\f\1103436,s\GS\1031257\STX1G\1038500'bM\31585t\54330|\rRZ\n\994461j%Q[k\1073141^!/\NULcOO,\24218\&5FrNV*]A^J\fSfSI\t\r$\992923dy\139371_\48571o\RS?\ENQ\ETB3n\f")}
testObject_PasswordChange_user_5 :: PasswordChange
testObject_PasswordChange_user_5 = PasswordChange {cpOldPassword = Nothing, cpNewPassword = (PlainTextPassword "+\185331f6\tR\2411(\1068440\1064658\v\1045032B^r\1096753Q)>\STX;L\150236\GSA_\182170\58058\DLE]kE\1008697y\n\67321\38072Pd\149736\ESC\DLE\ETB\18348(.efe}q(\51492\983611\\\RS\1088749k4\DC4\1056524\r\1103194\ETBD.\SOH\t\NUL-=#\1113775\58295\&7\"o\tt\49599\146533\n0[]\RS.uR-<]\DLE}\126609\DLE\168520%\SOH<\155120A=\1041041f\1091748/j\1079238>n\SI\"~:m\32442F\98367u\ETB$+\DC3\1098010U@#\996546\&2=u9\11534\DELZ\1097027\20399X\59453\1051078=A.\127945\b\EOTD6`(D\150934E8m\ETX\671\&1j~\1043370{\SYN}\r\1048362v\35188#\SUBF\NULc.~~\54607Q\DC1R:\SI\18218$\1038508\DC4A\"Dr(|\1073989|aG\ESC\1000731\181950\1057035\STXko\62607n\NAK\STXC\1094557\&1'wn>gbTI\US\f\"mX;\RS+u:>\"x.U#\NAKV\1089113f\157413\917941\GSO8P1\148828\t6\1063448kl\DEL\171299P\171344\a?v\1102988;NCI;\NULZS\19726\n#\DC4=\1026062\&8-\SUB.\"\172430\1096257\bD\1103092x@C&XO\163749:r~dVBp\NUL6TM\59350\f\ENQ\RS1a\ACK\155694H\24716f\984396A;\132424r\DC4~N&FYx\1052328|\DC3)rM:\154543q/\13712=\17386\187331\997223\44903\987307\154689\1056894svP\NUL(1D\170831fc@O\154385M{\DC1\DC3\152320\125197\38599\1042762\DLE\t\992134\1091477\185206^\STXg\33306\t\1007122\1102907(\49051c\135071*E\121387@; \a\ETX\EOT~\t\98441l&\1069372&54\1052134\DC2\DC1T\NULW\1110345\162729kZ\DELjXj5726NY\1044576\"@\163500\52831{w[N.\DC3\ACK_\\\1047721\1081687\169751\DEL\111003R@_\SO`]D\1106465\146720\&9\161664\CANd|\STXd\f3Y[i\EOT.q l\SO\155349RW\"\CAN\SORi1\ETB\96470\150263\&0@KG!t\1103569\&9<t\ESC\31001\149703s\153788zT\27791z\1079430\172826\35902\11576`Q\142855IifDd;;\1055753M\1065448\EOT\SO\STX\22427\NAK\183158Vro\NUL\RS'MdqS^\161006\RS\100466]7;6y\1030433-\21545g56\ENQD\SYN@\US\FS\1066170\1075570D\1031334\&4F\RS.<*\r\EMfc\161135\1093259\1081894\1032358\15456n@8\179157f\1049124\r \ETBA\RS\1083691i^F\183893}q\64685\1046237\28718{j~bT\1051502\DC1!\b>_\1056984d\1104896=8CF\b1\23831)vI3I\USuzbo?|E\45935\1060777\135848#\EM\DC1gprq\1001182N\994959\1070976mQ\1029876\1088519U\f=[V\CAN\51255V$\92496a/k\1053620\NULi=\nmw\156189\133221X\127059/\24630\1076320\156946\&1\38477\1047688\EM*;\a\182339e^\1067265O\991855j;\STXD\n\1037588\SUB\49598}Z\131235\95099\190877\NAK\1054453Fw\1064693\\#\NULW\ACK|\51027G\SYN\1007816gW\bX3p,JU\133461#d\tD7\128032\1051369b\"KM\t-Y%y\1020543\SOg?\SI*\DC4o\1088032&\37499p\161627\DC1LE\GSM!U7\v\984810\156673\CAND\t\1000705O\SI#\EOT=#6\tDm\26151Fycw\f\1024956Tq\153247:{:\GS\SYN\998770\NUL'\6647\1099344J^m\SOH{\rJe\1046599/\1081287\STX}~\be\190695a\ETB\36742\DEL:kz\1047631()\120339\EOTM\DC1K\987733\37052*O\1025712_b\nuh\\6}b\1113520\142593>r\1004567\69448H\1072373\16270\&2d\1106042\&3R\129184pM:B\DEL\n\187845D\DC4")}
