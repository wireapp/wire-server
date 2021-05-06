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
testObject_NewClient_user_1 = NewClient {newClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newClientLastKey = (lastPrekey ("Aw")), newClientType = PermanentClientType, newClientLabel = Just "/", newClientClass = Nothing, newClientCookie = Nothing, newClientPassword = Just (PlainTextPassword "\ENQA(&\136117{l\1002586e\t\NULjH\174132\ACKc\1055911\a\r\1042571\f\43369,SKQ\1079786;=P#7F\41812\tX9:\ESC+9S\ACK[(uv\1016711\1058819t\1057336\SOx6n;\SOT\SOH\97155w\10612\SO2\58033\&4.\SO\1011212\ACK#\1601P\1052404\DC1O~'\54980#B*F\6287\t\1020230\100696\v\\&\54112W}>Rd8\1018971cs\1047852\59250\1065155\SYN\41275\frE;\SO\38144\1060867a=U\59967!\176079?\139410&\vBtq@\1040505\63221]9(xp\180911l\1039303\21408!M\1057259\1040022jb\1107063\1043422\21409P\ETX\128610;\ETX74L\9776\NAK\1103735h\DC1*\1073606\DLE\64813N]\b\ESC\64119f:6\SYNc8\9864s)\FS*Nis\DC4YU\SI\EM<6nj]\7649{vd\r\185343V`BeeaSo+z\DLE\STX\DC3|e\n)\ETB^\160680.^@\190598\&5|\rj}\1066299i\158362qT[@7v\ETX\ESC6<\1093633\1051899\34747=\ENQ\94586~s(|s\ahnGTqs6s\US\44472*a J'Y|(c0DcB\SO`V\SO\EM{\ACK\SUB\1010333l\\\1021668Ni\SI\994175\62640=KC]\31345\STX\27126\1092165\1013976\172996\NUL\181481\1002073c\184839\60632m2zj0Kh\1069799DV\SI\1058768\31327-Z5N\63914v\174588}\1018088\"\DEL<\ETB_/EK5%S\188873Aq\SI\1006398\159215C\11700\1031818X\ETX\SOa.Pi\1013789q[._@\53839Z\ACKk\DLE-L\98845hI+5(V%d\984638V\154623\GSpnO\ESCh\18624')\1049984\&6j\1078510\983042\1044438Q\DLE\US\991752XV8\175416W\\\DC3@o\ESCu<HI\ENQ\160564y\95316KVBm\b\1019658h\ETX9\SOHN\b:o\1042651\1041776h\SYND\137278\187984j\"\1004750bW\1065510\"\189477O\144339h\1016989Q\1092688d<!\83186\ACKIG\1047080\1079714\DC2\162391\&3a?x\CAN\30996\&1\v\147525@\96074\32098SY\1108490*MN\ENQ\"\1056298\EM+A\b(A!K \n;\ESC\1031069\&9\153137\&7j}cegG\DC4\SI%/rz\30190ctemXH+T@\DC2t\157157\1090610\ENQ\SUB\DC3HZY\a=!\1027745\DLECKL\1051694\USl\ETX~!f7;vI2\DC3gx4\178457\988709SS\121120Q\999494)\r\DC3ap\a\DC2u\FSnw\1066034|Bh5'Ul\21073V\1084254H+\26234'\DELWrTeS\51801\139148_\1063109\159689]\v)\1095978\46824\156998\SIx\b-\10667\&1\170476\135428\DC4\141765Lp\986441O\27923\FS\70106\t\169694!\US\RSC/s\US\83289u4m\1102637\996144V\32604\SUB[\ACK>\989741\ETB%\59264!nx\48631ZHC\1072716|{%\rp6\ETB\1067034J\">De>\99226!Wk\24387sa\1036553\50896\176157\28465#tii*\RS@vV\988434\RSQn6.Fv\50607\43361\1099697\1020674\1015936J\74482^{ca\"WK\DC2\1109306F\1090870*}} \78298`\DC2@\1019691\991784NbE\ACK\34985L5\SI(^\155480`X\EMsK.8tJ\1012206\SUB\170602\1044732#\EMt\168867\CAN\DC3\FS?\SI\37469\996364\9478\156728\&6z\1027891!agf\98616&yy\1079429`@\SOHoa\1111784\n\DC1\"_\\\157150\SOH\ETBe\148697[tol\DC2B\US~\158472\3412\1111076|}\988503b\1030568\"lo3q\STXC>\985421\8922Q\1095570u93B\15396\ESCz\\n\a7d\184414K\1041255\DC2[AejUw\SUBO-i\97738\987438F\1032986`\ACK>\1001369\"\FSO3Q\1048805Kk\"}>\39556\995494\STX\1076555w}!pgb\142429y|pf\1095650y2\SUB\\9F\\#9Lie\170084\&5\189361\\M,@\1047495H\1049490W\1050362o\984545}\99145hY]\GS\178566\t:m>\STX\986905Xt\50658\1029488\SOH\1031\1079732m\1031865\51960r\\\1095645\&6`\DLEP?G_\991984\1016672\998864^\987386n!y\rd^\SUB'f\1021431I\190878K\188735\92418\"\1088355M\EOT\SOH\1000519\DC1\1017986},\1019889aDn"), newClientModel = Nothing}
testObject_NewClient_user_2 :: NewClient
testObject_NewClient_user_2 = NewClient {newClientPrekeys = [], newClientLastKey = (lastPrekey ("3.")), newClientType = LegalHoldClientType, newClientLabel = Just "\1068185", newClientClass = Just LegalHoldClient, newClientCookie = Nothing, newClientPassword = Just (PlainTextPassword "\1020852z\60493\DC2v\1085367\SI0Z*\1048334:Uo\1059262\141421pRB\1047749Ig+&K7z:\tL~\DC2#\153624\SI\\m8\1099876\RS?\SOHM6f$\30853d#Yf\1110141>\ESCXo\v\990329s[}u0\1079967\1020355r\96584F\DEL\DLEA:N> l.o1\180336\DC2\ACKVRP%\1032270\1085588.YY\ACK6zC\CAN\94617\b\EOT@\f\DEL,\SO\170904\DELM3yX`o\58021\&4_\96249i\SUB\128669\&11\33046X\ACK\ESC7:\143840;0i~3\17346\a\SOHN\178980\146887\DC4\61060n\ACK\ETB*\1011030}\NUL\1071277L&&O\1063650+>\RST\EMrUgA\1104663\ESC\r7L\1076183q\16904\147437\DC46o\STXX*\ESC]_\1040278ruCm\fIp\28046fSmG\GS\ETX*1\SI}(c\94492\47669T\1051718\999591\n?@\STX\166730?wf\GS\DC1N\n\SUBhm\31991\rp\19928\DC1\177425\DC1\EM\99894\1050313\1090689K\163795)%R\NUL1\SIhjB)\v\US\188101j?bd\CAN\"nPd\5747\13364v\1089213\1026897m\SUB\NAK\SOnKv\37504Ba\1015240$\SOH{\47751t\ACKeL\1055611\DC4\CANzt\60065\&9\ba?S\185266.\44901*U\133153sg\1076371k/\50018\1077048o8Q\135487\ESC=/\SO\1073425\121400C&`\110775{\64083\b\GS\187764>\n\ESC\1105892x\ETX\44093}e<\DC4\1099001\&3]\1057894\DC1$\1048292'G\EM/r.\CANd7\b\125102\&5\SI~\EOT\1094328\&2n\157643\SOHP@M\b^\f\133321\SIN\4392\42487*\CANn\16169\1101691\1100598'\1049299\985866"), newClientModel = Just "_\1108781"}
testObject_NewClient_user_3 :: NewClient
testObject_NewClient_user_3 = NewClient {newClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], newClientLastKey = (lastPrekey ("\ESC=")), newClientType = PermanentClientType, newClientLabel = Just "o", newClientClass = Just DesktopClient, newClientCookie = Just (CookieLabel {cookieLabelText = "Q\EM\52743"}), newClientPassword = Just (PlainTextPassword "4}})G\DC2.\13788e\1002031&e9\STX.*p]I\24804\33146:PS)\96754b\ESCPc\r\8889\NAKN8\1105744j\ESC\185037\ETB\EMu\ETX\\}i\1042007=\165424[5\7826f\DC1qE\4776\160129+\US[^-\151309c\1027427Hg\EOTOr\NAK')\1064902(<\1009469+\ETXS-|#l(wmt\1029702vd{\1001166\SOHv\v\1049937\28094\FS7bh\RS\151025\98684yJj\36993jA\DLE6c\177961~^2q\USrMAD<W$\9406eg\1081167\95417\STXM\1076138V[ed'\ACK}<p\148814'+\1045494&\ETBa\DC26\v#&NU\f\v\n`K\995165d\999006}\154857i\985475\174442\144248\1089811g\83021{\169286]av'z\139278V@M\STX'<y8l\1100974J\GSw8o\1014668}m\68109\EOT0?\1034498\1085808\184884\21209~@\180941Zi \1017581>\148063\1022504\152361kG\STX0\n5\NAK\174126>\1052505\EM\ACK\GS:\153685 \fG&\DC4s\8831\1016721\f;\986086G!>E\987380\1005445'%Vn\ACK!8\989245\DC1Jy{\1090453i\r\1107246\991354^\n\185440\"Dl\DC4jY\ENQ\b\ENQ\SYN\991976\USqD\99428i|7\34725CGgp\152000\&8dh,\38094Yr\94820Wx\50481#\SUB?}lm/bJ\\\1104303\NULn\100508\DLE\ENQ8[\\\38671/-\EOT\bt{j\FS\1056320\GS\DLE\1470\50211\rAQJn\ENQ\1093460K\110851#\153924{\DLEW9\143459!\ETB_KA\GS\1088553G6,dD\EOT#\150387\\\GS\1066443\&9y\1036188d\1042669\tB\1036008VN{\1058544\r%7[Z[1\1653WE\DC1i\1111914B\1092042\70275YL\997513`K\10697$bb;e\1001981\t*\STXA{J\nd\NUL\NUL8UF$6\DC42j\24330k\NUL/3\172037\140521*-4Fg\1094555S\1113735aJG\\\987888oT\SI;\US\1002970k\1020412\SOH'Pw\GSx\169819\1022849)\ACK\22977\SIE_O?;\RSX#[<\CAN\"\184925TE\31021ya\r\1094075\36201c+\EM\987800A\DLE.e,\1021019|\RS\143998W\151270\18884\DC3\28173\157470sy\t>i2\EM`(\1105860\SUB\987188\SOH\DLE\rt\61217,v\98531\1113215\120845\43425\1014593\DC4}\n\DC4/\ETXrs\tZq\SOY`g\1006625\SOVe\1020776i^\126075HS~\998696\180362{\1022591\1080357\1047157M?J\FSa\SUBk2o\171870\166276\t\b\1098989\&6\1051285\&4x\1094694k\34297\&3\1047292\1101459-|\t\113727\1099360\&5e\EOT\DLE\45344b\USh*^w;yaoQ8m2\ETB[\NAK\155942\&8\EM!+\1097050\"37#\57837=\NAK\187618:4N\7839 \\~\186905\\\46854\1024211\1058913Ru\1042196\1072749\ENQ]Ko\ETX3m\DC23\SYN\991718,`GbT<^^D\nr*\1107541s%V_J0Q\53155\DEL\174551\EMG\45287\1010820z2g9t\1042121[\f\ESC\GSj6g X\nK\121023N\995216\STXY\1033149\164134Z\990101\DLE<\50471>$\99552|\FS=\FSO\94625<T\1060036tEs3:\1057796\&3\63874\&6c\FS^X`cM\SI\aJ\12465\1060357XE\128868d\"\DC4\989632\1097764O6)\a^Ygy\SOH\191402\SI~`gg\NUL\173722ZG\b}1t\1064271M.A=Y\1058713c\EOT\ETB\v~W\teR\1069206\DEL>\1037761\SUBSM|IZ2=U#\SYN9\989047t\b6[\40157\f?\NUL\993120\151556_CHy\166294\1026759@w]\ETX\45013W{#\1005973"), newClientModel = Just "Q"}
testObject_NewClient_user_4 :: NewClient
testObject_NewClient_user_4 = NewClient {newClientPrekeys = [], newClientLastKey = (lastPrekey ("")), newClientType = PermanentClientType, newClientLabel = Just "P\DELG", newClientClass = Just TabletClient, newClientCookie = Just (CookieLabel {cookieLabelText = ""}), newClientPassword = Just (PlainTextPassword "\39076\183966%[d\62816\143322\997618\r\bc\1101930\ESCo\1055537\RStq\74766Ie\43347Z-\186175\&3i\ACK;&}T\1091460\SUB\1095631o\bwysly\fhSM\ENQh\CAN\987837\SYN\1020968\SO\150839\DEL<\999010\f^\GS\\@X<):.4o@sk-a\1065190UJ6H\DC1+z*$\988522dl\ACK1Fk\15320\v>\CAN\US\ETB\\;\176323\b+\1011844\121034;Ckp=\17186.R'<\r3\v\1095356\ETB\n>\9945k\r]u@[V\50347\CAN,7\157543\1011024f\ETX\1089001KP\DLE\DC2\11651P\187789\NAK"), newClientModel = Nothing}
testObject_NewClient_user_5 :: NewClient
testObject_NewClient_user_5 = NewClient {newClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], newClientLastKey = (lastPrekey ("")), newClientType = TemporaryClientType, newClientLabel = Just "", newClientClass = Just PhoneClient, newClientCookie = Just (CookieLabel {cookieLabelText = "3\1017038E"}), newClientPassword = Just (PlainTextPassword "\1072104\1014875M#6\70465\1003396\&7\DC2$\137857\21830/\USeN\ETBN\1073816@}'~`@\ACK=Mhc*+=:\1094510\168593K\1051611#\987743\63045?+>\142428s8i\1048984p\176209u\187446\SI\ENQU\188821\70106\46912.K~=\SI\ACKqI\DEL1\1069230\v\1007904\&2}\GS\ETX0bE\37751\DC2]'"), newClientModel = Just ""}
