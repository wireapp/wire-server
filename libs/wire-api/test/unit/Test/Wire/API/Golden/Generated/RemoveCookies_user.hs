{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RemoveCookies_user where

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
testObject_RemoveCookies_1 :: RemoveCookies
testObject_RemoveCookies_1 = RemoveCookies {rmCookiesPassword = (PlainTextPassword "=w\4648z\44696\NAK)\131109\991100\ACKs\t\"\92365\187506\&8 \991266cyh_\DEL\92618\149217S3S\1078459t {RF\54699\n\DC1\1005249\ACKM7p\1056052\1028531;\59228CF\DEL\1044459x\160099\1056064:\998599\&4\50099f\1075282\1016552*T\984321\ETXv\163991De{b\EM\1098789lrA\ENQ\24897\1025103\26004@)a\CANLD\t\170886\129422Fr>\a\ETXZh\"\ETB\" &R\FS\26973\59569?uK|T3\DEL\ETB_3\1063902qA(@\SUB\145462\a\ETX-a\r+k]G|\1038708j\1027672bG\1028099\1071492?=%V~\ESC8\1072574#zv\1092478tO0\140493&;iB"), rmCookiesLabels = [CookieLabel {cookieLabelText = "\ESC\1057742"},CookieLabel {cookieLabelText = "hqEs"}], rmCookiesIdents = [CookieId {cookieIdNum = 2},CookieId {cookieIdNum = 2}]}
testObject_RemoveCookies_2 :: RemoveCookies
testObject_RemoveCookies_2 = RemoveCookies {rmCookiesPassword = (PlainTextPassword "]\CAN(B\RS;\1057800\US\127867^\ENQ`]\58544\r(\CAN8\7874\STX\1107242=\FS\1082346MA\\ur/~`O\DLEV\154594\RSN\\}SJ\1101114\SYN\48100FC^+(\NUL\54573tv\1085755\987351\1092433`O4 \166976\1093199\1014922\ETXr\1032885\1039389A\n\EOT4\1093779M\32654\1081063\142821:Rx\SOH\164257nC\US\14901X\157847\EM:\1019699F\SIg8\ETB\ETX<**\DLE\SUB|J\1095677Mu\ENQn\15845QI\168878\1099629?4#,Nr\US\27348>&e\NULp\1007095\ENQ\b% \SUBP\1028758\&4G\177348{r\ACK\DC4_!ao\188861\179994\DC2\SUByY9\42082\135728\DC4gi>I.\997591\DEL\SOH\1064612E_\ENQX\DLEv\SI\1779j@\EM4\163405\DC1DyTF\STX\ACK\59339\1043145\ETB;=4/n1\v9\77951\SO\42887\SUBr|T\ab\1064733\174714\1099691d\1097338\"^\ETB\US\v\168646F26Rt8\1040224hYX\100185\1019387D\97215\NUL\63803\RSc\f\43683\3739\a\917994.xSPx@o2\1075876[+\150357\ESC\NAKN\SI\f\140940U\170043\EM3\40065\SOz\68352Z\15982\t\STX\1007639\&7x\144296\983127\ENQ\CAN\CAN\179311^\1037176\US\f\1112691\1041709\1001222E\EM\1102923U\64831EV}4Vx(~\rI9\ESC\ESC\DELZ\185889de\1004429\1025726\1031240\2001\53979\NULqS\1113905N\STX\SUB4\b)B\74524vF\"s5\FS'&\DC4\132750W\1006137j\RS\SOH7m%(\131099\NULH.$W\DC4}|D-\CAN\7075\STX\a\1102869\1007046\b\DC1\1016592\995095\ETByFn`X\131838\1000371\EM\22057f\SYN@\1023197D4\1080474Y\ESCF\f{\1110441g\1105037\t198\EM(\61264C\176787\rMXo\SI\177836\1086756\1085466Be;\DC4D4plC^\DC37ZakOzE\96653\1058885U8\DC3q\1008509\176562HPp\ESC\ETX\39558OT:.\191424\1078274Sp\147148\1007368+5\rVV\DC3]\30237Bp\1001163\1096050\DC1Ci\1054107|\FS\52645\DC2\\;\1109223-\1092668$X/V<\GS\154204\1013948\&3li\SYN\55185PI\FS\FS3\1074310\ESC\USv\42731AA\RS\78851:r\94306+$)8\989329\&0\"P\1105818\136792\SUB\1022988~\1024689tA'V\f.\a\35508D\140949\&6\SYN\fnm'\1035603\1000153iLwI\1322Y5!\t\SOAG\1035970\b#\1100172\1055535\US\100273]\1024560\n\1077647J`\1037320\1066408vz\183542\187289\DC3u\146347HV\1035964\53954\1074713dj\EOT?\SO\NAK\138059\NUL\GSR\ESCN3\1032285\&8y3i\DC2#\180905%[.\1048525K\SO?\FS\1004399\1000778p\987290d\987363\1023621\&9<+M\1014416\NAK\1504\"e\v\EOTQpI!v\DC4\ETX\US(\1102719\&1I\1109727A*\1106492\&3\FSl\by\163669\1069378Co\DC3b/3\1937\1000924m\19611S.b\101056e\DC4.W>#5\1022145\1083446\1009672O\7249\1100526/p>\1075860n$\1105667Hc\DEL26\150736\&2ZT7\\L\1068871\GS\DEL!\1046707.i#\1061215E\1077583\1066909c0\EOT\1055284\SUB%\815^<s\1027076;H\1071191ZBT\139935\1085309\ESC\173996Q\DC4,mZ<\150215>\37735A1p*N \CANHi\167848\RSu\r\132362kys"), rmCookiesLabels = [], rmCookiesIdents = [CookieId {cookieIdNum = 4},CookieId {cookieIdNum = 3}]}
testObject_RemoveCookies_3 :: RemoveCookies
testObject_RemoveCookies_3 = RemoveCookies {rmCookiesPassword = (PlainTextPassword "\18997\120812\a\1067476\1045632c\55247bF)\n\17493\&2>#t\167474\DEL3a\990200u\166327\1051091/\163369%\1046354(\\6\1099193\184952\SYN\tgaW%p\EMQ3p:1A\132360\ENQS,]\r\1075102<\145624\&3\58512z\162270\&4G\1097670\984809\9224\STX\DELw<)=\FSY\1065919\1090022\12623\1061276\&8=E9\7990\996581\US}`\SIDV\1091283i8\"\STXB\\H\NUL+'\59410(C\DC3\EM\1101721\118822\165433\986645\CAN\FS8\31465o\CANw\50707\GSZ\fe;b[\NULK;i4^\137910t\NAKU\984468T\EOT:\SYN=\DC4WQ\142706W\132402J;{\136472\SO|\ENQY\fv|\1095110;-b\1085666 \188286[zQ\20188\ACKhFZ\FS#8\DC31\EOT\DC1&(-5(+\1096888S*H)\DC4\1100749E\1011007\EM3_\180709^\DC2qQ\1096034\&1H\NUL\14184ps\5952\DC3:\45532x\DC3j?>.\186008\RS\1054999vgms\174837kH\1035635`\1006674:@XBa#1$mfTZ`\rPM\1061006\EOT\984023r\RS=\f\21693\&5\1106095<\1095761s\134867\ETX\STX\150534\EOT\r%\"~\178794ZMC]N\\2MPo<23\\Y\1080578n=\161441sf\NAKi_{\5660- \ACK~'W\143804Ap\984460&\38891\136712a\RSwX\DC3?\14017g\ETBsX\1074713)\158694\DEL\tj\DC4y\STXq\FSyz&\v\1048526\DC1O%\97709XvQNI^\1043857P,\\8\1065917\v\1082622\38730?<\ETB@c\a\17462EWkz\1062475\100332dL\DC4\STX`p\186523sv\1029457\1100830\1038629\SOH\1006791\DELM\1022816\v\FS\ACK\169799>B#\DC1v\NUL-H~\1024051\&4[\994993\1057413\1003709?Ci\GS\1047993</X\171534B\ESC\ETBV\",\USL[7d\GS/8crt\1061250\5604\"X&\SOl\DC4\NAK?\CAN\1069111#\RSE;'\177261\1098990[\STX\1015629\&36\EM'P\156376Pbb\DC2\14911\ACKBOx\DC3\1048405\40366z\14869+I\STX/\33120E\ENQ\123604"), rmCookiesLabels = [CookieLabel {cookieLabelText = "|E"},CookieLabel {cookieLabelText = "5"},CookieLabel {cookieLabelText = "\186803"},CookieLabel {cookieLabelText = "\1007941\1059463"},CookieLabel {cookieLabelText = ""}], rmCookiesIdents = [CookieId {cookieIdNum = 1},CookieId {cookieIdNum = 1},CookieId {cookieIdNum = 2},CookieId {cookieIdNum = 2}]}
testObject_RemoveCookies_4 :: RemoveCookies
testObject_RemoveCookies_4 = RemoveCookies {rmCookiesPassword = (PlainTextPassword "\43922$\58340!\1025797>F)6\1084726\1114026d>\1068389@m*h\"P\EOT\1049417\185910\1009736\RS![\DC4\SI\1069562\8832\ETXw\172783yiL\CANl\ETB\US\48506\94213\1036617\&4\1060439\1029569\DEL\DC29HG7$tT,9\14488\1110194\US\f[e\1062831<=\f0`\153384\\|\NAKBX\47851\32547\vO_P\v\1039318\1024297f\DC1\1064931_\1007280_\SUB\EM^}y\41232,tz\r[K.\1038490)*^gW\987594oUO\NUL\ESCj:X\61332+\b\162535/C\EOT3d:W1z\EOT\GS\1084412=U'SR\1101458\GSUKP\11076\\o\63058\&7%EQv\EM%\63970\STX\138681\&5\CAN^\144941\bLsl<,CzF\SUBa5\vxT?D\n\153180\SUB\EOT{c\60150\1041030xic\136135\99766H\1097696\1100716\38724\1082259Q\1083127\r7\ESCDY\b\SUB\SO\CAN!\ENQ0\NAK\148303Q\ESC\STXe\ACK\DC4d6\1096775\&5\167291;x\ni\160755]\7252lA\1111929\1056355%\1014890\ACK'\v\DEL^\23083\41103|\1105625jE)\DELZ\35413\169455\140652\1018230c\NUL\b\DC4\169445\15762\f\24851\41952$5:B-\1045750{\186982\1037341c\CANVM2qIS\rYmj\SI\f\17552\DC4\ax\137278\35222\&2J\43244K\145815>E`r$\1036625\1109284}!\ESCqa"), rmCookiesLabels = [CookieLabel {cookieLabelText = "\985242\165538L"}], rmCookiesIdents = [CookieId {cookieIdNum = 0},CookieId {cookieIdNum = 0},CookieId {cookieIdNum = 1},CookieId {cookieIdNum = 0},CookieId {cookieIdNum = 0}]}
testObject_RemoveCookies_5 :: RemoveCookies
testObject_RemoveCookies_5 = RemoveCookies {rmCookiesPassword = (PlainTextPassword "R0\1091319?\194635<PV\EMR\1088617Sp\r8q\\\1102199\165830\NUL\SI{W\EOT^F\46143\r)\DELi\985259\1038705)\ETBT\1001222%$\1002228\DC3\vV;\41625\EOT .'M\US\rK3\54478\51139l o6\14516\187030$\SYN6I5\ETX\119169&<\DC3#\ETXx\ACK\ENQ\45082=*\1056144\SUB\1099768\SO|\DC2\14135\63015\SYN\1044182P\15883b\EME\1035702\&5\1046260\144898\44551b+\1088756I=\a\SOX\EOT\EOT\1036553Uz,/2>f'<\ngtTZ\f5e\GS\FS\159153yX^\33771Y7z/\SYN#\DEL\ENQ\1058398w\ACK\DC2\78743L?@\ACK\EOT\ETXM\1067372\1046069\1006142\\\ENQ>$LPX#\NAK\GS'\1039223\&6+\1102271|\182891\DC2^rV\136927cjA)q#\986073\ETB\ESC\73112\1048583b\\\FS\ACK\SOH\rf9?s\SI0\1043013b%Bn$MJ?\159450\EM\EOT\5448]oV\1100713%)N2;\EM\NUL\1004672U&\1016384:\1047548A\148162\1088903pH.0\31675;YJ\1015197#\1096279\NAK\1003857D\1019128\18008\126564}i\1038739N<*I_C^!g\SYNC)\ENQ\"4;\150743\EM~T0"), rmCookiesLabels = [CookieLabel {cookieLabelText = "{"},CookieLabel {cookieLabelText = "\ENQP"},CookieLabel {cookieLabelText = "\DC3\1100799"},CookieLabel {cookieLabelText = "%"}], rmCookiesIdents = [CookieId {cookieIdNum = 4},CookieId {cookieIdNum = 3}]}
