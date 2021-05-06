{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Login_user where

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
testObject_Login_user_1 :: Login
testObject_Login_user_1 = PasswordLogin (LoginByPhone (Phone {fromPhone = "+0944287442"})) (PlainTextPassword "iB\f3\1011011\59519`=D%\96379):\STXE\1074338\r\DLE2\999019\DC3\1041198\EMz3\ETBv\v7|M\158311*!0\31006\137191\&8+p\\E\97170o)@\1091921\1049504B\US\917837p\GSg\12750}\125027}") (Just (CookieLabel {cookieLabelText = "Of}\STXP"}))
testObject_Login_user_2 :: Login
testObject_Login_user_2 = SmsLogin (Phone {fromPhone = "+18829767380416"}) (LoginCode {fromLoginCode = "\DLEb\b\1076680\1102234"}) (Just (CookieLabel {cookieLabelText = "w@<\123591\60880u"}))
testObject_Login_user_3 :: Login
testObject_Login_user_3 = PasswordLogin (LoginByPhone (Phone {fromPhone = "+976624164556735"})) (PlainTextPassword "`@\12939\1066763\GSR\94527qRo\63503\1018247sU6\24947e\178342{R\1058543\RS\19238\1058836aB,9\r\97261\983094*\DC3\64566$\94065\t\DC3I\ESCC0\SYN w}#c\GS\22924Q9[%d\SYN\EOTY\137205S9\SOH/%SU\155220&R_%,\ESC\1104029\988421U2O\US\SYN\DC3\ACK\1036794n\63245\1009328\SI.\CANf[\ETBK*4\n\1003590\10645\1071367\\`<\58568{9|\1013119pgq)wig\164230Y9+NbE\60691\2887.Dw\SOi\135364\DC1\993707H\SUB@v=\110757\1048628u%bL\DC1\1023077\RS']\1012829\1007030\1044784l\7352\1083895Y5xax\1080078x\v\1864\1041484)?L0\SYN\DC3\DC1lIHD\DC3\1106012b}}q\94866{l5\STX\1075487\DEL\1022198Je\998949ub\1022622_)ZP\EOT\1068037\&4\40316y\ETXm>\50089@\f#XAG\GS\SUB\53372\DC1e\22796a\b\12324\SO\SUB\DC4O\NAK\16352n\1053141\171723h\EMC\"\1003211[\SYNU\169077\1023805\1029950\ETX)Fm\f#,\997167jq\DLE7\fI\98811wF&\989696\ENQ)Q6\1079180\nDg{") (Just (CookieLabel {cookieLabelText = "\1030319\SUB\1000256\131204\DC3"}))
testObject_Login_user_4 :: Login
testObject_Login_user_4 = SmsLogin (Phone {fromPhone = "+322458851442"}) (LoginCode {fromLoginCode = "v\SYN\DC46H\FSLt"}) (Just (CookieLabel {cookieLabelText = "\1036780\28741"}))
testObject_Login_user_5 :: Login
testObject_Login_user_5 = PasswordLogin (LoginByPhone (Phone {fromPhone = "+58386843306214"})) (PlainTextPassword "Ek24q\v\73936c\137667I~H\97292\184212v\15062?\ENQh\64014\ENQ^8\\\SI\111338_a\1042468S]),GlwJ<N\t\SOHL\GSGP\182285\\\148173=K\b%4\1080815L5\CANH\4968)\1043995.\1041636ql,\CAN\74119&\1031983\1056684+I\SO;C\131859kOEK<\1094247X\DC4W6E0\1045252B\ETBH\8324\1079996?m\ACKz\ESCA\12715\n\"\GS\US\77824C\1063545=]{\DC1\NULP\\\ACK\NAK~\US\b9Gf^\43004\1079324\&32\RS\ETB\153753\32172&\1010751\1071732\a\987998\DC2mg\1075677|\187296g\SIb\24659B\DELt\DC3jM\SO\t^\\\1029245\142817%\1104545\1096545\1055035L\RSK,\DELT\1057389\SOP\EM\nn@\SUBv\fEw\46046\nk\160671tN\NAKB\EOTWF\na\f2\\,N\RS$\RS\99261\&0Xy\167580\NULa5\DC3[K\1018616\&4\FS1\nc'c&!{7w\1034206(gLs\EM6T:A\SOwR3\134282j-\1101786A}\DLEni\24122J\f\SIf\DEL{bG8?$n&{|L0\158285ozf]?h<d8N\STX\SYN4c\n\SO0G\"(m>\v\987439\5902\USx\22244S\1095303\133259O\DC2\DC3B\1105701}G\1082566=\DEL\1090110\nS\\\988253jKx\1041397{\180691R\18211\83099\13532\134112l[\1079065\1072053,\59742Cd\1027772\176848\&5id\EOTG[I\1006505\29954B\STX]\DC4x\EM\28562G8R\n\ACK\1056122\34769\&2\b\t)\1104392\SYN8\9690<G Xk\vXP\ff3U\1023893?XgHS#(\ENQK.-ue8oA\DC2?\t\169307\996777\179091\45967\DEL\ENQ\NULe\DC1j[/\1314\RS\182192AB\t\EOT\83347u@\44798\&8n\51796\1011345SiH\SI+eyH\1081112#1\129123\EM\182084;\174105-;B\181878P\131438\ENQ\GS;}=\1079426[<\367>y\DC2\1006510yH?Qb8\DC1<pI\DLE\154584\v.\1096834{\58382\989290\NUL\985162\&0)R(\62803\22579^u}5a|y\GS\986741\&0\ENQ!T\1058287Rn\bHJyS\119001uHfx=_i\58981u\ajKn|\46114kGs=8\1046358J\174158P\1068350\ETXE\1007816\1033994D\1003211Z\NAKh\174240\NAKd\1008316I/\DC2\65294\&5Dh\DC4n('\GS4=h[sKG\NUL\141381M\22124\"\EOT\141081\EM\SUBRV\161183\EOT]u\SUB[\168155Q(\\\172111,O3L\1007307wr\138190\994436/\1107602\1101635:p\152692\143591dA:;4pF\1029658DX\120580\51815\n\b\161748\NULO!R6GG\1062362\53769\996533Etc+t\53964tw\171959*w(\DLE\78432\180178|p\1002774_\DC2\CANN\\\947H%\166499\"\139158\70289\NAK\v!\GS\147979Q\1072519\STX@e\DC4\1088853~z\DC3Qu'b\1078407\97805\r\10248\1105770\177814D3#\989878\CAN\ETX\STX*\STX\STX9\DLE'o\1100176q\1065323\36305b\NAKP9\EM\r@\DLEST\160044\a\\3\1113051\DLE\46187\FS\71101\tD5j\ETBfD\SOHP\1018968\EM\NUL-\1015286'\";\985135\&52\1084130mL^'\ETB0[G\DEL\NAKl\1013217!\\\999503[y\ETXmiT6=\"&K&\SOHE)Y1\8511\FS\DC2\1029724\NAK\1108726r\986036\ACK]i%^j\1056803\&7Y5\94597\134772\51206\ETX\1016312\1066249\161517\1096309'\EMZnN\b\1080414)]\SO\65613\10729\132176q\32682,\1027725\121396<Y\DEL,$n\1040921\145755\DLEJ2]EM4yI\1099801\1004332\1079030\SYN\165164\ETX\t\127031\"}cjY\a\64341J\129109\"i\71437S\ng\74753\ETX\1069038\175922\\Q\1059470>\5025\SOH\t\RS\61812\1104423P\FSJ\21068;\24702\a\ENQ;.\SUB\189864\txuy\48918\29411\DEL\994586\&9\1038760\r6\1065304\n\42300\DC1u\1023617d\DC26\ESCfb\1023495(e\FSG]q@G\1031301\&9\bg\ENQ\vT\164460(e&\7780\1081011r\46755\990177A\DEL\1082612\GS_\1102905v\17153z\GSq\EOT\SUB6^y\ad") (Just (CookieLabel {cookieLabelText = "0Qk"}))
