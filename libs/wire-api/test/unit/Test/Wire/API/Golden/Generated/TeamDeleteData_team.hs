{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamDeleteData_team where

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
import Data.LegalHold
import Wire.API.Conversation.Role
import Wire.API.Event.Team
import Wire.API.Provider.Service
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Invitation
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_TeamDeleteData_team_1 :: TeamDeleteData
testObject_TeamDeleteData_team_1 = TeamDeleteData {_tdAuthPassword = Just (PlainTextPassword "\994175\US3\1010981}Up\EM\59661\SITZ\177663\ETXa^\SOgy\1087891\5644\98730[p\1007573\NULKv\151553wk(DkgM\1077103,xoc\\\DC2k'\v\1091283\24836\&2 c\47301\&8\a\ETBlc\172471\DLE\b\r0%,6\ACK\2403;\149435y\1028817D#hZH^&\1077430Ql$\DC1[\\\DC4\NAK\5551<\1101094\1716<\1083553\57466J\998894`v\1012493}G&\93781\1043925\DC4j\SOHX\1096869o\148055\1089617\n\886\167387\31381\SYNeC\DLE\38660\1104497\1068422\ETBG\1078097\1101454\12741\ENQ!~\EOT\143052w\NULE0#6fDi\187023\156784\155991\133657<Z\EOT\SUB\1112335\&8}Fh\30975\74077\\lq\26854\SOH\1091191 \ESC=\EMJ\1027991\15779~(\DC1mP\15686\DC2X&\1013558g\72283\1023650\11803E\DLEg.`I\DEL\ENQnG\EOT]gX&\DC1%H \1001412K\125216Kv\US\172298\50637U6\v[\a,\123618\ACK\12818.;_H\71727\STX}+5-\aB?D\1024132\&8&\v.\1021764\ESC\EMj-E|nb4mc%\160596\1005713OwdW\995905\NUL\SOH\STX\160981k\ESC\fp\SYNz;W@\1026619_\1012333\CAN\fTD1\r\DC1r\fkH\nx+:-\t\\5\1007515l>M\EM7q0QW6\DC3\f\nNO\61766s*.e^O<}h3L-jV7s\ACK\48453\1005423\157987G\ENQ\1085633\"\58903}\993821S\181187z\1055184\aa1\DC3\145975CE\41441\&5=\46172\1024801z8Nd\119530\1079018!\1039284~\1107746s\SYN\FS\177760;C\1067034\1112067\ACKy-\1064972m#%\1087904}\FSK\1094063$~O\1020665SvMJW$\169312L\SYNt\n/.L\ENQ")}
testObject_TeamDeleteData_team_2 :: TeamDeleteData
testObject_TeamDeleteData_team_2 = TeamDeleteData {_tdAuthPassword = Just (PlainTextPassword "\1010443J\168108p1$#\NAKty\r\1052006Vs\CAN\42118\994059\vBJ.u\bD@6\61863;C\n\GS\149097VN\ETBJ\1053224\DLE%o\DC3t\NULR\US\CAN\RSJ\SUB\33051\DC3C3,rF,\USKM2z+\161275\rL\120186\DLE`!\CAN\a\DLEq\CAN\1066252o\ETB\ENQ\tV'C\SYN\185156-hx\22657\143994\ESC\1074551\141263\59365pp\148269kG\SUB\NAKI\DC2Cw\SOH\1050868\NAK\"\1022044Wu\1046398\176652\"F\DC1[\1101975@c\GS\CAN\51842GV\7987\178077\USyvDE\153803\151476Q;&\b\ETBX\DC1\1007422LyuKF\69779\68648\ESCP{\SOH\48132\ACKqs\SOH\185787K8\15778$#xW V\1006817Mw\1050072f\985982\&3]\126083wP9N4S \1024884s\SUBx")}
testObject_TeamDeleteData_team_3 :: TeamDeleteData
testObject_TeamDeleteData_team_3 = TeamDeleteData {_tdAuthPassword = Nothing}
testObject_TeamDeleteData_team_4 :: TeamDeleteData
testObject_TeamDeleteData_team_4 = TeamDeleteData {_tdAuthPassword = Just (PlainTextPassword "Ux\137650\NUL\SYN\ETB$x@G+\DC2w\21666r\ACK5{N\NUL\25828\1101084\ACKrL{\ENQ\1096880\149035kQQ(\ESC\132665\r\44729\1032939\DEL\SI\a\187307\136403\ETX$yl\22579\54313\1106682Uk\SOh\1047013V\160319=\DC4\US*\25844b\NAKU%\EOT\29302h\CANY&XkybqB\bZ];\SOV@\996235\1089318\34031}R\aI/\32494j\21160Ai\163462\voTJ+#HP\1065970Fq\1028003\ACK\30641\&6\US\161507\991368&\GS\28817)\1058664GgdlS\"\DLE\997057\1037803\&22\DC2a2^Z(N\7695\110663XgNd\1065252+\1060306\NUL \131148\SO9\99972\CAN+\DC4cf\US\1113406(E\1108051M\STX+\a\7769\1090061d\143464c0!@A\t\SIP@+P'VM\1040858k[?\135297\f\1086287wDv\SUB\50821u\186040\neb\DC3m:\DLE\1026510\1080921_\EM\DC2V\1049531\ENQ\SYN\61299i;=\175808>\\jl%\1081334yc1;}\n\t~7qZ(k\1016009mWs\183898*4;\FS\SIEKl=\143438@X5@0\USf\USP6\63073f2v\"^VwL\n\22205\1013891\191294$i\1031657.\70021\a\150222\EM^\RS\14939\74304I\145502YQ\EOT=2E\b EUz\DC1B\45724\98071_>\EOTW\ACK!\1032498\&4N}\ETB~\994299\STX\168831}\986641\"\8340\1040976\rMo9\179344E\1049415y\135582\1042819\CAN\EM-\ETXOz2>\SOHFpe\v\1083442KH\USI\983681\&6)r\1022310\1098475I0\DC3)-Xm*\EOTEy\1097012\DC3\92669,}M6$\SUBbm\DC4\RS=7\23871\23175\30118Y\NAK[T(\DC4PY'5\1001259\1068749\1070284\95681\181573|\DC3\191278C\30906\&5\169139r\1002868T\1040004&\49808f[[y\1113283\n#\92309'\SO\20194L\990676\SO;)p \1099862\159925;q\1055626|\b\185256e:N:n\b\1003155\v\20572\FS!\46482\NUL=\143712\DLEVC\174446 \98625G\120660\174624B`\134648hNr\42790'\DELXe@\b\189939\1099842zbP\163148\NAKG\CANe+\SYN\1098544\RS\SO}\195010r-")}
testObject_TeamDeleteData_team_5 :: TeamDeleteData
testObject_TeamDeleteData_team_5 = TeamDeleteData {_tdAuthPassword = Just (PlainTextPassword "\SI\DC379\169968K\1013767\135970t\DC4\EM\ETXg\1092043A/\n39\1016325\NAK]K'\150758\1020273I\EOT\185455\148421X\131507q|\STX\SI\4949\DLE\t\ETB\1052749\ESC\1105451lJ@\65239H\1088285O\DLE Kw\985317,\177128J\177958xc\158910(\30994tm<F\131698N\63646\987175\DELzo\1059053nz\48022\42319\EM\\\1004742\14063\1043735)+\19558v00\SO\DC1mg\999383\US=3\ENQk+\1017025\1074527P0\ESCnpR\1098938\42809\SOHU\DC2\1049426c\14825\190327\181083\1004221Dzm#3\1060901n\DC1KZS\ACKhA\1083121\&1\EOT\146802\1103742\184120\67245\1030956U\12689%\a<\a\DLEzv>@`[r\NULm\265!4\DC3\ACK8\1012806n\1037850\GS\\\STXK\52053\&9\1104438\165207\1023640\niQqNk\DC1\DC4D\ETX\GSk\1102294_")}
