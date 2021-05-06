{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.InvitationRequest_team where

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
testObject_InvitationRequest_team_1 :: InvitationRequest
testObject_InvitationRequest_team_1 = InvitationRequest {irLocale = Nothing, irRole = Just RoleExternalPartner, irInviteeName = Just (Name {fromName = "\1035135*1GH\59341\1104982\991811@\ESC\134608SL.0\1085478$\NAKZ\17348y\180088-\ENQe\35592F\ACKHwNs:GQ\ACKX\ENQz\40095\STX1\480B\96082g%\62767\SYN\SI\1084065\1052244\\f\110667\1607\1007251o\SObZ\nK$`A\DC3JT\bK5bu\16120#\NUL\DC1\137923\ESC\t\710\&7Q\DC3f\147712n\1063295#{iC\DC1"}), irInviteeEmail = Email {emailLocal = "L", emailDomain = ""}, irInviteePhone = Just (Phone {fromPhone = "+41756354628933"})}
testObject_InvitationRequest_team_2 :: InvitationRequest
testObject_InvitationRequest_team_2 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KN, lCountry = Just (Country {fromCountry = AE})}), irRole = Just RoleOwner, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "", emailDomain = "7"}, irInviteePhone = Just (Phone {fromPhone = "+745834292162"})}
testObject_InvitationRequest_team_3 :: InvitationRequest
testObject_InvitationRequest_team_3 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.IO, lCountry = Nothing}), irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = "Q|\vvu_\1040060\&2\31213~^V.g@\GS\"\DC4!MsJ\FS4pyN\1000598\"\SIk\EM\16219\1034044<dt+\1065102a{B/5\995060]=\1039148\1073666\&0//\160105?\1007950e\161382\b_et-%\b_\EMMS~\thDi{\\\49982/\149852\DC2\1086780P\SUB\1109490$98,n\166535\1075019vx`\1018337\ETB@\994182"}), irInviteeEmail = Email {emailLocal = "", emailDomain = "v:Z?\DC3\1014212"}, irInviteePhone = Just (Phone {fromPhone = "+8158795496813"})}
testObject_InvitationRequest_team_4 :: InvitationRequest
testObject_InvitationRequest_team_4 = InvitationRequest {irLocale = Nothing, irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = "\177524g\987227@m ^\1095085w\1005711A\EM\"\97268O^L4\65242\RS8\63663\ACK\STX\RSA\DC2\190645o\94600qJ\1014423\nI\164698\1017604\SOH\ETB\ACKCF\1086284+\ETB0\23646\EOT\rsyX\180654w\n\17921C\ETX\152305l\NUL\vi\EOT\174949\v\RS\SO2k`\NAKSmMU\EOTz\19592\&6\\B\CANvj}"}), irInviteeEmail = Email {emailLocal = "T", emailDomain = "\1016896RV*\DC2"}, irInviteePhone = Just (Phone {fromPhone = "+540825211569"})}
testObject_InvitationRequest_team_5 :: InvitationRequest
testObject_InvitationRequest_team_5 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.EL, lCountry = Nothing}), irRole = Just RoleExternalPartner, irInviteeName = Just (Name {fromName = "E\DC2\US\1038271"}), irInviteeEmail = Email {emailLocal = "", emailDomain = "\f"}, irInviteePhone = Just (Phone {fromPhone = "+99106102063"})}
testObject_InvitationRequest_team_6 :: InvitationRequest
testObject_InvitationRequest_team_6 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KY, lCountry = Just (Country {fromCountry = LK})}), irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "\1082001\rvgd\vo\\"}), irInviteeEmail = Email {emailLocal = "Q{[e", emailDomain = ""}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_7 :: InvitationRequest
testObject_InvitationRequest_team_7 = InvitationRequest {irLocale = Nothing, irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "\1086309\ETBZ\r\1003794\184200:\77873*p\nGD\140473\70702\187009qrQ\ESCHw7\aI.5\999805Ql3~\163684\178484Zg-\1023364\1100938kv_r?\97967\b\STXG@\ESCA'\1084698F4\1050176C\1025882,\32633\149504\1016270TX\DLEq\67136@#\1056080T}:K\STX1\n\ETBT\tZV\1073814Et\ETBo\1018170\SO\DLE>\16942\990982\24480\SYN"}), irInviteeEmail = Email {emailLocal = "%\FSmN\1000417", emailDomain = "\SOHN"}, irInviteePhone = Just (Phone {fromPhone = "+203595686"})}
testObject_InvitationRequest_team_8 :: InvitationRequest
testObject_InvitationRequest_team_8 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.PL, lCountry = Just (Country {fromCountry = MN})}), irRole = Just RoleMember, irInviteeName = Just (Name {fromName = "r;0DZxe/>O@N\178270UK\"a\162320L\GS\CAN\ETB-\989126w]!\RS\t\DC4\FS\26087\DC1\1024202\ENQ"}), irInviteeEmail = Email {emailLocal = "5", emailDomain = "o\991152"}, irInviteePhone = Just (Phone {fromPhone = "+124045881"})}
testObject_InvitationRequest_team_9 :: InvitationRequest
testObject_InvitationRequest_team_9 = InvitationRequest {irLocale = Nothing, irRole = Just RoleExternalPartner, irInviteeName = Just (Name {fromName = "H\US\DLE~z\1023326/19R2\n\RS\1110185\165151o\143736 8q&0\SI\GSYF6G!X\nQ pfV}\DC4-l\GS!\CAN\ETX\39478.\1006697G&uBr^J/\133050 \19002\100856\149672Av\14043\179125k:;)'@\\|\1051457\&4B-~T\SOA_f\164931Z\ACK9y6,]"}), irInviteeEmail = Email {emailLocal = "\f\1002825\&0n\993694\1070539", emailDomain = "\nd\1069849\&8"}, irInviteePhone = Just (Phone {fromPhone = "+04863690542"})}
testObject_InvitationRequest_team_10 :: InvitationRequest
testObject_InvitationRequest_team_10 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.ES, lCountry = Just (Country {fromCountry = GP})}), irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "\171070\1110253WP\1065892\ESCQ\DC2j\1095686\&3O\1100172\ETB\1107902\&80+8\SYN\CAN\1087316{\160982|$!\ACKb\17387my+'y4z\DC4\34034jF\95515\&6\ACK\1064941\1051349\"\a\US\NUL`\SYN\NAKf\1056185\999944\ETBBBOIK\147958c'\136918\131483\&1\STX\v\23906\159134\34066| ("}), irInviteeEmail = Email {emailLocal = "\t,:", emailDomain = ""}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_11 :: InvitationRequest
testObject_InvitationRequest_team_11 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.XH, lCountry = Just (Country {fromCountry = VG})}), irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = "\NULQu\1012951!C'\ENQJ(\ACKE:\1065555V.8-\NUL\98191\1013099\1060509\6308Qo3\SOB]\1047336\16401C\ETB\n\DELM\SYNBn\1086235_\1058545\1024237\96498'yK\1071066\STXYr\1000676@\1076843\SUBg\STX"}), irInviteeEmail = Email {emailLocal = "\161165\DC1", emailDomain = "\137513J0}"}, irInviteePhone = Just (Phone {fromPhone = "+992486850"})}
testObject_InvitationRequest_team_12 :: InvitationRequest
testObject_InvitationRequest_team_12 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BE, lCountry = Just (Country {fromCountry = PY})}), irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "\1000448f9\145355vU\SOW\1020633Z\\\134258n_\1058173-O:>\1011084\&8n\176838\ETX\1103669n\149474%cQ\ETX)\DEL\EM\b_H2\1105562 ]u\1032254q-O\65842c\1068147\39005T\DC2(\1039052\30540\&8G\1087896Z6\ETB\1113866#\SI\83000\35328\1019341wVu\152535Q/?\119203\5238CR\7576,M\n"}), irInviteeEmail = Email {emailLocal = "Y\167740", emailDomain = ""}, irInviteePhone = Just (Phone {fromPhone = "+430229034499"})}
testObject_InvitationRequest_team_13 :: InvitationRequest
testObject_InvitationRequest_team_13 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TR, lCountry = Just (Country {fromCountry = MQ})}), irRole = Just RoleOwner, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "\a\1088248a:\DLE\r", emailDomain = "\1083468\160306L.M>"}, irInviteePhone = Just (Phone {fromPhone = "+38821910602993"})}
testObject_InvitationRequest_team_14 :: InvitationRequest
testObject_InvitationRequest_team_14 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SV, lCountry = Just (Country {fromCountry = AU})}), irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "\1057469\ESCG\1106249\SOHr\150041\985481\SI\146870[m\1099979\&08fWt\166536X\1079489i'\141888\189544>\1087589?\EM\52936\&621\97545D\SO\1005372\f\DLE\1103955{CW\ACKzP\ESCcS@F\a\1085242=\1089594c\DC1xv\35477\&1\DELy2,G\1049197V\GSO\29059(^\ETX\DLE\DC2GL\DELHm\NULnz\995665\ENQ^\1072322E\10085\1098879\DC3oV#9v:\9489n|\984550\ACK?\10714\1071487\SYN\DC1;O\128091s\1075458\&3I"}), irInviteeEmail = Email {emailLocal = "\169445fGm(\a", emailDomain = "S\ETB"}, irInviteePhone = Just (Phone {fromPhone = "+91194439"})}
testObject_InvitationRequest_team_15 :: InvitationRequest
testObject_InvitationRequest_team_15 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.RN, lCountry = Just (Country {fromCountry = LY})}), irRole = Just RoleMember, irInviteeName = Just (Name {fromName = "\fE\63923\1019895r\ETB;\fK41O\DC2\b\71189\bV\"U1fA\993958+\992957\&9i\ESC~Thf~Q7Eg \1041943\v\1101746 ix\1080193\31633\n\US\ETX\35260$nu\EM\GS\US\DC1\a\1081877\&1\992408v9\3404O\1032397Y1\RS\1062902\&8d6\23293P\ESCRU\44720D\EOT\164290O\51151(\GS\FS@\157565\1093479\NAKb>\SYN\119061RO"}), irInviteeEmail = Email {emailLocal = "q\78509", emailDomain = "\150605"}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_16 :: InvitationRequest
testObject_InvitationRequest_team_16 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.AR, lCountry = Just (Country {fromCountry = BS})}), irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = " \120698h50:LW\GSm\179053t\r\t6\ETB\1073780\138148a$\985342+(\DC3b\1013701\43764\153919i` \SUB\FSi\NAK\917831\&9Ke\a\b\1028789\"{M\161990S\ENQ\\~\SOHX75\1016830[\31245U\96717UF1\DC4\98458\25383z\SIo\142308^\CAN\CAN9\ETB\SOH\RSz"}), irInviteeEmail = Email {emailLocal = "ed1", emailDomain = "_k\DEL\DEL\1052001}"}, irInviteePhone = Just (Phone {fromPhone = "+5200460769"})}
testObject_InvitationRequest_team_17 :: InvitationRequest
testObject_InvitationRequest_team_17 = InvitationRequest {irLocale = Nothing, irRole = Just RoleMember, irInviteeName = Just (Name {fromName = "\US7n\SUBv}i\985869\68879\DC3}\18416)[\1030580>rl\US\166542a\43605\&4DVQ-?QC\47704Yhd\1084490@[@r\69637\"\USfPC\SYNBt\1018159\170725l5r\CANBH\37763!\GS\120117\1000543\ETX>DX\EMl\171896wSw\bz\SO]W\DLE[\FS\ETXLt;\1014084a\ETB\STXRF\19477\n\1015654G\78312|\STX@3SX\30719\&6rz\175882.\31837K\1013431s\SYN\DEL!\98361\165388a\144889c\1084969+"}), irInviteeEmail = Email {emailLocal = "", emailDomain = "\SOH\EOT"}, irInviteePhone = Just (Phone {fromPhone = "+4402318506166"})}
testObject_InvitationRequest_team_18 :: InvitationRequest
testObject_InvitationRequest_team_18 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.HO, lCountry = Just (Country {fromCountry = AW})}), irRole = Just RoleExternalPartner, irInviteeName = Just (Name {fromName = "\1031899nH\63742t,\1105745?c`}[ll\SUBa\t(\998700\EM\a\1034781\&6\SYN-Eo~\DLEI\1032087].\1103229i\f\1090701<\DC1\EOT\USC\ETXM&U\NUL7\57599\DC1\46086\&3&,-ltN\188898)\DC4C\1011218\1015322\DC1GGnx]\EM42=Nl\SO]\NAK\176750\f\1044497\1082730:R\998707\73824\986B\1009319Pm\42850\142342:1Z0\1004131\58875\GS"}), irInviteeEmail = Email {emailLocal = "", emailDomain = "\ACK7x\49754"}, irInviteePhone = Just (Phone {fromPhone = "+5485210093441"})}
testObject_InvitationRequest_team_19 :: InvitationRequest
testObject_InvitationRequest_team_19 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.RM, lCountry = Just (Country {fromCountry = AS})}), irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = "\ryt\1097515\DEL\189048\1052846\1064596a$\163950\119026^Xg\623t;{%`\GS\aj:G\1084889\158570AD\ETB`\1005006Z5\182751\1031555w&#\166608X\19976\ETB\STXl\f\1654\ETB\1006008gM\GSJl8cqp9\28570m\ns"}), irInviteeEmail = Email {emailLocal = "\ETB\1089326\998117\&5\SUB", emailDomain = "\DC4"}, irInviteePhone = Just (Phone {fromPhone = "+24086810667763"})}
testObject_InvitationRequest_team_20 :: InvitationRequest
testObject_InvitationRequest_team_20 = InvitationRequest {irLocale = Nothing, irRole = Just RoleAdmin, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "O\1036245+\15662", emailDomain = "\1106280cWq\28073^"}, irInviteePhone = Just (Phone {fromPhone = "+5566987538804"})}
