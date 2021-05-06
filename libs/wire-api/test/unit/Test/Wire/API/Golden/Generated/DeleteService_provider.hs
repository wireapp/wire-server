{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.DeleteService_provider where

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
testObject_DeleteService_provider_1 :: DeleteService
testObject_DeleteService_provider_1 = DeleteService {deleteServicePassword = (PlainTextPassword "\65810\1055428\FSL\ETX\RS \STXg?3\"h7\157866]\1012635t\SYN\1077472\1096140]u\DC1-\160083o\f0\1073423vV\1029070\SO5\27556i\EMb'\STX\59957m\ETX@S\1063093\12837\b\181108hMm_j[\ETXl,\ESC\SUBH\1034136<\1002004K*\fL#\27670\5277\DC1\DC2C\1050132\21708A\1066998\USYu6,\EM\94400hB\170463l\71705mg cNX`V\5280d\1021224\3350\1035808 4\985239.\1014476B-|\DC4ck\132017C\RS&\1038561 UP\SOHy\1047489\DC3h\1064639\23172fvpO\3986\FS\189569C\DC1\STX\NAK\ETBoQ\25929 \EMn=Rs\182188x2IJ\ESC8=?\50459\1055699x\ACK\190339l1\1043451.vR\1042366\188302C\DC1\1021118\RS\92278\12837\tnH\154022\STX\FSv\SIz\180700\NULxid\1043260\&2\"\NUL\21934  ?\1008347'/-\27372\&9%\ty'bJU\SYNc\DLE*\946`\n=\147412\r\ETX\SUB\v\155500H\1054416\n\238V\f\1002048pS\163481\NAK'MQq\b\62731\1092739\b\NULt\1016936@\SOs\RS\33482\343:nj\166832`\SIj\a\1097046\1071980A\DC1\1082399^o`\1100159#\ACK\RSwr\aG0g\ETBq2n^TyJ}&D\1103234e\DC4\US\ACK(-#\58697|5Oec\172522\138699nI\999479\&6\ACKjek;\1105260%\78401\NAK$;\SI\995361sS\ENQ\154873za\ESCI5I\6679\988361X\1025938\ESC;\DC1f\EM\26151\NAK\EM\f\137758\168113)\1057573\&66&&z\DEL&wT\989699\RS-2\SOH\NAK\aHcZ*\DLEp!5\SOH5cMzQ\DC197FM\131155\1062615>,Q\STXu?e\19849\SUBC\1094253F{7\ENQ(6\ETB\1074268xf\43176*\GS\ETXu\DELB\1080769\"\n}6\989863\&2\1025271\26455\1097354\169041$\95251z\995206\&5@\1022935\1099798S\rz/lw-k\NULc\133541M~.g\FS8\1033709\\\188480G\1100600\1066131|vS~\GSa\SO\1028133\SYN\1056164^P.#\174523`,A|\SO\49916\&0\"c\32519\96264\RS\182234J\167518Sj*^\182038#\NUL\999688\59374\167290u\ESC\96505MJ\170144\ACK.}\72806\DC3/ \\\96223\61742Bo\1093077s\SYN%\RS2b\DC2\64167:ZA6\47814\&9\1055392l)Re\1055808\ESC\1106272[2\a8]\141722\EMD\40163+\SI\SUBDw\SI`\1052280$\EOT\110847#pnj\1006249+\186465\173779?\\\1451Cs!i\24960\EM]?TC@S\94503`\49851L#\161707\SOP$r\984660.cOX\ACK^\1010876FC\r\183393\b\36662V\SOH\1013488q\SYN\1003544\DC1\5808G\28765vDp*\34426\165285eH,3#h\12884YO i^6\n\fQ^\179984a\DC3^v\DELN),?Te\t\139802m\NUL[= \EMS\t\DC4\1006229\&2&\b\DC4`}- `\38933\7838k/C\v;\133299\SYN\a\ETB\rV\DC2Hel\100687A\NUL\1086866@PD%\163073\&1\1088436X\988960Lq\NUL\DC3ew^Cw%=\DC4{=f\188669<X\47696\160293S\155629\&7\ETXhl\US\63431>p@\DC4\1065931\12684b\52352\ACKk\DC2$gN\vh\DC3@\DELw[\t")}
testObject_DeleteService_provider_2 :: DeleteService
testObject_DeleteService_provider_2 = DeleteService {deleteServicePassword = (PlainTextPassword "\127081I\853\1102160\183080\1092298\t\1087341\171101F\DELZ%Y\1037059\DC4\\lq\1045938\SOr\35487\\\72247[,a~G\162981V\nP[h\51359\b\NAK\155447\EM\"JP\152756r(\1077636n\174356\US\986817\1054357\996793\EMT\NAKl9\1003726fKC\158170gV\98527\f-G\992825?\FS\25053\vT\FS\GS6^7\170639+Vv\SUB\fA\n\2669\SIJ\13154\41095\1102004\DC3\b\1024393ij\1057460\EOTK\1063220X\ESCN\SOH\"\172207*r\DC3b*?5#\1067620I\STX\52936c%\ENQwg]vs\1092586n3\190863DQ\GS\175540I\GS0\1013412\149718rH\ACK!\DC3vnf\22257.-J'7\1078572g!\DC3/\f\ESC(klh\bC\137038\r:C\179691\998155L\GSC\1049789oH\ENQ(\366\SOHB[p\EM \1073124z\1063309 \34597A\145338W5s5\1050260\ETB5||<U\1078793\1008290\&52\68738yB%\NULB\142150\162135@\186506h\ESCf\1000982\NAK\SOHg\STX5\129463|\998975oOz\ENQt\US\1101345d\1056177\1015271\ETBG\a(!\16149K\65758\180970&#[=\1018715U\"\40375\f,M\v\1028535-\GSW\40425L\62969Aw37:\RS\SUBl\139500\&7\EMH\GS,\1030740c\ESC\SO[ut\42593")}
testObject_DeleteService_provider_3 :: DeleteService
testObject_DeleteService_provider_3 = DeleteService {deleteServicePassword = (PlainTextPassword "f\1090695\&9\NUL\987517t{*\74956$\1005503\10588\EOTZ\rSrY\63608\r\bw\177242\174267\"BD\182348\DC4\1028674\EOT(<\53008c9w\ETX\1103181t?\NAK2m-nh^\FSfKBs|a%\adR'\b\138313\1091519<x\985554\ETXqr*W\1007023\ETX\b`^^\190978\&4\67236\DELwP\EOT\EOT\1040394\64165\&6\1101134\GS\173732*T\DEL\EOT9\STXwt\52051(>3\99418O$8}*\1108337\SItH9\DC1\v^\f3\1018985\48943\STX[\154619bu6L`.j\DC3W\1051468ZF\1041443\EM\rG)k9\FS;\FSz\3083\&1\184058\DC4T\11688H\146832f\177863{S+\141850X\"m\147644IG7\US\DC4wjAe\1076737hiF8 D\f-0K<@2\64340\DLEVz\992563\1043162Q\33019g7\1015869{\1111215\ETXh:\1021411\23094\145182=j\\\141803so\22655\1097771\145631aqN\28035f{`\1026734r!I\RS4\t\ETB\1070807\185355\SII\SOH\b!]\EM\100620\EOT\FS\SYN\ETB\SUB\DLE")}
testObject_DeleteService_provider_4 :: DeleteService
testObject_DeleteService_provider_4 = DeleteService {deleteServicePassword = (PlainTextPassword "g\156694]}\ENQ\16216D\13734\DC1\1029617h[_N63tpq&\SYNu\DLE\DLE\SIMW\2739b")}
testObject_DeleteService_provider_5 :: DeleteService
testObject_DeleteService_provider_5 = DeleteService {deleteServicePassword = (PlainTextPassword "K(\1080236!;\53894\ESC\r\1019641n\"{POmL\1034035]0Z\"\NAKD_\45518I\12663d\ESC\94465%+|D")}
