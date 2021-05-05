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
testObject_DeleteService_1 :: DeleteService
testObject_DeleteService_1 = DeleteService {deleteServicePassword = (PlainTextPassword "\162328\28623\1022780W\tX\a\1038506\21375\NUL\USFz'\DC3V.ouM\59634\64679Ka\RSl+8\1042001\DC35#\ni\ENQ\SYNNt\190246A^\NUL\nr\DC3$\47694\22385\DC1\n\71715?\5903\&8\1029069[\\_Mih\178258\150596eC*\59530\1054438n\1060512Kg\31314q\14682+\100132C.o*K|#jMkS|\DC1\1073505e'I\ENQa.\a\43584\157300i\58758 );{1{\166878\\\1057587'4\n\174240*\41997\ESC/\1112758\189237\DC3\1015767\GS/\45653\t\DC3\ENQ\174809B$,[\161413b\1066299|>RU7DBo&qVe\1032968e\SI\STXA\EMU\1099565\1058727\DC3OZ\b\1042218\187687jX{J\\Vj\DC3\1072336\"\DC2Bg\f`\1105701\1035081'2\146039\137850NKw\1077894fS\ETBoj4h\SI\DLEK\1053679\&8q\26912]\n?y\66769\190899CY\DC2\1074330\DLE\bG zc\ETB\5746\1094278\159159}\b\127469\b\\\93009kwz\US\1097156Xd}k\181866w\EOT%Q\174795Fp\62111V\98747B\ETB\168962\&4u\161650\1060491\14844,f/v\992997?\1079343i-\t\1113781 \55188\1040858 \162935eo\STX\37420\DC3]QF\162120\95920\&8\119912\RS\1049840@\58993`\1101148\25350\DEL}\1033720^\DC3\DC4~pZv+X.\DLE7>\US\62056d\1095806\64151\999672BW\ETXL\RSy|\5771 \133341\b\1109137\137610\r\b\180772];/\DC3O\ACK\1095662s\1040184\16785d%\16121\DC3;\EOT\185380V\161410/_\44697\&0\SOHL)v\b\20710\&0u\EOTH>x)\SOHB\SYN\1085239O\DC3J\SI_M\\\RSl\1037138oY\32346\92783\SOk=\165769\186496Z\b=\"\bi3\1096774|=kL\1025004iu\"V\141532\1084863_%5@\DC1\ETBSJ\142938\1005310\SOHc\146505>\a \189940\146108\125056ew8!\157147\1014528Ob\1054561MK\DC3I\"mr\10520\RS\1017017\988883\"f\t8\SUB{\1046013@&\1009335_\159286\1098335p2\92617\DC4\148711&\1083105Lr\ETB]\148703\DLE,tb\145118\13435w\ty\EM\995424M\997344z\1027931\30833Lp\1001662\16582\f+\ESCt\1028142\&2\148731<`JkT\1103619!1\1086559^[{")}
testObject_DeleteService_2 :: DeleteService
testObject_DeleteService_2 = DeleteService {deleteServicePassword = (PlainTextPassword "\140744\1014744'\DC1\99534\DC2\NUL\985758m\ETXQsl\1042238-\143053v\DLEF\1100319U\ACK\1088279&Ay\45222Vkz\1057675\65070\GSX\ACKk9+O-\46190WSVj\STXKs\1018675X\DC2E\146455\1015238\"\NUL\42325\92305\54512\9345\r\1091865\NAKTx\54969?$\78215\&1\ACKM@\DC1{Z\"[Di\62486.%\RS\DLEtUa\1086325j\ENQ;\36539]\168705\b\EOT\SO=%x!\b\" \1012882\SO\a_?@\100869m\SUB\r\184081g\53218'*}f\985628&(\STXk\USm\ENQKWg\DC16T\f8#\v=\FSS\92250B\DC4\169194lD\GS\DC3\ENQzV\1034393\1042753b>9\1094637'C=\bE$R\1093291\1090922\1065152i4\1037109\&4QcR\160297)D\1011103X\1018803Y\1002314B\182275\ACK\60389eP#\STX\142485n\SUB\1059173\&8i?qUiK\26645,]$9 \\9z26~*\SUB0\DLEL>\189039D\1051792`\US\61582\ETX\995013\1034346K_KD\ESC\152779z\1110410cm\1069761")}
testObject_DeleteService_3 :: DeleteService
testObject_DeleteService_3 = DeleteService {deleteServicePassword = (PlainTextPassword "|NF\NAKD\NULd_\GS3~\47601\1053829~\DEL_;\SUB\1104683\29440\SOnqIC-\CAN>\ENQDa\SYNaV\SYN\1051576NF^P$z4\1084556\&9\DC3~i\1016042\45982\FS\FS0G\DC2<\187507}8}\1057353k~\137331\&14\917588%3ExpY4SE\5248\68747^\"\1091246\a>\998207\4920do\SYN\\0~\153516\SOX+P\STX\t\30716\63024ipjvum\EOT\37287Y\SOH\RS0\187634`@\DLE\1061491r]s\1106240\68291i+]>g\1039694\"?\28637\1049485\1103669\SYN\1050302\10571`\15600\40752_\SO\132681\62148\DC3u. y\1089425_3")}
testObject_DeleteService_4 :: DeleteService
testObject_DeleteService_4 = DeleteService {deleteServicePassword = (PlainTextPassword "l\96163\1013858\1052099\&5\STXsv`\30110E_V\1026204J\1039653\126257Z\DC4{U\139809\ACK\1078896\1040555@pd\"z=X)n\73755UmB1w|\188513B\111212Bsp\147620,XP\1084107RP\41136\173012AR?R\f\n}d\r_\1059845C\147500u56[+\1030893#\28625fa\1078261\190284H\1011030d\1004108fIQ\SOHWh9\144902|}NP\ak\fn\t\96519m\DC3\ETBd\14955{/")}
testObject_DeleteService_5 :: DeleteService
testObject_DeleteService_5 = DeleteService {deleteServicePassword = (PlainTextPassword "2\99413F\DC1\"\DLE?s'\1064591\a2@\v/Mg\GS7Ng\1092270Tb8\GS\155960\&3\STX\1009989\f\ESCG4\CAN\b\1107903x\1075934")}
