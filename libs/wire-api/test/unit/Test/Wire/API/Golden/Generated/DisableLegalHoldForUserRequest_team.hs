{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.DisableLegalHoldForUserRequest_team where

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
testObject_DisableLegalHoldForUserRequest_team_1 :: DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_team_1 = DisableLegalHoldForUserRequest {dlhfuPassword = Just (PlainTextPassword "<sL\31560\998796\SUB\b4eI\f\1076329Za\DC4!\1036656\NAK\\p\SUB$\SO\CAN\1091187\1035096\1034497`\tPl\DEL90\45548r\SO\181463>G;@\US}\ETB/N\74515\DC2H&Rnq8\FSO#\RSg\147769HBjVs\1048401f\ESCp\1090671\ETBX\990616=~\1284\1018850\17346Q\180517N3P\92723<P0H\NAKL\"\83289)^N\r\ACK\ENQjU5\f\1106432z^\1059503\r0h\FS\170446M\1086432\ar\\\ETX\DC3[\SOH\DLEShL0\1105813x\1015090=\n\173745\1070399\1085244\156805\17412\EOT\1026570\ESC\1054586H\NUL\151979CGt@6}3_=,9`r<a\1079095[\184621AY\989533\1011707s\EOT\NAK!+\181359\189744\164020\&48;\STXc0\29030#~\DLE\1067030\v\FSf\159790!0w\187338W\f?\1000868\1014178;dd\RS60\150831\1094248\ESC\"bdi\154963W\r+\1002256\v\t,+\159727\b\na\SO\GS!2\DELa<\1075808gh\SYNi\52665")}
testObject_DisableLegalHoldForUserRequest_team_2 :: DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_team_2 = DisableLegalHoldForUserRequest {dlhfuPassword = Just (PlainTextPassword "\STXL\CAN.{,\SUB\31256\&2!\32527\1025377\&4\74992Cb~\NAKGw\985579\n\ENQM\50788\&8\1035118\1044262\179842v\1041509'O/e\1073890\RS\60758-W>\SUB2m$Tw8N\SI\SYNCg\RS\1080152sP_\v\DC2*R\1054007mm\1072411O#u_`\RS\98816PwFP\SOH\35210k\1033085\&4(3V~im\NAK\DC4C\178782>`\1044208O\ETByJ\37513\20655\DC2\31543qg#?\178195i[^4(N\1096992\NUL]\1080351\188824( \9461t\RS+J\1074219me\DEL.\98423/\991397h\164261L'\t\166956Ks?\\:\29677lg\rs\1009451[l\SI\174907\&9\1057870Jg%E\r\ENQRi\SO}\EM:(\18719p\NAK&\ACK;2O^]\1076120\995122\ACKK\160727\&9IZp`c\FS$Y\165417\1028164ktw\23045}\149126>\1047648S4eS<@\59776s\t\98905\&3;JX\158919\&6aO'\r\EOTl\18222b\ESC\7800]\141761f\1086737\SUB\71443D\1064919^{E\\]UA1\n\1009819\&6\ETB\138778\EM\15325#\136607Ax\137130\ACK\FS\28658V\111272}(\185377?\"A7\1042411\1073051\&7V^\fjs9v\SIL\SOr\1109607\ENQ9\15966Yn\DLEB\1083531J\988883$\SUBQd\1052009\5452\DC3=\164807\DC3\DLEPpKU\SUB\143663\148208E\11497o)@nC\t&V\151146/\ESCm\\\137458\1044838\1058558\SIk\ajCx^O\1099869OS\178148X41\SO\4501\SI=\ACK!7y\NUL\tjb\1104751\DELi#\151886\US\33984iw\34183\CAN\152397\21134P\DC43\95156C~(Z3\CAN\NUL\34105F\11276^\148524;\v\t\b\f\NUL\GS18v@\EOT*B*-RF!\DC4{-\1000965\1054949\&72\rcb]A'\9236\SYNyxGKf\1070413\1019105\1103131\1000856\1089672\ETB\1088713\1105869Q\rK\119569\158631`\DC4NH\GS\35313\148604\&1\EM\134327\1110651smigC\SO\SO\1105564ff`B\158865<`BL>\SYN1\1042553\STX\51191B\10175r\998645@\ACK\1063322\FS_\69720\EMRCT\DC2N/[bhG24\RS\\\a=\ENQ\1060729wi3\1017282\171993\tl\190560\45694\SOH\182665\1020449yt\995078&)b\US61cE/\STXQl\SUB\983438\30249\&4Qv\168201\&8T\1012555\1007196bRMV!#d4\FSO~\NAKJL\58236\bu\ENQ\FS\74201M\a*n6\161972\997311\983810\US\NUL\176794w0$\7153=e8\1001989:v\ETBPh\65367\164946LU)\60553\1089724X\181314\6295p\134219\1042607y&\GS+7UF\RSc")}
testObject_DisableLegalHoldForUserRequest_team_3 :: DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_team_3 = DisableLegalHoldForUserRequest {dlhfuPassword = Nothing}
testObject_DisableLegalHoldForUserRequest_team_4 :: DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_team_4 = DisableLegalHoldForUserRequest {dlhfuPassword = Just (PlainTextPassword "\SUB\ESC\SUBRG-A\150549Q W\1093385|9>Y,\1004492&Z*mC\SIc\1059594u%#8\r\177344Q\1013685X\bl\"\1017219ym\EM\120012\183601H\SUB0h\ESC:\\\FS~\SUB\1014835\ENQ\ETX\1111142\1099620&\1001845&Wh\158643\188991b9'\NAK\164177h\US\25888\1042005\&0\DEL\985334\ESCE&\1066313\53290p\STXB\169590K;\1006952\DLE_m\1015602,?\171167\ESC\\_\1077601\&7n\aX\164128@\NUL\1088994 E{O\DC1\ENQ#):/PQAh\SOK\994968]\158800t\12978-@\99554\"\GS-oP\1015052\&1K\FS1N\ESCFR-%\nz\FSL\DLE\CAN\1092146\1011136\ACKI]\1043466I\1089213\1113842t;\26538_gLG\74316\ESC\52265\DLE\8071U\1006766\EOT0\31698RN\13422J,h\SI27?ln|\19867$\EOT?5|tr6l\40012\64428\159856c\1068170a\DC35DQ^\ETB\1029155T\983471\1109795\&1]\1110970B\1075379AGw;\CAN\r\1070540m\NUL#L`a\1070912e\154074M%=,\9639\45037\1057931w\94809'x\ESCI\136897\1059826\92257\119233H\ETX,[\SYN_\175642U1VFL\SO]\STX\1014350\1015470\1085570\1004970\167048\SUB*\FS8\r|0z\43609adm\FS\1102191\NAK\DC46?#_ts\f`\1064386{'l\169585O\ENQ\"j\1048288\&6\n\ETX\4604\147384\DLEWv~P\1033887\GS\"'\SUB6V`?K\NAK\f.;3uU\EOT^#G\1090050\1079999\97485w\1059535Z*\1001874\185226'\US\998113[.\155425\FS\121069\f68$$\1049575b8\186916-\n\1051342`\155915\&2NC\RS`\GS{\ESC\181103Ou\CANu6\120918zSJ1rp\EMZ\23097\170293%*c\984713\EOT\NAK\t\ETBVz?u\SOHu\1058935\DC3HI&X\NUL\1052541\ETBs\EM0OM\194980#\17551\RS\1041503\DC3\44232\&8\990142\1007235\1000880P\"&@]3BM\DC2C\1109530}\ETXK\DC1&_\159651\r\SI\ENQ\29081\52294H\74491C=\1103093\"\DC1\146254\ENQD\DC3\NAKK%\989896S\189327;9\1006665\&3+")}
testObject_DisableLegalHoldForUserRequest_team_5 :: DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_team_5 = DisableLegalHoldForUserRequest {dlhfuPassword = Just (PlainTextPassword "ITU\989941V\169776\t\176602\NAKi\\Ur1/j\101050\US#\94805U\39357\ACK\148277\&2\5683\SO\5243\SI!a\62508P\991491\1042612#\139698-\1106398ElL\DEL_PWb \1028866)dKjtP]\159978q\1053641\&7\173890$p^\SOHk\993397\&1P_Nafu\190177Rf\DC3BPp\986717a\131625)\SYN\1013923\&5^% \51382\&8$\ETX\178104\1014005\vhr+\DLE\121065^\EM*PA\1020372 2")}
