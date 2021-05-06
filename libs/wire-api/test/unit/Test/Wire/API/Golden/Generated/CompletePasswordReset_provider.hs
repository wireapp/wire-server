{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.CompletePasswordReset_provider where

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
testObject_CompletePasswordReset_provider_1 :: CompletePasswordReset
testObject_CompletePasswordReset_provider_1 = CompletePasswordReset {cpwrKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("f3kQNyJoBGMySN=Ov30y")))))}, cpwrCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("6WPk5Zzt")))))}, cpwrPassword = (PlainTextPassword "\1010979\v\DC3\176498D\26654{H:s\150452\GSY&u{jg\DC3f\1046208M\ESC\1054769\11782NoRMqU&\ESCq5\DLE\177916Z\145257#|p\STX>YD>\152434T\73006\FS|\1050394\NUL\189879$D\142258b\DC4\ETX/\1026104\EM\173355\186704|\NULPr\146385\SOH\1025546\SI\\\GS\1092839\DC3U\SUB\1087343j\ETX\1036081\&1\ACK`i3&\154479b\1026460\FSCf\993539\1108958j\1071562a@V@f1YE\137650w\24063z\2125\ESCV$\987924<^\172888:\1038730\1019504\27909Mvxy\STX\1052270vsP\SUB5\6530\169145\44576Qj4\\k\DC3\159649\1047920E4\1047283S\SOH\DEL`\DEL/\SI\EOT\31720\132599\69915-\NAK\96088L\1007562r\SO!\156031&p\1066097\NAK\51758H6x'm}\CAN8\USH6\1086684v\156122\\V\n5D]\SYN)H\n?Wq\9370\tT5Jg9\12919\&2\DC1\1080972F\19297{8\ETB\1076815_%|\NAK\DEL6v\51509\999079g\NUL\37975\154163N\50713\&7\ETX)%mP\1039355\&4\1047516l\DLE\SO\GS7~\v9:bR\68413G\STX:H\989068\RSUK\DC1\54487\186401b!/Ae\f=I\136810T\191108ABG\DC3{E\1007982\990624\985692\&5!.\SOo\SYNUYv/}\1069194\1095771Er\SOH\161984bX`^\1041636\58486\DC4\r\35474g\181612M\134146\41101ZPJ#\ESC\183585W\a(\ESC\996142\SI\CAN\48643 \1098601\fk\40387\1063618\137661\50038\100226\25731:\RSL\f'}_f\DC1\\'>\r8E\1019964M\r\16473%Y)\1073865\ENQ}B\1024392\1080583(+\71685\rnHdY\DC2O\154998\GS\EM-\NUL\FS9J\67309P\DEL%J\v$<\DC3\1040301\121317JW\NAK<\ETBoh\SUB\1008655\993569\996897jZ{\"gq\188145\&6\FS\CAN\nNEPK#\CANPHsq^\140\166433\&7\"\1051082RV \CAN\CAN#\FSr\54047\DLE\1067351\&0o\tXx\1056136oD\FS_\DLEF\991266;\1009748\NAK^x2\r\SUB\1091867\EOT\40553\47952f\EM\15688")}
testObject_CompletePasswordReset_provider_2 :: CompletePasswordReset
testObject_CompletePasswordReset_provider_2 = CompletePasswordReset {cpwrKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("xxhcg_6=uzDhy97M4xAC")))))}, cpwrCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("zm6rBUe_S0NT")))))}, cpwrPassword = (PlainTextPassword "7U\SUB<Cf\147303F\SOH\29534\&4\37112\EM\1068893\1056990\DC4\EM}3$C\857m>\v_\119158b>\1000910,\v\ACKI\97108r\1060812\1051449r!R\25165B\ACK\v\1042520s\ENQ\ETBM\EM+9\a\186803\185566\FSD\NAKQf\SUB\168127%\DC25Og,\191099Af2u}\55252r1AX>7\21511\DC4\EM\153592\DC3n\54846\1078427ya`+Q{\SOS\ESC\aBO\996139OX\94873&wq\1002876\18730\157469\1064097#\174853\986075\FS[\1020382Lq\21713CMP\fh\DC4\"c\SI\GS>\1036407\1107057(dNG\1086441\996111#z\123208Z\1090209\NUL1\STX\SYN\SOHgSpB\1089796X\b\n8xD\a\1070725\&0\95036\DELD&\1002398\1112526~\189138<c[DI\n[z\SOk\67270}x\DLE\191\78089\1020422\DC3B\1042968\CANy\1032791^E#\ENQ\SYNF03\62337\&5\166597\\\1094142L.%\13908X\DC2\b9^\DC3\DELoq\121512h\NUL,3%{='\179268?\ESC\1044915$\1065320.d~!\aW[\SI\53369\ETB6X\183874\39533GI\1090472l\nB\1037950\\q@xCG\RS}\11199\&8\163557+Yz\GSd\DC1\DC3nu\SUB\1038718x\172043+B[pGiV\1008205\39670yI\1085107M\31931\1055823NU\SOH.\1016762!w>\DC1R64\FS*%O,\992182\1018923\DC3W\1066467R\62115$LX\DC2jH/\DC3\127257${\1007740G2(\DEL\153873\131764\1012522\&1/>Q\46782\1091603\1002649jQ\SYN`\1099086\ACK+w\EM\1078242\DC3\NULe`\tjJ\NAK\SUBm\1087160CTEeF\rLsw\STX *\ETX/\177807=4q\1073807}jvC4\186327\1053163\GS~/!h!\ACK\"x;hY\1110863I\994546heN\CAN\1024312\12958\RS\NUL3\CAN\27618\118900\SYN\188137VhV\DC3\1060608VQ\CAN,I\b\DLE%s\1044289\USe\v\EMC,\DC2\\\NULq\DLE\DC4QVmZb\142274\161772C9\83282\38339kMHb\\\DC3=8%\SO\1066954e+T\1090509\1099363~\ETXF\19206J\146649\&5\1017975tI5a\US\74928\\r/\GS'au\f\150560Yo:U(\aN%r{\SO\SI?\SI*^\b1q\DC1[Y\179522z\SUB\NAK2'\4117\1111298&o,\ETXHi\SYN/\STX\1099573\157832\1090205|yu\134861#\1031240\1007537j\ACKD\17600i}\DLE\1010550p\137659\&2\174610\SO\\\36364\1014161\&5e\ESC~\129024\FSIa\EOTXor\SOH\ENQ\1093743\SUB\165263\993842\NAK\STXi\161846\54859\&0k\159020\SYN\46551\51768\148446M!\95739O]}P9\66874\DELZ@\ACKNgdU\rD\143210\&4\1025919\174384`x\"92\182433\991830Nz\t\1081916\&1j\\TofJ\NAK\ETB\40876\25153[XS\1090552\1028612\SO@")}
testObject_CompletePasswordReset_provider_3 :: CompletePasswordReset
testObject_CompletePasswordReset_provider_3 = CompletePasswordReset {cpwrKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("4Wg3MuuT2sd1fiOQs3vc")))))}, cpwrCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("GokQp5NAr1KKRLQkI")))))}, cpwrPassword = (PlainTextPassword "^ZbZ\78485\41476\2318]\1061539\&4\54571\12822\1042745XUv~z\EOTk\159391|f$\157997Fq\987716Nw\183766WO_\1072202`\GSu\1009279w\1045873>\1001180l\25188\ETBx\133701:\27850\164903~\149685\1026585{.\aAW\140635\ETB\r=\ETBR;^\18779;\SOH\NUL\145877\&0\NUL_` [{\ETB\b;\SO`og}>D\ENQv\SUB5z\DLE\155573H\1019395Z\183808\98737\&3\150078\1103818\&7T\SYNv^K~>\1044408E% }S\"2\1060075\&9\STXCC\DLE$\SI\1107058\1005704Ey\b&\1062639\DC2&V\156269\SIs\1066084$3\1034202\ENQD\r)X^F\SI_\SUB\SI\1088637'\1087676\&4K\t%A\989869n\4386y\1089748^\SUB15p\n\101029\171168;N(^5\181174n\45701[\92221\ETX\989211?'\1102324\997858\DC2)m^)\ESC\CANz'z\176813' J5\SYNM[ \1112527r\DC2\20495\&4\1047056#\b\46203=\1073943t\DLE>1K]\GSN>\STX\SOHMp")}
testObject_CompletePasswordReset_provider_4 :: CompletePasswordReset
testObject_CompletePasswordReset_provider_4 = CompletePasswordReset {cpwrKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("pIISmT4nBTzXQY3c6zcU")))))}, cpwrCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("bfVl6m9")))))}, cpwrPassword = (PlainTextPassword "2\STX&[60iq^\1074434\ETX")}
testObject_CompletePasswordReset_provider_5 :: CompletePasswordReset
testObject_CompletePasswordReset_provider_5 = CompletePasswordReset {cpwrKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("9mebn62YYe3rc=Gt2ATX")))))}, cpwrCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("NUmKrbDBArCVMmkCt=B")))))}, cpwrPassword = (PlainTextPassword "\127980\t\99227\DC2\142083t!\SUB\1077123n\1102133z\6108\1110131\1041405T\1015604>=y9w4\ETBW[\ACKiq\98868o\DLE%*^j\1075893aW\154597\r%\34242\1012215\139035x#\156023tJ\CANR~&\35445\SOH\NULG\83114(\170194\NUL\186279n{+\DC1\65565\32824CH6__\tkr5\NAKp\134291. \29474r(K\v[+-N\DC4<\b\39770\\l@\DC4W\1092014a\SUBd\CAN*\ESCx~]$Q\183329\&5E\1096092[^\EOTr\158440\1065695\157775\GSt\60380\SOHG\ETB{!l{K)\996444v\1099139\STX\1028998\1056021\41258\&9\134069Kf(\b\47069\US&U\1009712\1054055[*~vt\DC2\22364*h\ETXJ\189267{\50760\r8c\"d1D\11585d\143800\1063255iSNk\38608 WrZ\t\1078417\DLED\28800\ESCa\182811h#\160082\146761\1106089C\1045972\1016665M\DC3:1\DC2?V\20300\DC4S\1011753k\1074261\DC1C\RS\123616\1017104\1108771\95707\v\1107411}\DC4\EMfI\3501\ETXNAO~!2\137409\8536I4`s\ENQeAL\99713D1D_m/j\SOb\SUB&\35108\DC2&$Xu\27032\SYN\998091\1023371\&7\DC26\993079\&7\999699ZJ\167333d^\ACK\165812e}\NAKa\"\ACK\STX\ETX\1085018\DC2`\1080547\1033344gX#x\STXH\1065707R+\51051i at^\SI\\f}Gp(c\1099009\v'\\8\f.\1052060n\NULOn\1020150fdV\1039847\n\RS4\50213\47638\1085771un\a\GSimI\STXp\DELReTd\1113949\CAN@w)7x;/\\\DC2\1074625Ura\v3*\NUL\1001706(Ug4\DC2\EM\FS8a\44906\&27\b\50420\27261\EOT\\\ENQ\100677!\DC2b\fuo\41918T)\SUBZ\DC2H'l\EOT\5325O\DEL\EMn")}
