{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Event_team where

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
testObject_Event_team_1 :: Event
testObject_Event_team_1 = (newEvent (TeamUpdate) ((Id (fromJust (UUID.fromString "00004bfa-0000-716f-0000-11b000001343")))) (read ("1864-04-23 17:49:30.529775870163 UTC")) & eventData .~ (Just (EdTeamUpdate (TeamUpdateData {_nameUpdate = Just (unsafeRange ("\17787u\ENQ\1051768v~\ETB\166086\1022227hz\1064584Q\1098691[\ESCap#)\1109343%\1080124\DC3\185693P\168731@UB|iJb\STX\28824\1049915_\163972\1087351%\25432Dc\1069473G\1027815\DC3#Qum\1075119@)|\1069542M\DC27M\NAKVTE\DC4v\DC2U\1045090d\127162\1091582\1047559\US@nR:\183166#T@\DC3k\ETX\1102587g")), _iconUpdate = Just (unsafeRange ("\148984\SOu.S\DC4\ESCu4\DLE\145897I\STX8V\7728@jj\180873b;\998675\RS\1039444\ETBOdk\US}\DLEuG\ETX|3j\11145\1036268PZ\1062972r\159752\DC3\r]\1058506x)W\n\997931Go Y\33830\&1G\ACK\\;4\SOX^i9nv,g\ETX\CAN1\45186}\38394\994868\SOW\57609;p\194839\SOHj\DC1\36822a\139623\&7\191125,\174929\135742i\1102977=4o|F\DLE3\DLEP\29780\US>r\rNVb*:\65325\a\1051931\a\DC1p Dw\DC4:.S\52103\DC28&\f\195014\ENQ*\180824 &\1035169\SO*CN`c%t>\t\f,K3s3/\SI\1077098}]]\128681\DC3tOB>\RS;%F\EOT\1052210\&27q$\1024870o5n\DEL?t\RS\SIs\42087,2/,e\ETX$i\bzM=Kj\SYN{cwl\59533<>\SIMO\1096197\44873k\1088334\97433kpU\189885e\994872")), _iconKeyUpdate = Just (unsafeRange ("\1060961\71887\tUi\1021746\f9}-\DC460%4J=\STX$3\3970k\1023155!\100001\1035847E\SOH\1069822\989622\SYNc\1085161$PG0`R\1055631hj\fz|K\EOT\b\18624L8\22548S\987188\ESC1\1058090UBA\SYN\36467N\EM\"\32548D\24354q\1093351i\SOH\985984\163279k2\381#\92955H\22057\1095931\1101745}\",_^\74250\b\r{v\1031931\999634\1058288'\187479\EM\1043720\DC3\183239\983499@g*\1010453`\1092083\SIi\1062608\1042857\n\63030#)\1049971\18364\ESC{S%\1045327B%\EM\STXq~SN\EM\1097823\136667-\8581\78412\1038136\94507'\155366{F"))}))))
testObject_Event_team_2 :: Event
testObject_Event_team_2 = (newEvent (ConvCreate) ((Id (fromJust (UUID.fromString "00004e4e-0000-2b95-0000-276f00002bbe")))) (read ("1864-04-26 05:02:40.338587548991 UTC")) & eventData .~ (Just (EdConvCreate (Id (fromJust (UUID.fromString "00007a06-0000-6e24-0000-2cbe00002fd0"))))))
testObject_Event_team_3 :: Event
testObject_Event_team_3 = (newEvent (ConvDelete) ((Id (fromJust (UUID.fromString "00006ff0-0000-7246-0000-3f7b000000f3")))) (read ("1864-05-23 09:50:43.162036420981 UTC")) & eventData .~ (Just (EdConvDelete (Id (fromJust (UUID.fromString "000057eb-0000-0f65-0000-546500004c33"))))))
testObject_Event_team_4 :: Event
testObject_Event_team_4 = (newEvent (TeamUpdate) ((Id (fromJust (UUID.fromString "0000405c-0000-43a8-0000-53570000769b")))) (read ("1864-04-11 22:33:31.938076321574 UTC")) & eventData .~ (Just (EdTeamUpdate (TeamUpdateData {_nameUpdate = Just (unsafeRange ("+1ity*\DC1")), _iconUpdate = Nothing, _iconKeyUpdate = Nothing}))))
testObject_Event_team_5 :: Event
testObject_Event_team_5 = (newEvent (TeamUpdate) ((Id (fromJust (UUID.fromString "00005699-0000-37aa-0000-5aff00001f6c")))) (read ("1864-05-02 22:46:22.304699165004 UTC")) & eventData .~ (Just (EdTeamUpdate (TeamUpdateData {_nameUpdate = Just (unsafeRange ("\\\175897\&4\FS!\20639\4497[F\136674Y\DC3$\DC2\15782Q\CAN\1107636zX\DC2%\35011S\1080981\bbOw\vB;\SON\96483\GS\fT>\129473,WB\54284O\8915\20179\1099588\1058846\SUBgqBx<\989859\154070y6L+\NULe\69739\&5Jng\179120\ESC6$\b\129601\1079135\1023395\SOHX\SUB\SYNYS>u\1086038\EOT\26164x\62547%\ESCAO\137306\f+\EM\25504\EOT\EM[\EM\11978\1020469`!gD\60259e\94313\&83\1007240\177576f\987282\ACK\73686t\2398h\988131>18\173924\FS\99720\992997w!h\NUL\DLEO\1062891\&6%V\STX\1105813[j+FP\NUL\ETB\GS\165517.\1082326F\SOH\DC3")), _iconUpdate = Just (unsafeRange ("?\NAKg\1054552)\ETX&\ESC\1104352yF\999382\t\1086321\1022622\94629m\ACK1\110760eO)kT,\ETB\95461\1016483fNe\r\1053957I\139906q\NULs\1073624\1090085Rs\12646^\RS\142105\1015355E(\1088250\150707;rG\NAK\CANY_\ETBK\CAN\1109726\rL\145488l\NAK\28733A!\ETB<G\47796G\"w\1074811`z\1040836\168467}\1044791\1051662h{f\65862\&4+^\988868\&8y\1110891\SO6\147957\1046608@)KJ/ryA\"\1078065_\ENQ9\SOHj5\DC2 <\1060505\51493\1097048HyU\DC4\1222n\STX\1010938\1021953vtB}\151909wtH\vyC9Zc\SYN)\10561\156573\1059835\1056098\998770\&4\NUL\67664\DC3\1060933\DC4{z[Ov\166262\b2\DC3\SO\SI@\759\1071604\\/NS_M3\1108592\31018\132035\&9i \US~RTv!\SYN\DC2\36582\RS\CAN9}\DC1\DC4\FSk\nY\t\t\177433{\US\186015\SYN\137475~\1093159g\1057819h\1030813\DC4/3L\DC2>F\fU\14680\GS\1073475O\DLE")), _iconKeyUpdate = Just (unsafeRange ("\\;\1109852\ESC\1053004\36980?\986735\1075999(J\SUBM?\SYN\990547\STX\EM\1013483\1093289\993728/\r9>M\f\134335=;+0\DLE%\1017791R7'\1103320Jm+\rHKC\1055484\14433\DLEp\49509BLm0\156598\ENQ\GS\135529#tb\120714 <\5250mO\993520\1067514)A\25682n\DLEu\FS(x@F1.DZ&\v}:\1019605]36\11353;H\NUL\a\DC3\f8;N1"))}))))
