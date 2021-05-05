{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.BindingNewTeam_team where

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
testObject_BindingNewTeam_1 :: BindingNewTeam
testObject_BindingNewTeam_1 = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("r\v\SUBuVycX]#jG<\1096190\983190f\1030251{p!\1006951\SO':\10904;\ETXWA\"\1073944\27894w\11364y\bO\1008202G\1068394\97641\SO;8\DLE\1035336rD)@?n\SOH\54396^\1017536V\GS\US$Y\ESC6A\v\1002913\DEL\GSe\"D\1001859\EMn\24673\1054436~\n?\ESCA&\159522=\1090048\rG\1029719EPl\52880\51158uK,7/<gZg\127833mj\ETBx\134577\131251\&4wp\2007\1106180[\SYN!\153774\1087429b\1021668\1083563C@\171202_\ETX\78390\&9?I\155966=R!:\150576/%")), _newTeamIcon = (unsafeRange ("\45714\129189.gE]\1063889_\178672N\1072442\CAN\11106X\26737\143765Z 5dG-\159692tq\DEL\DC2LV\CAN:B~\SOH\155368]:\58719uS\98733\990574\991366\n~\SOH\1095381Z:1G\34569\a\f\54646o\1002688t\EM\1041557\131402\1064328Zg\62790")), _newTeamIconKey = Just (unsafeRange ("r\DC2(\100565[;\62389tE\170762R\DC4dm\1067052\&8\1002300\DC1\194757\ETB\US\1032939\150883\158061\ESC0\CANi\SYN8Ag\97779#n\96237\59827g \v\19539\&8\985813\ENQ\25540J\183404\16833\tD\1106713\94750^x\10102\NAKb\1107026K\t\97450\ETX\DC1'\119625t\DC3\1010138\35231\GS%z\SOH\1036774V5BP\aN\v/'\SO\164711\&5\13233V\DC3\NAK\1111743X\52648\&2k\tk\190822DF\1006933U\ENQ\DEL\NUL\FS\SUB\t$_!]aH\ETB\GS7\STXRSRa\1101944j\CAN({\1061462K*,me7M82Sq\163723{}!\EM\138013|~=\58285\5466&{\32073\1080421Y\\t\ENQ\DEL8\1042987\1010585Q1Kx9\DC32\ACK~h|%b\14321\SIk\166167+\rM\34796DH\t3\ETBk\DC4\1005334kN:\144240f=G+\1049989\&7i)\96935`!t")), _newTeamMembers = Nothing})
testObject_BindingNewTeam_2 :: BindingNewTeam
testObject_BindingNewTeam_2 = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\STX\ESCyq2\1090722`/\188850\ETB\164965\NULw \fX3u\DC2F\133522)&p\r}\36916\185361]3%\ACK:JOHJa\1026543>S\991714\1089760\ESC\1014659?\1004660\DC2S#\984608\71916u\SYNj\48035\32744$?\SYN\171316[\DELR\\\a")), _newTeamIcon = (unsafeRange ("k*p\SIT\1046254,N?\1044947\&0\r\n\94637\DEL\1066113qD\ESC<\US\r\CAN\64558?\1039427H.\128069Mei\178416\t1ycc%\1102689:,n?'Cg\SO2U\1108053\NUL\SUBZs\34944b\134629*5\1066264\a\64623\1002584\136906{\STXFp(``\128449mI<u=\120159\18735\1048432\1037490\135564K{ |n2x\133262\187332c\1046807\990459-r\1082628J&y\nU\USF%h\1059731LZy\165977>9\1086809x\SI\1004181\1047586\SO\133924\1049039\SOH1\\))\ESC)1\139315A\NUL\v8*\147580\\\b\DELl\DC3e\CAN#^\43219G>]\ETX\SYN\t\SO:\1006585^\152393Ne\189010")), _newTeamIconKey = Just (unsafeRange ("\ETX\4740\&5i\SIfp8lO3o\a4)\1075396\r\US\1071600f\";")), _newTeamMembers = Nothing})
testObject_BindingNewTeam_3 :: BindingNewTeam
testObject_BindingNewTeam_3 = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\SOH:H\EM:VE\1097983ql\13387\NUL\58850=T\177586\a_\1000810\DC3f\1030643\1082110\"V\997185Y\DEL+J_\984759\1099256,\ft\4266{\134115\ACK\173925")), _newTeamIcon = (unsafeRange ("\\c|yR\1092829+S`'\1064895\140266O\100308j\1083603rA$\165816^)#\10283)\1108492y\986466\&3*\DC2\42235\&8\r<W2S.|F\1065586\16656\ETB\1108337\SOH61/\DC4$w\vP\50213\194908\&3s2\1044296jV\tL&s\FSC\US]Yn\f\65177\CANk.\171941*\RS'NW\DC2\67615\&2~T$F5q\DC1Q\DC3)\184591\DELzln\GS\27335\1016627\1086110\1051843\1021190<iBP8\65015>\68059\ACK\t\99295@7cYk\1015167")), _newTeamIconKey = Just (unsafeRange ("\12017BwE\1040838x\66890Y\1017278\GS~\64022\DC4\b\US&d!\83219\&2\ETXa\37883D6\1014567D\SYNA:\992924R\990278$\1027959}\1097834\74537V\188238tYiz\1048233\60555P\1014912\1031294\CAN\47217\ACK\1039232\51633=\1081016;_p\ETBp%-7\1057963\1023226\1012344\1028424Z'\1095296<=bk\33334\1047241\SO\US\tBZ\t\ETB(^&\ACK\1015844CF$%\r\1026161\SOA")), _newTeamMembers = Nothing})
testObject_BindingNewTeam_4 :: BindingNewTeam
testObject_BindingNewTeam_4 = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("*\\v\191283\185081\ACKfD^\1041854bq/\"b\83265_1l\50911p:\29000\1007538\EOTflJ9-Tt\195063{q\1032980of\1045255\\Y\983435\&3\t\DC3F3V\183049-\DLE9\1025546 nd\1000792y\EM\SUB\1062153\1058557\1014375.iV]\1105259c\DLEd\60395V&r\DELzPoBz\1108624\1072229\49250IY\ETXyd\1101593yEA\190201&*bp\a\1108788QV\1104083^*\ts\NAKO%3\1053377\183441k<\160451M4\DELM\DC1Xz\128544:\1009241\2962\SOt\DLE\NUL\1042035Q\SOsM\995815Za(k1a\DC3\SUBX\170558\1015527(~Uq\35450Fg\147221")), _newTeamIcon = (unsafeRange (")\95431x$34\164284II!d!o'k")), _newTeamIconKey = Just (unsafeRange ("\48228\v_\b\164046\v\155475\165049d\DC1M\159586.W}}\1091527@n*\66233'F\152739\32796\DLE\40400n\1000148\&1ntTM\NAKr\SUBM}!\136675z\55033jk.0lQ\55062K\4307\1097913+4\1034660Q56N,\EM\14291\DLE\1092507>\DC4\ESC>}]z\vv^6FV\54967G\f\ENQ\65328-\48301R}\1066348;!r\34145k\997839\a\137934-\n:#\DLE/\STXe\DC2!%Y\DC3)nkr\94538sHp#\\\34020Z@\1070584\1037538~\32586rh>\1035247\1048848\n\DEL\GS>")), _newTeamMembers = Nothing})
testObject_BindingNewTeam_5 :: BindingNewTeam
testObject_BindingNewTeam_5 = BindingNewTeam (NewTeam {_newTeamName = (unsafeRange ("\1065292\170846Z{\984115v\165352CGe\n\ENQ\ENQ\1089769\166126m\143948\SO\997650\61224\DC4e{\n\SYNw\1037755\GS\ETBu\170243Lm:z_3\1062819\1060584/K<\DC1\1084091*Q\1001478\US\8492-\1079098\&9\SOH\GS\78266\185898}!7\1032304\FSFW8|\31668\ESCu+K4jcg{u@\1029344F\66022\&9Y\DC2\1111893yQ\v\15156\a\t\EMPE.\DC4|\5642z1#{\149987\21165\&0sO\ENQ\30292\ETBnMCj\1094658%\174778e1A\ETX9d,\188362e\7633I/;\1063080\&8q6VEJ\vs\1061314\&4=Gtz\DC1e\1069972o\EOT{V\160228f\161250`\STX/7\1102761x:\5491D\40819v\NULUr\984054\ETX\NAK\DC4\ETBe\r\54617\DELf\ETB\a\US4\DLE,ywa\ETBc\169262\EOT\tT\NUL\1092631g\NUL\"\ENQh\149981{\ENQ\1001280\&9\a\989652\DC1\NAK\DC2o")), _newTeamIcon = (unsafeRange ("$\v{S(\999888>\57546zK\64022]\NAK\147577%M\160030")), _newTeamIconKey = Just (unsafeRange ("\1014287\\P'?'a\ETX\1058299k\US\1065570\21712\US>RC\1016306`+\146564\&7c\178878f3J\a\CAN\t\33269sXG\175110nX\ETX!oD\1069898D)\985379\&0\STXxcB\1076196\13971Q\FSq\128913*B\1067939H\97837\1031089V\GSxh6^a\ETX/\DELZ,CJ\ENQ\83086m\DC3I\23331\40347\66411>XF\135960\132760\DC2\b\DELCTX\12626:\SO:|}Mx\1000003\1033006?L\20486\&1\986254\DC3$>Dh")), _newTeamMembers = Nothing})
