{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RmClient_user where

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
testObject_RmClient_user_1 :: RmClient
testObject_RmClient_user_1 = RmClient {rmPassword = Just (PlainTextPassword "\f8\58178\&5\94444}\FS,K\DC2\177537>\v)-\983929}\ESCb\DC1\a=\985668p\1017127+\15406jA&\ESC:$\f\1112982@d\166080p\DC1\SUB'\ETX\1113948:\US\ESCkA\128476y\166779%\aZ1c\171808)oD&\DEL\SI\ETXf\v(T\190234EI\DC4B\DEL\21845,7\a\139744\&7k\111247bv(\SUB\DC1)^rZ<]?Va\a\"\1053447\&4M\nQ\USD\46027.\135933\f\SO\1011124DV\131173SH\170325_\997843\100127\NULf]p\DC2Z/\39328\aJQ1\ESCU_L\1045879\EOT\1044402\SI`\DC1Hs\1008737\ESCl&\te\bexEC\DC1\ETX\1056264\1025055\133057\&2\53388\&4y\ETX\171926r2\1052120\a]\ESC\"W?\v v0Jq\1001936rQ\DEL\1001786\"\DC3}\51072p#\1043499\NULm\1051515\DC1VI\47019g)\US\1100670\1108613.\DC2rm065#\NUL\a\ACK\1060893O!\EOT\1104363\SUB\DC4\DC1-\998649\5938%\1080836\60393\177042]9\1040841@\b\CANn\157200s\110870\989700OWE\132352\1096551Q_t\DC1.\19450M\143672\999842@+%\US:\1075439\158152S\NAKr&}r)\151867$\DC4\USr\NULix\44197\SI<\1066381\1021465\1093760\98136\35209\ESC%\NAK\"+'\bN\18535p%7VsJ#\ACK\1063453KF\n[v\123588\GSy\64859r^\ETB\1036404\ETX\ETXCp\1479<3\DC3e\1019256\1103360>[\DELcX\NAK\159610G? X\143249skq}\44327NR~&ca\174046C\"D\1099275\NAK\120178Zl?f,Qd\ENQ!#5\DC4Ph\f3\US<\bxa\aUz\15211m,!\167028DOqa\SUB]\1077512\133546\&53\DC4:\US/\n\RSVVa\SO\51356\RS\n\1065692.\33670\"5k\1060889_',2\rmgsd4\\\SYND\133186YUF<\155117\986379\DC1!2\1044044/1Ii\ETX!IV)\184494\188610\DC1@||]3\1022593z\1080357p\137485\ETBt\r\bp\1112498`\997060+-f\166755ZU9\1062893Pk\1063487\NAK\179766\&1\1102307y-\1059054d\GSx\DELu\SI\1009172\RSt\1098746\SYN\SYNTlO&f9_\1045494q\EOT/\1033342G3\ETXG\1033400{/\FS(PD&V}h\ESC\1056927p9\rp{g\1029883,1E\1075311\DELSMd\ETXi\"k\100456\&1&Uo\DC1\181481A\1042281\DELp3\1001214\1048606fNhX\NUL\151502~\1025564e\ESC7:\1013116{\v\DLE2\1010222\149868+\SUB0\1107361\ESCM\1077204<\t\150498#\SI\1108706\1036521\DEL\110877\1041745gU9p's/j(7C\153889\71069c\182493kcFv}\DEL\ETB\14695\96865\92325\ACK\DLE\CANn)\26462\994304$e7,L61\159298{'\31400\97460f\SUBl\17135\NULkj\RS\78024m69\GS\1076823/8!Y=\1104572\1003471\DC33Is\1113183\173958\FSe5C\ENQ\151089p7m>\471vJV\DEL7\SOH\132662s\149663\128037\&2S\68914D\DC3;\tQ\146205\179571\&4\DEL_Ql\65464\50815lX1<\1072597\94585\r,\1053868hC\SIDbR\1085608o*\DELgn\1064857FJ\1060171\SUBK\1015395\STX\1016488K\60656\STXk\132985!goU\21301y<8h\EOT\SYN\FS\SUB\ENQ\b\USh\ETXH\180376I\147397s \1063918\ENQ\1000302\n\158680\CANj=+\ETBS~\USH\156212<g\155397'\DC4Q\\\nfVnk\1017068\1040158\&9\">\RS\\\182963\1012492\SUBX4\1113090\1093425M)\72403!\EOT\SYN&\SI\154124V\ETB,\1081126\1089041j'\SUB\154506\SYN.\1106509q\187117A\29002\1032943\1094355Fjs#\1079311\DLE\1000435\&9\NUL\1015461D7@\DC2iD\EM\152847-\179575r\FS\DC4\ETB\DELwH.\DC1TC\SI.\1003310E\tYr3#~4G\31667H\38849\99703%\\\EOTdJu@\ESCU\ETX[oGkt\155425V\169308f<\tJ\1034027s\33110\\P#AOQ\160359>d\151042\STXP{xr\1036169m\1061238jgI?mwyL(\NAK\1077778\28372\DLE\DC1nmN\RS3p\f\\Jp\30468HH*\GS9tuO\55289@\1077179\vudj\186975\65946R]\1035237\16182\191192\988792Ba`\1063293\SI'=\30622\SO[")}
testObject_RmClient_user_2 :: RmClient
testObject_RmClient_user_2 = RmClient {rmPassword = Just (PlainTextPassword "\25751L\1018523!\142703\&3s\"B9\993834\1110263\EOT\r^z9\29936]{\SUB\1014528\ACK\SO\993996LO7,5\ACK\DEL\1109250H\1010315")}
testObject_RmClient_user_3 :: RmClient
testObject_RmClient_user_3 = RmClient {rmPassword = Just (PlainTextPassword ";vwG\1015770[\GS\SOHY\60251\990016`\37408\&0\ESCZ\DELlBP\SO\1027712(&c\DC3\986321PwZ\1037676C\ACK9\1075420b\t\SUB2\1023838(n\1079091s\DC3\DEL-e\983695,j7B\ACK/\21283\1048101\DC3\RS\136912i\1024550\1067443\1099285l\f\1053982oY\DC2\48400\1002574\181019d}P\RS%>q=}\ETB\nU`ye\1039895\RSU?f8\ACK\n-\ETB\\f\ESCzD^[;12\EM9A!3f\1099990~Q\100084 \NULMC\"\100285\\u\987233\SOHw\DC19\ESC\983531Q4fq\"g/}\\[\DC4i;\FSZ,\ESC\1091795\GSh\SOA6\1076992-`s")}
testObject_RmClient_user_4 :: RmClient
testObject_RmClient_user_4 = RmClient {rmPassword = Just (PlainTextPassword "E\GSZnJ\USi,\120939\NUL3\DLE^\\\ETBmc\161369Qr~1\ACK7\ff\"\17693\ACK*\27762\nUB\FS\ENQ\\\USKk\GSGI\a3F\25108PDJu.d\DC4}\n:!-\181487o\CAN\CANr\1014654OJ~g\v\1074407\&8mb\38755feX\15263MciGU%\128018\GS\NUL[&\133590\ETB\"yGb;\128086\NUL\542<\178623]\SOH\1112614\1114024\1076493{\CAN\DC4z&U\b \DC2L\NUL\1068288\SO\11101D6\134934\53583\1070919w<\NAK\SO\US'l2\SUBX\DELZ\1012368;\DLE\r;pe\b\1072865$l1&:\65263\"(KH\6995n\1009780-z\a'g\ETX/\b\991703\169967.\984066'KG1\1086440$b%F\DC1SsI\DLE\ETXK\ETXh\1038105\1009892\SYN\1005501[ivH\ACKH'c'2\185697\DC2H\SOr\n\aiU\ETXcQ(o6vFAv\b\181085n\DELT\EMVv\b\1018759\1105741\1077672\SUB\1047579BM\DELV\1083005@\US)E\169248\1032858Uuno\164724\NAKw\ESCI\ESC%\139661V\DEL(V\1021595\1086557\EOTl$SHuRj\fk\190919}LS%\1112122\168698\9382\1082622E0\182115|',\v\1113968\&0\CAN\EOT\SYN\1045681\&1\v\ETB7$%eA.\54080\EOT\ACK\t\169409sAg6O^m&DL Y\1075595\1094742\62263\1032365\1062082\1108043\989582\"\SYNb\ESC^}5\184333\nD9\1053881\1001752\FS\t")}
testObject_RmClient_user_5 :: RmClient
testObject_RmClient_user_5 = RmClient {rmPassword = Just (PlainTextPassword "\1063633v\DEL\60507\1096351\1108107vZ\ETX\t&\ESC\998242\US\1027484f+\59242\&3X\NUL\83515iGx(\SOHBD\8968eMk\FS\EMs.\162555\23137P[C\996148<<1m*\DEL\FSj\53540[F;\RS<\1016111\142822\EOTe\1103587\999647L5\992221>\DEL;u}\US\NAK\n\1097463\&0\9139\DC1\EM\SUB\16833)yJz\60739\EOT\1071214\ESCdx?\SO\1048327I\1073946\ETX\SI\1046085\182485\"odUC\57846e")}
