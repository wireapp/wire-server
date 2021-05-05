{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewClient_user where

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
testObject_NewClient_1 :: NewClient
testObject_NewClient_1 = NewClient {newClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], newClientLastKey = (lastPrekey ("")), newClientType = PermanentClientType, newClientLabel = Just "", newClientClass = Nothing, newClientCookie = Just (CookieLabel {cookieLabelText = ""}), newClientPassword = Just (PlainTextPassword "\b\STX~d[T\1085362\181241|7z!\bth5$\ESC;!dj2d-\ETB\DLE\STXf\1097057k)\139849\ETBg\1076562\173400\1084787\ACK\38960L\1112278\DLE\155301\&4\985637eY,t\t\DC4n\1011466d\US2i(n>(}\DEL`\159570\1068210~/\1014737\170174\&1>\SUBF_9M\r\SUB\58754\&3\142690\179341\&8\1094758\23413/H`\v\147452\ACK]\161803\1061612\\\1000841r\1037636Zp$8\CAN27d(\1069419\&0G\179321\988120\51646\GS(\v\187890{\161447\1033188W>\ETX\v\DC4YAfDlHR>m+\a.U\1031584.*\RS*\1058932v\ETXuJOVjcj\ENQT4GW!{\DEL;\DC1\t\1029731\&2gR\DC1\16016I\1023080:\53656\fRPB\ACK]w\DC1M1\986828u66C\r\1022411\ESC2xg\SOHfv\a\ETB${\1072592b\SO\SOIs-\1013410)\13546GrE#\156477\&8\1059469\176352\fXXy\21904Hc+ze5\988515\1091278v$\984942\138261\172913>}D\DLEE\133700\159535Om\SUB\DC1\RS\147573S\1015577t\EOTL\988595\1108693\990386;\GS2\DC3P\bl\1007448\"\ESC\1072618\110603B\SOD\nh8\1047544\1059226gC8Z\f\127017\75010\EMXU\t\153383-^LP \SIPaG\1093662\1070788\&9\67274tz\1081793\163587\ACK|\a\135860~I\CAN\DC1\1017310}y\SOH\ETX\1020602>\SOH\1020838+m\140508\15700\1011870y\f2u\167096\1106314\143163jC\1044020\fp5u\73058O\1097490\EM\17629(oB8"), newClientModel = Just "\"n"}
testObject_NewClient_2 :: NewClient
testObject_NewClient_2 = NewClient {newClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newClientLastKey = (lastPrekey ("\USa")), newClientType = TemporaryClientType, newClientLabel = Just "", newClientClass = Just TabletClient, newClientCookie = Just (CookieLabel {cookieLabelText = "<c"}), newClientPassword = Nothing, newClientModel = Nothing}
testObject_NewClient_3 :: NewClient
testObject_NewClient_3 = NewClient {newClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], newClientLastKey = (lastPrekey ("D")), newClientType = LegalHoldClientType, newClientLabel = Just "\r\1017817", newClientClass = Just TabletClient, newClientCookie = Just (CookieLabel {cookieLabelText = ""}), newClientPassword = Just (PlainTextPassword "\167266P\CAN\1036031/$n3?\190718\152504I\DC4\SI\b8\191435\&5\1112642<e\SUBFe@i\1033137\1100137\1095356!\5339\GS=9\EM,:\SO\ACK\183325=,>\DC3Z6;?d\185634D\1194aH\SOc2\1074126\131865\1075933h9\SOH.\164958$\120065\ETX\54170D\SO]T@oQ$\NULu\1100923\95713_&C\139455\169401\&8p\39346.n\FS\1041441M\179796\DLE\EM{A.X\92591afD\1052024Co\SYN\ENQX#\5175\1005681\13237CS'>\151804\&9~\NUL\1022212\46741\185033\143436Ib/\ETX4ZH3 \26821N\ENQ\62590\1017495D\998139(G\7255w\187488c[\1010695U8\EM>\1102387\1055681\n\30842M5\NUL1\1080262v:E\DLE\170589\"\1070461v-z\131409\b\1109973}-x3<\CAN^\ESClf2\1091196\USF\SYN6\EM(\US=b\rX\1109914\8869\5719\1062154\ETB\49820w\1113128W\SUB3\1012733>\ENQ?3\1027818\985396w\990891\120677\v\SO\174596$C\DC1?\SO@\45730\SYNf\160249\&8\1083545\987725d\v\SI\NUL\992384\174997\ENQ\15298\17723\RS5\129528J\43601uo\FS\ACK\51952Q+\1069991Q\n\1062064\1020465\\N\CANdyi'\GS\DC3Mz\t\1088167\63762`+T\78120\DC3\1009664\nrT-\140902/\FS\156191\&8J7\tu\188260!(\FSKf\NUL\DC4?\DELl)k\bW&Z\GS\SO\1111521}\1072398*p$d|xV\1105782\f3tzg=b\1093888\EM0\"bz\170203\NUL8|n/\1027956$?\159534R}\ETX\154566\1085161\1104740\ENQ\GS|\DC3x~<\996152SH\135768<\165641\164165V9!\1089963\181154\44819\DC14o-\153884Mt:qL:\1087893$\1081097p\CAN(Xl\154312U\1000247rrV\NAK\NULb9\31955`68lw\1060569<\175253h\SI\DC4[\186345\61413\186559\f(\ETX\t\DELedNjLd\50860R\STX!H(f1\128341\1049980\ETB,@=\1008507[wC\31923\1108500p\1045530\SYN\STX\62114@\24991,,\SYN\1105666Y\1089656:\127078}\SO\1049207M,G_I\ACK\DLEoyn\v18\DEL\a\FSsG$\147849R\175907\&1_d\986095:\SOH\1048597;\1053490m\n\123141\&4O\f7h<V\US\EOT\148341<\159095@\\}\"fG\1047682\997568jBv\1112574\987181UVP\SIy1mKr\bDAR%Z=[E\DC1\158299H\1080462$.|\982[E\FSj]\DC4fK\STXYJ1r\136483`\163745\1088849\1091795\143800S)1M?g*w4\1043500\1027889\DC1XA\1069553\"\1080234>J;\fBE\146735\127388\&4\62402 i,m5\ESC\f?\SO=l\RSik-|W1nKT\NAKpV\NAKS5um=\tC-\4452a`\US]\SOH8T7\57628z\51282\991752\&8~-A\SI\994541\4049y\1101508\1027338C<\1093363\"?\154598l\DC1}\ENQS*3>A\EM_f\1001501\73976\1057978\DLE`{\al\1086382AJ\30170\1104057@\1044578o\167546i9N\1051004\SI`jW{$\69987\ENQ\v*\EM^o4\133108;C\1056258iD8\SUB\1083154\SI\EOT^~&p\32374\990662\&28be'\158714\ACKX\CAN\53267\1108155r/S{m\vM+\SYN\DLE7\45948\\&\GS\NULS\v\1084717F.U0\EOT\"\rT#?\1068111\CAN5\\fK\41280 ~)P\DC4.\50557\16230mJ\EM(P\STXWU1\DC1\1065463\189134\188266\n\1031073\1103985\1079278\a\1060262Qh\1010173}\136479\1086080\DLER*LD~I \ETX%.MbV"), newClientModel = Just "]"}
testObject_NewClient_4 :: NewClient
testObject_NewClient_4 = NewClient {newClientPrekeys = [], newClientLastKey = (lastPrekey ("mRU")), newClientType = TemporaryClientType, newClientLabel = Just "y", newClientClass = Just TabletClient, newClientCookie = Just (CookieLabel {cookieLabelText = ";"}), newClientPassword = Just (PlainTextPassword "\ACK3\DC3\30487x/\t>K\DC2~\STXsR~!\1047949B\51875\DC3\10350x\984383\23966\EMj5\\\137382\NULGJ\NAKIpp\GSWK!\1101331&\166298K\78427/\SYNXY#\1060210\57824vRKm\1009580\1092266I\SOH\39938+>\CAN\STX\v\987786\USo}\1058210\98200[m\1093673:#CRq\vrx\133725(-\132092V\174476\185652,>n\31370\DLEW\161375\b\156005\b\8275<'\1096348\DC2\169617\7132\SUB\STX\1063143\&1dnj\26272\1059222h\1103621a2\ac\137990\1791\131653\1111909\DC3\RS)\r\CAN'\EOTu&|tB\b\ESC\20421{J\996183\1041171\169713\ETX}\144859\"\DLE\US\146266\1082470\1110807\CAN'\1076956\ACK(\EM_\97958\\\ETX\1020491\SI_\EOT\47608\a\FS\SUB*\ENQ#`"), newClientModel = Just "\175251"}
testObject_NewClient_5 :: NewClient
testObject_NewClient_5 = NewClient {newClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], newClientLastKey = (lastPrekey ("")), newClientType = PermanentClientType, newClientLabel = Just ",1", newClientClass = Just DesktopClient, newClientCookie = Nothing, newClientPassword = Nothing, newClientModel = Nothing}
