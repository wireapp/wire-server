{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ConnectionRequest_user where

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
testObject_ConnectionRequest_user_1 :: ConnectionRequest
testObject_ConnectionRequest_user_1 = ConnectionRequest {crUser = (Id (fromJust (UUID.fromString "00005b02-0000-6c07-0000-6e14000058ae"))), crName = ":V\t\ENQv\SOeB\9614\&4\1066558w\1723\1038654\DEL)+\180605J\135260n5\DC4&\177236@V\DC1Tyox'#<\1046771\SOH\12751;=A|]\STX;\139582\SUBV3Ka)U5&c\136175-R@=\151896\&6\a\1014600\1051542}Qj\STX;<\r\NUL#\NAK\96624\1108651>\USbg\a4]6v\30173\133731\fM", crMessage = Message {messageText = "^\t\127816NY_/OL\58155t\1093168\173425do\39726\1014751T*3/\995010\SIbn\135976\RSG!\DC4miug\DC3=t\DC3\185133,*\DC3\41567vfx{B\NAK>!\SOH\17000D/Wy089\987300\28073\FS\fR\1100336DB<\70132\SUB\27203\40012\62318\&2go\NAK\DC4\1043645{G:)#9\50607\ESC\DEL(\ENQ\1062510e\STX\SI\DLE\1074152I\181405*\r.d\v?\CAN0\11430{(P\SOHGd\DLE\STXF\1058797N\5534j$\r\EOT\DC2\1037133I\985158gJyPih\1075758\1050873:\DELE\r1<m4S&mHm\1092633\b\996408\RS%z\ENQ\CANF\1008760Gi\190603P\FS\983990\RS:Hc=;\USK~[Z\131221SM\39708\1071747\1043863\&8D\1074032vfJJn.\1033834\NAK$\v4\CANZ4\ENQ&1P\EOT[n\13822A"}}
testObject_ConnectionRequest_user_2 :: ConnectionRequest
testObject_ConnectionRequest_user_2 = ConnectionRequest {crUser = (Id (fromJust (UUID.fromString "00005a36-0000-1684-0000-6f8b00005d7e"))), crName = "%/W\12227\GS$q\SUB(UQGw\22979h\17655EP\19038;H4\1017993\CAN/\EOTf\162609MA\GS\SOy\999965L\DC2\7604jhi OzbU\995027i#zl\169598E{N\1102737#\ETBx", crMessage = Message {messageText = "J\178581BGG;yFn\ACK\t}\NAKJ\95129\EOT\157634\45545\RS\1019485\a\RS\171340\EM{Y\1020146z\54623\67108\&5\US@"}}
testObject_ConnectionRequest_user_3 :: ConnectionRequest
testObject_ConnectionRequest_user_3 = ConnectionRequest {crUser = (Id (fromJust (UUID.fromString "000002d9-0000-5ac8-0000-2d3c00004acd"))), crName = "\DC4\1048741&\1010151\ETBn+\186683z\998849\113739\DEL:\1065690\30314eJ\RS\1067379k,\fFv{F}U\1074539E\1050644$\n\1101354\1084941CV\FSN\166007h\ESC\1052857*\r\DLE\SI\DC1<w\SI\21919p\38846\993424YV\1018156 [\tQ-\r\SO\990881\32426\9161\154432:U\ESC\1098691+\EMti\GSE\163711#\ACKA({+jb\US\1108941\NAK\1101969I\1011777p\1041822\CAN\DC1T\SOH\v3\\IZ\147340}:;-\1043530\92959OA\8400\179027\SUB5\1110621M;\93830\19595:CgLNl\DEL\EOTh\n.5R9I\n\FS\19629\CAN\SYN1z\1002112!\161456K\1050112;\DC31\EM\1076141:\\\1105721b.\NAKqH\GS0)V\35231a3i\1090728\ETB|Gr\NAKt\43312\DLE\135716S\v\SI\DELRw\150624\13128", crMessage = Message {messageText = "\32233\DC3P\FS\1005599\176360N\1051379\\\53465{\191235U'D\GSW+|\164348GJh\GSL\135151\54179={z\1081381(\1043657s\73083H~"}}
testObject_ConnectionRequest_user_4 :: ConnectionRequest
testObject_ConnectionRequest_user_4 = ConnectionRequest {crUser = (Id (fromJust (UUID.fromString "00005adc-0000-4bb3-0000-3ea700007a0c"))), crName = "\DELw][\181790\43932\r\ETB\ESC:Ak\f\EM}NUkj*\1027761\174875:\FSm\DC1\1102984\SOEM\bk\1037615l\1003200M\18434\144378\68135NI\SO`k\54050\183882\&5?\65547\180339W\17198\ACK\"\DC4!!\DC1\1006252\171258y \1079064\1102082\34452>\185344,\1085267\176923=/{I\t\vF\1111256D0~ 5K\1068550Ng^\169153\ACK\989644\145989HI\30560X\DELa)u\SI\147804\EOTC\1004475\f\\\NAKH\ENQ\t-^y\118864[<\15890DF \b\1107619\GSC;\STX=+\29213[\1045149P>\DC1\1037948\v\137390'ov\159834\62285\r\SYN\DC1&kz^\984162wGC\1080395f%\f^!\CANA\\\STXZZL8sF\DELW\135373\SYN\992781\1695nP\1087199\1107972icO(;K\145888\1096583*\\vS\ETB\1054867", crMessage = Message {messageText = "r\GSJ\SUB$od#\95510\148514\GS\32960D\170804R&\SI\v\1033667\1065915\US\n\132875\1004727\ENQ_\SOH\177434\SUB\1077261\173351\21834\&5Z\EM]%L\NUL<\GS\NAK`\ESC\1074557?=\SO$\1091670[c4\996432\ETB\EM<\1101797\SYNP q9.!6~/\v\SO0]\986374}}>\n\ETX\24484\1014807_\SUB\v@\990494X@\ACKrsMU\1040393,R\181832\\E\991266\175946\997370'\1027180\&6\32066+.\ACKNO\983331\NUL\162083\179207r\DC2Km\v\GS2%(_\7632\&8\29766),h)w\SOHPN{w\f\SOH\ENQ\FS*~\1099636\ESCW5^\58281h\190947\10266OE\r\NULi\1093640\&12&\DC4\ENQ\ETX$IT\NULp\51348sP\FSP\9629=\DLE\178786\DC2Ps\NULJNR\1044113RI}\13007\1074397\ENQ\FS>C0@\"t\63870\&1\SUB\999862AfQz\\T`\DC1\\"}}
testObject_ConnectionRequest_user_5 :: ConnectionRequest
testObject_ConnectionRequest_user_5 = ConnectionRequest {crUser = (Id (fromJust (UUID.fromString "0000557c-0000-4553-0000-49c700001fb2"))), crName = "%\136013\60107|\tv\DLE\DC4ca\US.\65822\34559\r&\\\b-o\6128h\aU;y\154633\\", crMessage = Message {messageText = "\52956\1260\n\1086713w\1044571rF\ENQ\1095995\&8\CAN#\"\1027347\1068155i\DELNT\17649G\EOT_\SI\SYN[Cu\1004155\1033274|F\1067629#\ae\DC4\161874~w[,R\f\40259%8k\183680\1053776;\1106229\119555w\182855]\SYNF\SOH&~w\1013456\rs\NAK/\161966N?#\ACKBn\DELD@\20256D\ACKV8@920\ENQ\189905\1072272\DC4\70101\t_\DLE4"}}
