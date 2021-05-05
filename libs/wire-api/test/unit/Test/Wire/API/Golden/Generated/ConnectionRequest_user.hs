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
testObject_ConnectionRequest_1 :: ConnectionRequest
testObject_ConnectionRequest_1 = ConnectionRequest {crUser = (Id (fromJust (UUID.fromString "0000523d-0000-5197-0000-0b37000038a6"))), crName = "qd\1283\1039441\US}QO\983092LT?Yd\1103447\&0\1055690\FS\1002600}\ETB%\NAK\1083918\175047\52577s\b\EOT\US[\ETB\120789l\1070068\39064\188982\SUB\1045222S\DC3\1022398\60469om\ENQQ\59155k9\1104105e\NAK\CAN\DLEc{\33930;|\1007326n\DC4\STX\DC3=*\144067\1032801\&0 \RS.\ETX\134716\52525%S<\125028\1015548#\1022920\&0\NUL\1072865\EOTP\1072321\1049268q!0\a{&]\1092353w\1060551\FSka~]\1020406FV\1067420q\STX\144817Dbm>\ETX\997552\136816\nzR\1042793\NUL\1040016\1086575<|K\47967\NUL\DC2Msvp\USLd{\1056267\15529'D\32609\&0I^w\DEL\987287\EM\n\182252\1069938R|\155541*`\1020917Q.\34386\141877<\155087\&9f8:\160627\1046486\&1b?\137730Q9m\67341\1103449T", crMessage = Message {messageText = "\ENQ}K\29540\a\EM\74337 RDVo-\45464w\997870\nb\29989\&8]\24817\1087700\1042603\1075311_\\\DC2\142786\1645\48448\EOTD\147357\1022889\994149\r$\69424#\988502k\US\137288\167130\62232D\154234A\52863^]{,,O\70149\&4o%;^*CCo-\NUL\1020338h|\DLE*sqQ\182438\1088568A\32296>\1069302k]\DC3\ETX\998931\1022253\US\DC4x)V9Qt(\156806Kyt-1\SO('b^\35581\DC3\1043107I6\STXM>Z\1059408b|\SIR\1026015\32788<O\993598! (\r,w\ETB\RS=;\SUBbb\993762a@\140493\DC4\159604U^Aq\1086336\1038158\NAK\ENQ\1081771h>\153727o\FSv\b\1110776!\DC2\155151'\DLE\SOH\1073175HE\GS\146361\&7:"}}
testObject_ConnectionRequest_2 :: ConnectionRequest
testObject_ConnectionRequest_2 = ConnectionRequest {crUser = (Id (fromJust (UUID.fromString "00000ec1-0000-7275-0000-361b00001bd4"))), crName = "-U7\1092389\&6th\182107\993192\58511V\GSh#\nV\145276[\SUB\1096046u\987302m^\n\1081590\EM\37018\133021o,\1052057\DC3\1108140\&1 H@\fwX\173120\1025564:xK\t\26693 *\3667\ENQc>kz% @m\DC2>]\rt\SOH2\68650\57417l \94702j\42133]\SO(9\SI`\SUBAP.O}-OfF^\989207#x,\128424\DEL\EMF\SUBl\RSa?{\177585,5K$\66576\1099122\63367\r\t", crMessage = Message {messageText = "\1015285\1047491V\ETBV\157040\4322\&4e s\ETXH\1019884\1075543H\36995\rT7\SO\173727\CANpa\n/cB\\\SINvt/\GST><\179275OJOV6gZ8\190224\137698a7.A\1042490,+k\ETX\DC4g[B\48594 zU@\38581\DLEHO\STX$F\EM\72823\174082\1112362\1099222\b\1051283\1008214$u\6473\DC2\DLE\18909ZO"}}
testObject_ConnectionRequest_3 :: ConnectionRequest
testObject_ConnectionRequest_3 = ConnectionRequest {crUser = (Id (fromJust (UUID.fromString "00006f44-0000-6409-0000-0b8100004847"))), crName = "\1097809\996780\SUBQ\35736bd\b?\r\132859Bm\1041921\59731b/\aX4\78657\1001536\168499I3\95694\&6\986049?\141505Z \SYN\183611SU\USU\995725\rz\1052190s\8652\185392\17306H\149507\SICe\RSf's\1054605/\1003866\a|\1111276Z\FS}/\52912\23445\23159\RS\59146\181607#\ENQ5\v\164086\&08G\b*j\1011738x\r\fmO", crMessage = Message {messageText = "p\151264]Q\18599GMd\USs\SUBM\STXq\1108217<\1031139\1053201lsp\1042963\1012963k\SIk(b$\120648U,\179499\DLE\25802\&1vuqZ(`\26717v\9383<>\DLE\FSU$\ACKs\DLE3#O\1022406"}}
testObject_ConnectionRequest_4 :: ConnectionRequest
testObject_ConnectionRequest_4 = ConnectionRequest {crUser = (Id (fromJust (UUID.fromString "00005300-0000-2b06-0000-18b000002c35"))), crName = "\GSs?\DEL\186929O\1011870\a\EM\161978\a\1085346\166079\1000098pU}\149110f&8O8\n\b\153048\55261bm'u\1109675f\CANVBI7\2447:#'\1065797\131989\150794\1064008@Y.Ws\1099049|?\rc\r\SO\1000363\1110342\40056\1075258k{\53543z\ESC=\US\DLE/~\n ^fFi:(\185343\DC1\ETX\SOH|\164084\168815\1088309nK-PAg\CAN\NAKT\993363\&83\1082576\1007228jo\8516\1066407\&9B[h\185253\20051W\SOH\STX\USFW+@pAP{\60896\9484\f\DC4\997884\b\SIq\fRz`95\SUBWZ\nTb\SINcn\fv(\30642G\t`V~y", crMessage = Message {messageText = "\f\1044375(\ESC\rm \1012373\1019086G\DEL=\"\1110528\177170\b\57381\&5\988616\SI|\1017658?P\7022\1054876H\SYN~_f\DC2b/\n\996251\1052146+\US\n\NAK\1078277\n>Q\998257\96222a.x\US-|&\7888t_U`k\1096015B\1112222\DLE\\3#4$nS\\\60250=?\1022864\1069206\&7\SUBrc(x\18747\&8k\\\SO\SUBe\1065603\1085431\STXz\"\1015030.Q-\1045118:{p{d\ETB\1055631\DLE\166489\145743"}}
testObject_ConnectionRequest_5 :: ConnectionRequest
testObject_ConnectionRequest_5 = ConnectionRequest {crUser = (Id (fromJust (UUID.fromString "000027df-0000-5c5b-0000-133700000e7b"))), crName = "\b\EM'>GX&d#\136006\143647\1037763X\140474\EM\US\DC3T?\"|;$_\1005872\&8\US\5426K\NUL'\119091a\b\r", crMessage = Message {messageText = "`\1109412\69737q\DC1\1045475\t\50129\&0\7391\b\CAN(\998862aQF#D?o'\a\GS0\60988F\8488\&1\92177\GS_{/\160784*5\133846\157350#A\f!\ESCs\165831\159879\&8oJBk\69400n-5\1113546\95247\&5~\DLET\17947wS>gj\ETB\38833J\SUB%[\135437\DC42\ESC\1080517wG\1109966JD\135107\1054716LP!\1081150\32271vT}\US\62645\57530v\DC3xL1\27358?\1027135x\1100391\&5\ACK\147761X\1045628'Ppk\DLE|F\120703\174961\ENQ\36192l9z\1111517\&5{\DELFs\ENQ\58696/\DLE\"z\DEL\1026323L{\1056123h-\1003898\vY\1002196\986249;+\"8\NAK\148908\178938%i\DC2k\1000065\&7UT}\62329"}}
