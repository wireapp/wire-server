{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserConnection_user where

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
testObject_UserConnection_1 :: UserConnection
testObject_UserConnection_1 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000000"))), ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000000000002"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-05T02:54:01.449Z")), ucMessage = Just (Message {messageText = "\1086309\1000956\NUL\SIt\995789V\r\1067568\DLE3 c\SOHGO.%nx\1091857\1110546 \136101\DC3A\r\nJ,t$f\ENQ\"&\STX\993121\RS9\DC44[\183422\97686\&7<:S}K\ETXJ\30855\98554`H1\1010092q\1059554\1084484T6ae/\3536\61487`T\1009740\59004\18097q3+a\b\1077053K\\\177241\52951L\US\t/p_\504\&5,>\DC1b35\31120:\SYN\59562\1085119\1094697e\DC4\ESC 5v\GS\28786\&1\999193\59784\ETX\ETX\129069>w\STX\DLE\1103180\SUB\1084537K=\t6=i\705\186494@\"0*\v1\1019358\1055961/\1040468PQg\65767%\fr(\82951\SUB7vH\1096220x+\1084208}\1046597^\CANX\134932\DELg\1022907\36866\"\1093174\22719\165369T\3928g\ETXqs\190424\CAN\be\17664M\"P"}), ucConvId = Nothing}
testObject_UserConnection_2 :: UserConnection
testObject_UserConnection_2 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000300000001"))), ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000004"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-13T23:49:43.590Z")), ucMessage = Just (Message {messageText = "K\1052114ad0rF\"H\1067572<\26793D\STX<U1E;%\SYN#\35465\51787\USl^\1012436\1014965`i;Mq\ESCn\EM\SOH\DC1\1099848\1007699c\1032200l2lwH\DEL\FS\1097531\67624\1087790\992069v\1101117r\170900Is\1083100\DEL|:\SUB\CANno\NAK\1094861a\62911\NAK\1028934\a\t\52148\&19\a\ETXg\189606\ENQ\152481\ACK\1081862mJ\DC3_\ti\DC1K(\143408\187887\&5m\1003248\RSYH\1086461\\\47027\1020794\&3X\33037\GS\DC1ee+\fDM/-TN\136039~z|\1028922\35686\SYN\ESC\DELJ\RS\DC1Iv\US'\DLEZ>~"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000003")))}
testObject_UserConnection_3 :: UserConnection
testObject_UserConnection_3 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000400000003"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000000"))), ucStatus = Sent, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-04T20:25:03.586Z")), ucMessage = Just (Message {messageText = "\78481<\DC4nuO\132898\GS\fy\\\DLEYGR\b\1036888\189681\DLEuFCNvh*+\b|8{LQ\1054888\5450twE\137533,~Ds[\SOH\61896Gy\1091862CB\1083441\RS\48558\ACKqq%4\SOLtWD\NUL<"}), ucConvId = Nothing}
testObject_UserConnection_4 :: UserConnection
testObject_UserConnection_4 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000000"))), ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000200000000"))), ucStatus = Blocked, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-10T22:21:21.036Z")), ucMessage = Just (Message {messageText = "\11121VEX&bGm\1037316G&wP\NAKtz\NUL{\DC3\ACK\SI~oPg\ESC\50438 4\1057345\DC4\1046357)\a\61737C\GS\n\\hNc`\f\US\191332w3\SUBT\US@J\1022383\168442\917996\"\RS\54403\1053142:\1059220\&0\148886\1088547%Z\STX_#?Q/\178562\DLE$\1030532\23680\1085406,\a#t\1096888\RS\USy,f\bL\ETXy\137065r<yj^}\1049431\ETBn\999144s{-\1065516 \r\8206S\150344F\GS\1035660uM\1103284^f\1088765*\"W#\nVy\989036\bi1\b\1018748\&4\1042451N\74185\168279l\1028092u \1014652m\ESCG`R\EM-t~O'?dE4\183984\18563\163228#^\DC1\b!R\DLEz\STX\SYNSb\1039061;w\189899n\RS'\991996M\23123\&0PDp\37774\GS{C\190026N\r7V\DC1"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000400000000")))}
testObject_UserConnection_5 :: UserConnection
testObject_UserConnection_5 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000100000002"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000003"))), ucStatus = Pending, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-13T04:51:29.749Z")), ucMessage = Just (Message {messageText = "kU1gb\27796\DC3\1043123C\1054115\140804\7919m*e~\ESC\73702R'\182662{ID,\f\rr\NUL\69844};Uo\1041288&f\157286\SOH\135678\SYN]\";c\1090486&i\176723\&6G\92435\t}mR\1069596C\SUB\ESC\1043420|\1029684\&7o\\~Dyj\1069120\25132O\ETX\141203\1108333\1031786{\144724;k/\38615 \190199\DC2jg}?\28657\&5\aZ\177779\&2\1054162\154034\187235fqf\97933\v!$\DLEY%~/y`K\a\70821\&7K\154850\1037694l-\f\SO7>\DC4\8932\SO\131336\FS\1054830@S\1068243\t0\SUB4I\"~\EM9\33691\t\1045309\ETB \1031404\47948\1052932r@\166625\1028847\49139\&1\164228+\95102s#G\ETX}\DC2\1006905\&3(-b\1051235 \126633U\62267\f4?zH\152778`X\GS\984260^\STX\ETBk\988266y \FS\173451\DEL\1111682,\145133\37017OX\1061243\NAK\2102\1065829\147678W!w 5_\DEL\1094743\1042996i\NAK\fo\5237\994922P`\1052275\176034\1062959\EMB\1067770\DC3\18121\NULs\34360\EM&\SOH\988330\NUL"}), ucConvId = Nothing}
