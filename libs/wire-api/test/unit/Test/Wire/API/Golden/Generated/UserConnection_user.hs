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
testObject_UserConnection_user_1 :: UserConnection
testObject_UserConnection_user_1 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000001"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000400000000"))), ucStatus = Blocked, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-04T02:56:57.175Z")), ucMessage = Just (Message {messageText = "&\SOpn\DC3\SOH_\1070772\NAK\1096330+\a2\1092076\ACK\160523Ik$\SOHQclmY!5\SOH&8\ETBCY\ACKTB\NAK\ESC\168093\1036697TKZ\1099891){\1103612\GSy\DC4\18813\ENQx\993839x5\1113202F>\1106096\1009733\SYN\a\bW[X\ESCef`\1068902\99627\&9\DELSV\DELvo/a\59880R9l\SI\42037\1061688[hB\98880DZ#g:\985562.\121289\1058834\SUB&]\" =\13052\FS7\DC2\31702\\\1093468k\27238\20447\1085682\&4-\1011186pqs\1091951\ACK\170927x^\989464u/\6719k\991276\&5\t!f\STX\SUBF&\158466i\1028091#i|\\;\156566\101029sx.]\10708\136435\1095216\NUL"}), ucConvId = Nothing}
testObject_UserConnection_user_2 :: UserConnection
testObject_UserConnection_user_2 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000000"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000400000001"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T10:39:36.951Z")), ucMessage = Just (Message {messageText = "%,?U\SYN\1075515\EOT\1053905\v\161106v\SOS:\SUB\ETBGm6\1016523\ACK1\FS\98176:nU3H/D\100185\SYNI=\162499\&7\24912/\1004942jh\ENQ\n\1017074/2eH\1058055?,\1095352F\\rk#\1029712:I\GSJ\ACK\ETXa\t\SYN<e\1073830\&3"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000300000004")))}
testObject_UserConnection_user_3 :: UserConnection
testObject_UserConnection_user_3 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000300000004"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000000000002"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T20:04:48.881Z")), ucMessage = Just (Message {messageText = "re\ETB\1026623\1110675\1052547#\DC3qb\f\ETB\1061731UQ\SYN3\b\DELZ|\96700lT\996496;\ENQ_\GS\158191m'\1086110z/\CAN/hF \54512\DC1\1046274\SI\65884o|\1071975b\1008075\ESC\DLE\1074804Y\1107070\t\CANv.\DC4\1073275/\1024207\59356\42097\47280\&6P!~/!N\181366m2(\1105836\987647<\1102726-N}*(C\STX,.OX\SI|uc\1102898\1040102\170689\DC4g\159281n\t!M\FSz)Bt\18010d\tS\58855\1027986\&3\141805==(\f\188923@\998173<\RS\SOHr\20677\STXSKQQX\182936\183937@F8>\ESC-\2828\188826&\EM\t9|M0a"}), ucConvId = Nothing}
testObject_UserConnection_user_4 :: UserConnection
testObject_UserConnection_user_4 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000004"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000000000004"))), ucStatus = Accepted, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-06T12:52:15.220Z")), ucMessage = Just (Message {messageText = "1\DC3\US%u;\1081496\142254\&06~x\78505"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000100000004")))}
testObject_UserConnection_user_5 :: UserConnection
testObject_UserConnection_user_5 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000004"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000300000003"))), ucStatus = Ignored, ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-04T12:38:01.079Z")), ucMessage = Just (Message {messageText = "SK-\1085198*p#Q7\EOT3\145431l"}), ucConvId = Nothing}
