{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Message_user where

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
testObject_Message_user_1 :: Message
testObject_Message_user_1 = Message {messageText = "\49759\b\DC2~\SYN\1109078\&3\1021012_"}
testObject_Message_user_2 :: Message
testObject_Message_user_2 = Message {messageText = "\134609#\f\44349\\\ACKt\DC4\53092\170757\NAK|t\ETB\t\173365\6729{\63165\STX;\SYN\1035558\164055\997763\ftM\1040777\28541+\DEL2\179334\DC20!\1084803fi_\"B\NUL'\181522\10307\STXwr\1111861]W\CANe\1034569.@\a|9\SI\36597QR\ACK)\1018805\SIL6\133337`I4\DEL\CANe\47551h\EOT\STX_b\f\RSEm\STXl\132166\tR!\1083520\SUB\44160Rm,\EOT\1086705\t|o\182606J\683%|AG\1046816\1053335\bYi\1030297C\1082056w\1045508\&2\1056400\v4\147837YM\6590\SOHD\1026820\&8\1042067\STX\38269t\ACK\144030\&2\18573D\t\184868\&9\DC3\57430\1065865\SOH\a\DC46\60710K\154429Ux_=Sd\SYN\1037961W<Rb\1087118/)\ENQb\aC.\51834\&0"}
testObject_Message_user_3 :: Message
testObject_Message_user_3 = Message {messageText = "B\DEL\SI7\SYNX\US\1086707+wC\72144\CAN,\EM\ESCE\34625l\ETX\191340\ESC/f:U\24568\SO5\165536+\1069723#]Vl*:.\RSW\1059651\ACKV\RSV8V}\DC27\FSzz\23580U\995500U\1043258\SI)T\163996V5}2q\f\ACK\a\ETB\au\30108S\ESC:[ o\155640\1002730T<\1040402\DC1\1068161\DC2;\82952V\1027939O(.l"}
testObject_Message_user_4 :: Message
testObject_Message_user_4 = Message {messageText = "\aDBPZ\1102402\rodi\144669nBh3U\62973E&T\1032002\b\STX[\"7K\vH\1082051\135603A\ETBzb\1086431|)2Iv,\ETX?p|\8402[\179009L\176176g\1078768=\152993r\135256\148200f\1075165I\NAKV\1050204yd\ENQrH\DC31\DLE\1040225>\v}\CANYf\166240\v!\63550N6R\41251\ENQ\US\RS@\"S\ESCn\f\35909\&6;y(\f\tS'\ETX\b\DEL\SUB\1006195\1025225wg)%lwH\NAKv\DC3vM\1068747\1111086\NAK\50740Mbz\187556t53h@6\DC1O\f\51672>q\136311D\DLE\b\1000959tbNH\ENQ\1024748V7\CAN"}
testObject_Message_user_5 :: Message
testObject_Message_user_5 = Message {messageText = "\SOM\1102210\DLEAf\CAN\GSO\DC3Y\60777m\1103850Z\5403\163946M*{4X\1022527\ACK[kq \ENQk\1107817W"}
