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
testObject_Message_1 :: Message
testObject_Message_1 = Message {messageText = "$\179329\64079\DC2\DC3g\13609\1099747\faKL"}
testObject_Message_2 :: Message
testObject_Message_2 = Message {messageText = "X\44735DGv\128717\FSw\35065%\1035419y\DLE\aN|n!\CAN\DLEc\fJ\64353p\168049G7\143827\&46<\DC30\1011280i4\GS\nK\1096517Ls'\1015678(q\1060749ei\\\EOT}\164381[\RS=M\16069\1082625 ;\15517\DLE>\ETBk\ETB7r\1035710\157781Zx\NAK\1105948\ETBUmEG.BA!:\EOT\165676\&5\986007f\11344P"}
testObject_Message_3 :: Message
testObject_Message_3 = Message {messageText = "\CAN\993316\US%\26947\&7\983237'\1001197\NAKui>\160921D\31429V\1049514l\377o\997382@\1043881\1012570-X\f]\53549\DC2J;aJ!\EOT\1107939dlI\"8\v*\1085579\RS\t\FS\v\ESC\1052464\n\ACK&\162528O\51224]G \NUL-\1003558g_d\DEL\v\155299\1073891\155650\NUL//\1022450\1099865\29942\118828'M=\DEL"}
testObject_Message_4 :: Message
testObject_Message_4 = Message {messageText = "\ESCr\NUL3"}
testObject_Message_5 :: Message
testObject_Message_5 = Message {messageText = "R\"\NAK\149642\1099900\RSW\NAK\1109290k\12857\&2\39091Z\DC3\ENQPxbp\FS\1011926Y\181103\NAKZ3R#\SO\1060673\&6\189228t\187886E\RS Ym\36737+\1050146\EOTT\1280\111235y\v\STX&\ETX\DEL\RSwY\134983>B\1802Ad\1046293\1053701WU%H\988539\&0\165551D\1067457\1023418w\40703\SYN\1025294"}
