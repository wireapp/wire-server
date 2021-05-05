{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PushTokenList_user where

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
testObject_PushTokenList_1 :: PushTokenList
testObject_PushTokenList_1 = PushTokenList {pushTokens = [(pushToken (APNSVoIP) (AppName {appNameText = "\ESCv\DC2I\DC3"}) (Token {tokenText = "\DC3\170228&64\1061669"}) (ClientId {client = "f"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\"9"}) (Token {tokenText = "1\41184"}) (ClientId {client = "6"})),(pushToken (APNSSandbox) (AppName {appNameText = ""}) (Token {tokenText = "3\ACK\1068596"}) (ClientId {client = "18"})),(pushToken (APNSSandbox) (AppName {appNameText = ""}) (Token {tokenText = "\134746\DC3\1033339i"}) (ClientId {client = "20"})),(pushToken (GCM) (AppName {appNameText = "."}) (Token {tokenText = "ghN\136984\1028475"}) (ClientId {client = "d"})),(pushToken (APNS) (AppName {appNameText = ""}) (Token {tokenText = "}R\98201R<\41622"}) (ClientId {client = "1"})),(pushToken (APNSVoIP) (AppName {appNameText = "\159449\"S+\SO\1051766\1108499"}) (Token {tokenText = "c\139155"}) (ClientId {client = "1"})),(pushToken (APNSSandbox) (AppName {appNameText = "\DC38)\1080611"}) (Token {tokenText = "+"}) (ClientId {client = "3"})),(pushToken (APNSVoIP) (AppName {appNameText = "r\SI3V"}) (Token {tokenText = "~\STX\ENQV\994234\1111924\CAN"}) (ClientId {client = "1b"})),(pushToken (GCM) (AppName {appNameText = ""}) (Token {tokenText = "\DC1^\ESC\DC2FL\DC1"}) (ClientId {client = "1"})),(pushToken (APNSVoIP) (AppName {appNameText = "Z\2420j"}) (Token {tokenText = "\\\94986\\:\FSa\NUL"}) (ClientId {client = "13"})),(pushToken (APNS) (AppName {appNameText = "\\W"}) (Token {tokenText = "0 "}) (ClientId {client = "b"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "+{\SINPt\83512"}) (Token {tokenText = "\18648S\ACK\1051794"}) (ClientId {client = "1"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "Q\26442\1014727"}) (Token {tokenText = "\t"}) (ClientId {client = "c"})),(pushToken (APNS) (AppName {appNameText = ":\DC3q"}) (Token {tokenText = "\78637\40746\57405a<\bJ"}) (ClientId {client = "7"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "oR\991999\ESC"}) (Token {tokenText = ">\1089571\1069269"}) (ClientId {client = "6"})),(pushToken (APNS) (AppName {appNameText = "\DLE\ESC"}) (Token {tokenText = ""}) (ClientId {client = "1d"})),(pushToken (APNSSandbox) (AppName {appNameText = "O\DC3\1106724'z:["}) (Token {tokenText = "\152499S\r\""}) (ClientId {client = "1b"})),(pushToken (APNSSandbox) (AppName {appNameText = "\94608XlY\190206\1041007"}) (Token {tokenText = "gp\NUL"}) (ClientId {client = "0"})),(pushToken (APNSVoIP) (AppName {appNameText = "\32989"}) (Token {tokenText = "\SOH\159773\&1"}) (ClientId {client = "1d"})),(pushToken (APNSSandbox) (AppName {appNameText = "\RS\f"}) (Token {tokenText = "\DC3rq\FS\996213\ETX"}) (ClientId {client = "5"})),(pushToken (GCM) (AppName {appNameText = ""}) (Token {tokenText = "9!\ETBp"}) (ClientId {client = "19"})),(pushToken (APNSSandbox) (AppName {appNameText = "\EOT"}) (Token {tokenText = "^s\990827"}) (ClientId {client = "c"})),(pushToken (APNSSandbox) (AppName {appNameText = "D&\72201"}) (Token {tokenText = "DF\151178"}) (ClientId {client = "1d"}))]}
testObject_PushTokenList_2 :: PushTokenList
testObject_PushTokenList_2 = PushTokenList {pushTokens = [(pushToken (GCM) (AppName {appNameText = ";jb\1082699Y"}) (Token {tokenText = "*\STX\25512'\NUL"}) (ClientId {client = "10"})),(pushToken (APNS) (AppName {appNameText = "\1055719B"}) (Token {tokenText = "\DLEOB"}) (ClientId {client = "16"})),(pushToken (GCM) (AppName {appNameText = "F<"}) (Token {tokenText = "\US"}) (ClientId {client = "3"})),(pushToken (APNSSandbox) (AppName {appNameText = "7"}) (Token {tokenText = "\DC3"}) (ClientId {client = "10"})),(pushToken (GCM) (AppName {appNameText = "c"}) (Token {tokenText = ""}) (ClientId {client = "9"})),(pushToken (APNSSandbox) (AppName {appNameText = "\FS\1057626\46590i\DC2Y}"}) (Token {tokenText = "_ \50469"}) (ClientId {client = "14"})),(pushToken (GCM) (AppName {appNameText = "\1113658j\30607R\a-"}) (Token {tokenText = ""}) (ClientId {client = "1f"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\SOS"}) (Token {tokenText = "\43666\&0\SOHt\1038734Q4"}) (ClientId {client = "15"})),(pushToken (APNS) (AppName {appNameText = "09&L\GS\NAKt"}) (Token {tokenText = "\9972\13609hT\134120\RSr"}) (ClientId {client = "20"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "t"}) (Token {tokenText = "L\ACK\180450\175200@K"}) (ClientId {client = "18"})),(pushToken (APNS) (AppName {appNameText = "c\\R"}) (Token {tokenText = "\DEL"}) (ClientId {client = "f"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = ""}) (Token {tokenText = "\1089762 "}) (ClientId {client = "15"})),(pushToken (GCM) (AppName {appNameText = "\32054$Q\GS"}) (Token {tokenText = "\1097500"}) (ClientId {client = "17"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\US+\161414\&0"}) (Token {tokenText = "k\162226H"}) (ClientId {client = "1d"})),(pushToken (APNS) (AppName {appNameText = "\SYN\SOH\t,>W\ETX"}) (Token {tokenText = ""}) (ClientId {client = "9"})),(pushToken (APNS) (AppName {appNameText = ""}) (Token {tokenText = "\137470sb"}) (ClientId {client = "14"})),(pushToken (APNSVoIP) (AppName {appNameText = "YzW\1013913\RS\EOT"}) (Token {tokenText = "~"}) (ClientId {client = "2"})),(pushToken (GCM) (AppName {appNameText = "f\bR\1096799\164658"}) (Token {tokenText = "\t\62699R8C"}) (ClientId {client = "19"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "{\983489m"}) (Token {tokenText = "\f\1003290"}) (ClientId {client = "16"})),(pushToken (GCM) (AppName {appNameText = "8\FS6B\96738"}) (Token {tokenText = "\2307p\DLE\aI7\154361"}) (ClientId {client = "1a"})),(pushToken (APNS) (AppName {appNameText = "\28093\NAK}k\1024282"}) (Token {tokenText = "\26026M"}) (ClientId {client = "f"})),(pushToken (GCM) (AppName {appNameText = ""}) (Token {tokenText = "_\EOTc\fs"}) (ClientId {client = "1c"})),(pushToken (GCM) (AppName {appNameText = "\b\1075985"}) (Token {tokenText = "(\US\"\DEL2\120708"}) (ClientId {client = "15"}))]}
testObject_PushTokenList_3 :: PushTokenList
testObject_PushTokenList_3 = PushTokenList {pushTokens = [(pushToken (GCM) (AppName {appNameText = "a\43124\DLE\1039813\1104037&\1071726"}) (Token {tokenText = "\DLE[C5\3713"}) (ClientId {client = "8"}))]}
testObject_PushTokenList_4 :: PushTokenList
testObject_PushTokenList_4 = PushTokenList {pushTokens = [(pushToken (APNSSandbox) (AppName {appNameText = "\33928\NAKi\SOH\DC2"}) (Token {tokenText = "L\1066999\SUB"}) (ClientId {client = "19"}))]}
testObject_PushTokenList_5 :: PushTokenList
testObject_PushTokenList_5 = PushTokenList {pushTokens = [(pushToken (GCM) (AppName {appNameText = "Y\SO\DEL\STX"}) (Token {tokenText = "k\DC2\155404\&3c"}) (ClientId {client = "6"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = ""}) (Token {tokenText = "\46084\EOT\121475\3792'"}) (ClientId {client = "a"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\71178\1051319\&2t\1108789"}) (Token {tokenText = "S"}) (ClientId {client = "1d"})),(pushToken (GCM) (AppName {appNameText = "}\93812N\b\1010626_"}) (Token {tokenText = "\"hX7"}) (ClientId {client = "b"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = " \1081411"}) (Token {tokenText = "^ldY"}) (ClientId {client = "13"})),(pushToken (GCM) (AppName {appNameText = "h,\1002036"}) (Token {tokenText = "Nm\f\1036948W\165512\146091"}) (ClientId {client = "1d"})),(pushToken (GCM) (AppName {appNameText = ""}) (Token {tokenText = "\DC2z%-\1005586"}) (ClientId {client = "1b"})),(pushToken (APNSVoIP) (AppName {appNameText = "j\DLE\SI\1099631"}) (Token {tokenText = ""}) (ClientId {client = "1f"})),(pushToken (APNS) (AppName {appNameText = "+S(\NAK2"}) (Token {tokenText = "\32850"}) (ClientId {client = "1"})),(pushToken (GCM) (AppName {appNameText = "F\ETXJG-"}) (Token {tokenText = "=\SOS"}) (ClientId {client = "1f"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "l\11410Y\138535"}) (Token {tokenText = "\US\60457\f\RS|w"}) (ClientId {client = "c"})),(pushToken (GCM) (AppName {appNameText = "6\993211K\a\151593"}) (Token {tokenText = ""}) (ClientId {client = "b"}))]}
