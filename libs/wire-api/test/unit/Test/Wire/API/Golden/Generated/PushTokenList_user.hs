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
testObject_PushTokenList_user_1 :: PushTokenList
testObject_PushTokenList_user_1 = PushTokenList {pushTokens = [(pushToken (APNSVoIPSandbox) (AppName {appNameText = "]+su\1034528\SYN"}) (Token {tokenText = "\13480\1076239\SO\EM\1020570Ga"}) (ClientId {client = "1"})),(pushToken (GCM) (AppName {appNameText = "\41199<\16244\917609S\DC2"}) (Token {tokenText = ""}) (ClientId {client = "18"})),(pushToken (APNS) (AppName {appNameText = "T;"}) (Token {tokenText = "F["}) (ClientId {client = "6"})),(pushToken (APNSVoIP) (AppName {appNameText = "6\FS\171836%"}) (Token {tokenText = "\1060910(]\1110652\DC4\rq"}) (ClientId {client = "1"})),(pushToken (APNSVoIP) (AppName {appNameText = "ZC"}) (Token {tokenText = "\US\ACK"}) (ClientId {client = "1c"})),(pushToken (APNSSandbox) (AppName {appNameText = "\ETB\SOH"}) (Token {tokenText = "o}\ACK"}) (ClientId {client = "1f"})),(pushToken (APNSVoIP) (AppName {appNameText = "["}) (Token {tokenText = ":G\1039934"}) (ClientId {client = "5"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = ""}) (Token {tokenText = "\1096977_\63976~\1112968\1102400\NAK"}) (ClientId {client = "1d"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\DC2x\997032,D\FS"}) (Token {tokenText = "\1089175\12497D"}) (ClientId {client = "1b"})),(pushToken (GCM) (AppName {appNameText = ""}) (Token {tokenText = "z`H\CAN<"}) (ClientId {client = "1a"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\r\ESC\EM}q?\1076449"}) (Token {tokenText = "\159893LZ"}) (ClientId {client = "1a"})),(pushToken (APNSSandbox) (AppName {appNameText = ">p\CAN"}) (Token {tokenText = "_\nZ"}) (ClientId {client = "b"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "X\CAN\1095333\1007681\SOH"}) (Token {tokenText = "\97581\DC4\\"}) (ClientId {client = "a"})),(pushToken (APNSSandbox) (AppName {appNameText = "\bE\100216\ENQ y"}) (Token {tokenText = ""}) (ClientId {client = "1b"})),(pushToken (APNS) (AppName {appNameText = "\1060556L\186711\96614U\20919"}) (Token {tokenText = "S"}) (ClientId {client = "1c"})),(pushToken (GCM) (AppName {appNameText = ""}) (Token {tokenText = "~j1h<\DC4"}) (ClientId {client = "1b"})),(pushToken (APNS) (AppName {appNameText = "\11592}FT"}) (Token {tokenText = "\rL"}) (ClientId {client = "19"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = ""}) (Token {tokenText = "\b\ACK"}) (ClientId {client = "7"})),(pushToken (APNSVoIP) (AppName {appNameText = "\100965lR"}) (Token {tokenText = ""}) (ClientId {client = "1f"})),(pushToken (APNSSandbox) (AppName {appNameText = ""}) (Token {tokenText = "\t\DEL\47715\1018891\92766Y\CAN"}) (ClientId {client = "a"})),(pushToken (GCM) (AppName {appNameText = "\154515"}) (Token {tokenText = "$M92"}) (ClientId {client = "c"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\1030250/OW"}) (Token {tokenText = "t\DELA\1068558\NULW/"}) (ClientId {client = "4"})),(pushToken (APNS) (AppName {appNameText = "\178754\ESC\1026363|"}) (Token {tokenText = "\53589DN\183841\18201"}) (ClientId {client = "5"}))]}
testObject_PushTokenList_user_2 :: PushTokenList
testObject_PushTokenList_user_2 = PushTokenList {pushTokens = [(pushToken (APNSSandbox) (AppName {appNameText = "vG"}) (Token {tokenText = "\1059403Ox\1026469Sp"}) (ClientId {client = "1"})),(pushToken (APNSVoIP) (AppName {appNameText = "\1077860\1061481\987932\119067E1\1001747"}) (Token {tokenText = "\DC4"}) (ClientId {client = "5"})),(pushToken (APNS) (AppName {appNameText = "\127586\98592rZB"}) (Token {tokenText = "D\182813{;`\133817\f"}) (ClientId {client = "8"})),(pushToken (APNSVoIP) (AppName {appNameText = "r'L\564"}) (Token {tokenText = "1\DLE\DC3X\62605a"}) (ClientId {client = "e"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\1418"}) (Token {tokenText = "\19755A\1013274NSG"}) (ClientId {client = "1c"}))]}
testObject_PushTokenList_user_3 :: PushTokenList
testObject_PushTokenList_user_3 = PushTokenList {pushTokens = [(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\FS"}) (Token {tokenText = "~"}) (ClientId {client = "0"})),(pushToken (APNS) (AppName {appNameText = "P"}) (Token {tokenText = "\CAN\1063257\138622p\fT\f"}) (ClientId {client = "17"})),(pushToken (APNSSandbox) (AppName {appNameText = "\DC3U?"}) (Token {tokenText = "\1097294\SYN M"}) (ClientId {client = "17"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = ""}) (Token {tokenText = "6I;\180159"}) (ClientId {client = "8"})),(pushToken (APNSSandbox) (AppName {appNameText = "b4vc"}) (Token {tokenText = "r+&\DC2["}) (ClientId {client = "c"})),(pushToken (APNSSandbox) (AppName {appNameText = ""}) (Token {tokenText = "(\DLE\DLE\GS%\NAK"}) (ClientId {client = "1b"})),(pushToken (APNS) (AppName {appNameText = "\a7\EM\SII_"}) (Token {tokenText = "ZaEHp\1073076"}) (ClientId {client = "8"})),(pushToken (APNSVoIP) (AppName {appNameText = "h\GS\SOH'"}) (Token {tokenText = "DG#7\SO\b"}) (ClientId {client = "1a"})),(pushToken (APNSVoIP) (AppName {appNameText = "#\27665tF4\1063896"}) (Token {tokenText = "O"}) (ClientId {client = "1d"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "@J\STX[(O"}) (Token {tokenText = "\"=~\174518\US"}) (ClientId {client = "8"})),(pushToken (APNS) (AppName {appNameText = ""}) (Token {tokenText = "("}) (ClientId {client = "1c"})),(pushToken (GCM) (AppName {appNameText = "2l\CAN\SOH\ACK"}) (Token {tokenText = "R\SO\983826Ae\GSw"}) (ClientId {client = "8"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = ""}) (Token {tokenText = "b\165459"}) (ClientId {client = "20"})),(pushToken (GCM) (AppName {appNameText = "\992864.x\f\167032\ETB"}) (Token {tokenText = "Q\1057738\1019425Q\CAN"}) (ClientId {client = "7"})),(pushToken (GCM) (AppName {appNameText = "Z\1112753!\1100129l"}) (Token {tokenText = "-"}) (ClientId {client = "17"})),(pushToken (APNS) (AppName {appNameText = "\134901\161898"}) (Token {tokenText = "v\DC4O"}) (ClientId {client = "3"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = ";"}) (Token {tokenText = "\"2\22543"}) (ClientId {client = "18"})),(pushToken (GCM) (AppName {appNameText = "/"}) (Token {tokenText = "P+\138588TT.\FS"}) (ClientId {client = "1"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\DC4\156809\ENQ"}) (Token {tokenText = "a"}) (ClientId {client = "18"})),(pushToken (GCM) (AppName {appNameText = "?&\NULm\EOTT2"}) (Token {tokenText = "\994405\STX"}) (ClientId {client = "c"})),(pushToken (GCM) (AppName {appNameText = "Fn"}) (Token {tokenText = ".\DC1D\CAN\983652C"}) (ClientId {client = "e"}))]}
testObject_PushTokenList_user_4 :: PushTokenList
testObject_PushTokenList_user_4 = PushTokenList {pushTokens = [(pushToken (GCM) (AppName {appNameText = "\1024424&\186437\1018002"}) (Token {tokenText = "\110870\1005070W\4747\&7\149099"}) (ClientId {client = "1"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = ")\aG"}) (Token {tokenText = " "}) (ClientId {client = "8"})),(pushToken (APNS) (AppName {appNameText = "\153977q"}) (Token {tokenText = "D'"}) (ClientId {client = "1b"})),(pushToken (APNS) (AppName {appNameText = "\STX>h"}) (Token {tokenText = "y\ETBy\f9f7"}) (ClientId {client = "1"})),(pushToken (APNS) (AppName {appNameText = "\1048519nAY\ETBV\\"}) (Token {tokenText = "9%\FS\988517\985020\&8"}) (ClientId {client = "12"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "Gh\12136\96918\1104472\STX*"}) (Token {tokenText = ""}) (ClientId {client = "6"})),(pushToken (GCM) (AppName {appNameText = "\1026725;\t'\147421"}) (Token {tokenText = "\1066468\&2o\DC4)_K"}) (ClientId {client = "16"})),(pushToken (GCM) (AppName {appNameText = "M~"}) (Token {tokenText = "(I2"}) (ClientId {client = "11"})),(pushToken (GCM) (AppName {appNameText = "VO\DC2\179969.d\""}) (Token {tokenText = "\1034198"}) (ClientId {client = "10"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "2,"}) (Token {tokenText = "`G3\NUL"}) (ClientId {client = "19"}))]}
testObject_PushTokenList_user_5 :: PushTokenList
testObject_PushTokenList_user_5 = PushTokenList {pushTokens = [(pushToken (APNSSandbox) (AppName {appNameText = "R\1047773\162695\SO%\v\99997"}) (Token {tokenText = ""}) (ClientId {client = "0"})),(pushToken (APNSVoIP) (AppName {appNameText = "\68311x\RS=U"}) (Token {tokenText = ""}) (ClientId {client = "1a"})),(pushToken (GCM) (AppName {appNameText = "Y$("}) (Token {tokenText = "^8\a"}) (ClientId {client = "1a"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\GSz"}) (Token {tokenText = "3\1002604\&3j"}) (ClientId {client = "5"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\t2\174354\&34"}) (Token {tokenText = "s~>"}) (ClientId {client = "7"})),(pushToken (APNSSandbox) (AppName {appNameText = "\ACKg\DC1UT\DC3"}) (Token {tokenText = "-X\1056725"}) (ClientId {client = "15"})),(pushToken (APNSSandbox) (AppName {appNameText = "\rC"}) (Token {tokenText = "\159317@\f"}) (ClientId {client = "20"})),(pushToken (GCM) (AppName {appNameText = "\a.\1091217\NUL\98075%\1106387"}) (Token {tokenText = "r\NUL\ESC\CAN"}) (ClientId {client = "14"})),(pushToken (APNSSandbox) (AppName {appNameText = "~\1076646\1083416\1102671"}) (Token {tokenText = ""}) (ClientId {client = "6"})),(pushToken (APNS) (AppName {appNameText = "\60065\11910"}) (Token {tokenText = "\1032156\1062184"}) (ClientId {client = "1b"})),(pushToken (APNSSandbox) (AppName {appNameText = "\US\v\SUB\1056297\1100819\DC4\988553"}) (Token {tokenText = "|)\DLE"}) (ClientId {client = "11"}))]}
