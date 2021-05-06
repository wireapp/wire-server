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
testObject_PushTokenList_user_1 = PushTokenList {pushTokens = [(pushToken (APNSVoIP) (AppName {appNameText = "\SOH\55261G \166968\1075776"}) (Token {tokenText = "\DLE\48806P\rT\1074294"}) (ClientId {client = "13"})),(pushToken (GCM) (AppName {appNameText = "\1010625Q(B"}) (Token {tokenText = "g\EOTU?{["}) (ClientId {client = "1e"})),(pushToken (APNSVoIP) (AppName {appNameText = "_BA\aX"}) (Token {tokenText = "y ^z"}) (ClientId {client = "18"})),(pushToken (APNS) (AppName {appNameText = "nd\1028572\a\1049676j9"}) (Token {tokenText = "\SI\187188"}) (ClientId {client = "1e"})),(pushToken (GCM) (AppName {appNameText = "lvU\8001"}) (Token {tokenText = ""}) (ClientId {client = "13"})),(pushToken (APNSVoIP) (AppName {appNameText = "\NAK\DC4<\DC3"}) (Token {tokenText = "N\132441"}) (ClientId {client = "16"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = ""}) (Token {tokenText = "R"}) (ClientId {client = "c"})),(pushToken (APNSVoIP) (AppName {appNameText = "6\63660\NAK\1038998"}) (Token {tokenText = "7R\39931"}) (ClientId {client = "0"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "[#\"\EOTD"}) (Token {tokenText = "o*p"}) (ClientId {client = "f"})),(pushToken (APNSSandbox) (AppName {appNameText = "C\154558|,"}) (Token {tokenText = "\ETBO1[jX6"}) (ClientId {client = "3"})),(pushToken (APNSVoIP) (AppName {appNameText = "\13182\SUB\143819\45547\1016690G+"}) (Token {tokenText = "\SOH\19250"}) (ClientId {client = "f"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "^"}) (Token {tokenText = "m\182588\f"}) (ClientId {client = "7"})),(pushToken (APNSSandbox) (AppName {appNameText = "]=f\t "}) (Token {tokenText = "\1034109\ENQM"}) (ClientId {client = "16"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = ""}) (Token {tokenText = "J\141654S\\\1072616+"}) (ClientId {client = "12"})),(pushToken (APNSSandbox) (AppName {appNameText = "%\RSr\NULv'M"}) (Token {tokenText = ""}) (ClientId {client = "1"})),(pushToken (APNSVoIP) (AppName {appNameText = "\1029508_P\t~\50993 "}) (Token {tokenText = "I0"}) (ClientId {client = "9"}))]}
testObject_PushTokenList_user_2 :: PushTokenList
testObject_PushTokenList_user_2 = PushTokenList {pushTokens = [(pushToken (APNS) (AppName {appNameText = "\1053233\992556y"}) (Token {tokenText = "B"}) (ClientId {client = "1c"})),(pushToken (APNS) (AppName {appNameText = "x"}) (Token {tokenText = "\13702\r\NAK"}) (ClientId {client = "11"})),(pushToken (APNSVoIP) (AppName {appNameText = ">\1054776"}) (Token {tokenText = "\US\151480"}) (ClientId {client = "20"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "%wY"}) (Token {tokenText = "U\1047324L4K"}) (ClientId {client = "a"})),(pushToken (APNSVoIP) (AppName {appNameText = "1ig\190626"}) (Token {tokenText = "/Ab0"}) (ClientId {client = "1f"})),(pushToken (APNSVoIP) (AppName {appNameText = ""}) (Token {tokenText = "\DC3\1043703"}) (ClientId {client = "14"})),(pushToken (APNSSandbox) (AppName {appNameText = "7\1050945S\1047873j"}) (Token {tokenText = "\DC34%\1081761\1099550"}) (ClientId {client = "16"})),(pushToken (APNS) (AppName {appNameText = "\DC3\EM\\\1103792\&22"}) (Token {tokenText = "GqI\RS-"}) (ClientId {client = "c"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\ENQ\"(\DC4*L"}) (Token {tokenText = ""}) (ClientId {client = "1c"})),(pushToken (GCM) (AppName {appNameText = "\b"}) (Token {tokenText = ""}) (ClientId {client = "9"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\1047654"}) (Token {tokenText = "b\65191<\188162\&28"}) (ClientId {client = "2"})),(pushToken (APNSVoIP) (AppName {appNameText = "\DC2"}) (Token {tokenText = "\DC1\ACK"}) (ClientId {client = "2"})),(pushToken (APNSVoIP) (AppName {appNameText = "W^\DEL\n"}) (Token {tokenText = "#<1P("}) (ClientId {client = "5"})),(pushToken (APNSSandbox) (AppName {appNameText = "\fqZ"}) (Token {tokenText = "\67988`J"}) (ClientId {client = "1"})),(pushToken (APNSSandbox) (AppName {appNameText = ""}) (Token {tokenText = "T1D"}) (ClientId {client = "e"})),(pushToken (GCM) (AppName {appNameText = "\DC2Q\1086824\&7G"}) (Token {tokenText = "\SIPl"}) (ClientId {client = "f"})),(pushToken (APNSVoIP) (AppName {appNameText = ""}) (Token {tokenText = "3sp"}) (ClientId {client = "1c"})),(pushToken (APNS) (AppName {appNameText = "c\SOHS\185140\&5p"}) (Token {tokenText = "\41865X\1088372y\FSQ"}) (ClientId {client = "2"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = ""}) (Token {tokenText = "]"}) (ClientId {client = "d"})),(pushToken (APNSSandbox) (AppName {appNameText = "\992089l"}) (Token {tokenText = "m"}) (ClientId {client = "17"})),(pushToken (APNSVoIP) (AppName {appNameText = "a@S"}) (Token {tokenText = ")\a\139225r"}) (ClientId {client = "f"})),(pushToken (APNSVoIP) (AppName {appNameText = "m\\\187104\bx"}) (Token {tokenText = "Tr\1049146D"}) (ClientId {client = "2"})),(pushToken (APNS) (AppName {appNameText = "$)\DC3\144173,\96252"}) (Token {tokenText = "nW\ESC\177043\SOH\\"}) (ClientId {client = "d"})),(pushToken (APNSSandbox) (AppName {appNameText = "'\985656"}) (Token {tokenText = "\DEL\RSv?\40243"}) (ClientId {client = "0"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "a"}) (Token {tokenText = "\52385\1105789\119619\165740"}) (ClientId {client = "14"})),(pushToken (GCM) (AppName {appNameText = "W"}) (Token {tokenText = "\147816Mw|h_G"}) (ClientId {client = "1a"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "5XF\v"}) (Token {tokenText = "\1087011"}) (ClientId {client = "1b"})),(pushToken (APNS) (AppName {appNameText = "\1030825\1087171J"}) (Token {tokenText = "3\EOT"}) (ClientId {client = "2"}))]}
testObject_PushTokenList_user_3 :: PushTokenList
testObject_PushTokenList_user_3 = PushTokenList {pushTokens = [(pushToken (APNSVoIP) (AppName {appNameText = "p)"}) (Token {tokenText = "yO"}) (ClientId {client = "d"})),(pushToken (APNSVoIP) (AppName {appNameText = "\37793\DEL\r\DC3"}) (Token {tokenText = "@'b-!"}) (ClientId {client = "2"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\t\182695\47075\a"}) (Token {tokenText = "Jm"}) (ClientId {client = "1e"})),(pushToken (GCM) (AppName {appNameText = "L"}) (Token {tokenText = "YJ\DC1iW\50589\997013"}) (ClientId {client = "1f"})),(pushToken (APNS) (AppName {appNameText = "\48486\SOH1*\53112j"}) (Token {tokenText = "?\tm\180766N\v"}) (ClientId {client = "d"})),(pushToken (APNSVoIP) (AppName {appNameText = "H\ACK\37751S_"}) (Token {tokenText = "\145137\STX8\44009"}) (ClientId {client = "b"})),(pushToken (APNS) (AppName {appNameText = "\1051743\NUL\t\994200\1036201R"}) (Token {tokenText = "e"}) (ClientId {client = "1a"})),(pushToken (GCM) (AppName {appNameText = ""}) (Token {tokenText = "-s\162492L\1017452&"}) (ClientId {client = "14"})),(pushToken (APNSSandbox) (AppName {appNameText = "-\1026605\1070209\49766"}) (Token {tokenText = ""}) (ClientId {client = "a"}))]}
testObject_PushTokenList_user_4 :: PushTokenList
testObject_PushTokenList_user_4 = PushTokenList {pushTokens = [(pushToken (APNSVoIP) (AppName {appNameText = "e\1010711\&4Q\132565V\CAN"}) (Token {tokenText = "HaX\175844\&5oS"}) (ClientId {client = "13"})),(pushToken (GCM) (AppName {appNameText = "m"}) (Token {tokenText = "Y\DC2"}) (ClientId {client = "8"})),(pushToken (APNSSandbox) (AppName {appNameText = ""}) (Token {tokenText = "\FS\ETXW\151080d"}) (ClientId {client = "14"})),(pushToken (APNSSandbox) (AppName {appNameText = ""}) (Token {tokenText = "\"F^B"}) (ClientId {client = "e"})),(pushToken (APNSVoIP) (AppName {appNameText = ""}) (Token {tokenText = "9\129127\SUB\v"}) (ClientId {client = "1d"})),(pushToken (APNSVoIP) (AppName {appNameText = "\EM'j"}) (Token {tokenText = "p\DC3z\1062149\f"}) (ClientId {client = "0"})),(pushToken (APNSSandbox) (AppName {appNameText = "lp\1023162\1083807\1104666\v"}) (Token {tokenText = "\153813&"}) (ClientId {client = "12"})),(pushToken (GCM) (AppName {appNameText = "2f\"G\""}) (Token {tokenText = "\ACKED'\74060"}) (ClientId {client = "16"})),(pushToken (APNS) (AppName {appNameText = "'\145971\r"}) (Token {tokenText = "\v^\27926|Ya"}) (ClientId {client = "c"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\STX\2276\DC4"}) (Token {tokenText = ""}) (ClientId {client = "0"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "dh\CANBtjU"}) (Token {tokenText = "\DC1g"}) (ClientId {client = "e"})),(pushToken (APNSSandbox) (AppName {appNameText = "-"}) (Token {tokenText = "(\GS0w\1109937k"}) (ClientId {client = "11"})),(pushToken (APNSSandbox) (AppName {appNameText = "yF\SO\168457\&7\DLE+"}) (Token {tokenText = "$"}) (ClientId {client = "10"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "\US\STX$s\EOTh"}) (Token {tokenText = "A&"}) (ClientId {client = "12"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "Q\60027\SOHgI"}) (Token {tokenText = "\FS!\166340BK"}) (ClientId {client = "6"})),(pushToken (APNS) (AppName {appNameText = "\30274\SYN\DC4nA\136975h"}) (Token {tokenText = "\RS"}) (ClientId {client = "8"})),(pushToken (APNS) (AppName {appNameText = "8u"}) (Token {tokenText = "\1045120\1030978\"\f\993678\1043219"}) (ClientId {client = "f"})),(pushToken (GCM) (AppName {appNameText = "\DC3\rf\ACK"}) (Token {tokenText = "\1084061\991558M\fx"}) (ClientId {client = "1e"})),(pushToken (GCM) (AppName {appNameText = "N\DLEB\CAN\GS"}) (Token {tokenText = "\DLE( s\189292\5005b"}) (ClientId {client = "b"})),(pushToken (GCM) (AppName {appNameText = "\96868P\141852"}) (Token {tokenText = "r`"}) (ClientId {client = "c"})),(pushToken (APNS) (AppName {appNameText = "hI\43416"}) (Token {tokenText = "{nj\EOTS\53247"}) (ClientId {client = "1b"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "P\t\US"}) (Token {tokenText = "\22950"}) (ClientId {client = "18"})),(pushToken (APNSVoIP) (AppName {appNameText = ""}) (Token {tokenText = "\37953"}) (ClientId {client = "1b"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "f"}) (Token {tokenText = "n\14949\DLE\"\142811)\ETX"}) (ClientId {client = "1"})),(pushToken (APNSSandbox) (AppName {appNameText = "\119013"}) (Token {tokenText = "Mqb\1091651f\ESC\180071"}) (ClientId {client = "5"})),(pushToken (GCM) (AppName {appNameText = "1"}) (Token {tokenText = ""}) (ClientId {client = "20"})),(pushToken (GCM) (AppName {appNameText = ""}) (Token {tokenText = "w\1055904"}) (ClientId {client = "11"})),(pushToken (GCM) (AppName {appNameText = "u"}) (Token {tokenText = "_[\21222V\v"}) (ClientId {client = "9"})),(pushToken (APNS) (AppName {appNameText = "\164529fY\156342M\1085099"}) (Token {tokenText = "\1072922\78427<"}) (ClientId {client = "1d"}))]}
testObject_PushTokenList_user_5 :: PushTokenList
testObject_PushTokenList_user_5 = PushTokenList {pushTokens = [(pushToken (APNSVoIPSandbox) (AppName {appNameText = ""}) (Token {tokenText = "|b\ENQ\DEL"}) (ClientId {client = "15"})),(pushToken (GCM) (AppName {appNameText = "M)"}) (Token {tokenText = "\v]b"}) (ClientId {client = "20"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = ""}) (Token {tokenText = "_AB\SUBbq"}) (ClientId {client = "4"})),(pushToken (GCM) (AppName {appNameText = "\1039528L\DC1\161516"}) (Token {tokenText = ""}) (ClientId {client = "12"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "az"}) (Token {tokenText = "&:\""}) (ClientId {client = "1a"})),(pushToken (APNSVoIP) (AppName {appNameText = "h*:"}) (Token {tokenText = "E{O"}) (ClientId {client = "f"})),(pushToken (APNSSandbox) (AppName {appNameText = "\1036678\53592djg"}) (Token {tokenText = "]h\NULi"}) (ClientId {client = "12"})),(pushToken (APNSVoIP) (AppName {appNameText = "\US"}) (Token {tokenText = "MB"}) (ClientId {client = "1a"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "_\DEL\158972<\35280"}) (Token {tokenText = "\1065225pV\EOT\54309"}) (ClientId {client = "6"})),(pushToken (APNSVoIP) (AppName {appNameText = "9#\62581\188259\22558\1078730\DC3"}) (Token {tokenText = "nH\984047\1109651"}) (ClientId {client = "4"})),(pushToken (GCM) (AppName {appNameText = "\1058843\1079872"}) (Token {tokenText = "\1110566@YF\NAK\rO"}) (ClientId {client = "6"})),(pushToken (APNSVoIP) (AppName {appNameText = "\1049980\1105966\1079999v"}) (Token {tokenText = "D\1048441@\ENQPza"}) (ClientId {client = "8"})),(pushToken (APNSSandbox) (AppName {appNameText = "\NAK\a7![\SUB-"}) (Token {tokenText = "\1022085\994359U\164669"}) (ClientId {client = "c"})),(pushToken (APNSSandbox) (AppName {appNameText = "q\1074512\165264"}) (Token {tokenText = ""}) (ClientId {client = "16"})),(pushToken (APNSVoIP) (AppName {appNameText = "\DELvI"}) (Token {tokenText = "tCd"}) (ClientId {client = "13"})),(pushToken (APNSVoIP) (AppName {appNameText = "P8_TQ"}) (Token {tokenText = "\1002281\1086522\STX"}) (ClientId {client = "12"})),(pushToken (APNSVoIP) (AppName {appNameText = ""}) (Token {tokenText = ""}) (ClientId {client = "8"})),(pushToken (GCM) (AppName {appNameText = "\ACK\1024701i\ESC"}) (Token {tokenText = "\1012727\97913\\\US_"}) (ClientId {client = "16"})),(pushToken (APNSVoIP) (AppName {appNameText = "y"}) (Token {tokenText = ""}) (ClientId {client = "20"})),(pushToken (APNSSandbox) (AppName {appNameText = "\1110692"}) (Token {tokenText = "?3\DC2\100711\&6\983873\r"}) (ClientId {client = "3"})),(pushToken (GCM) (AppName {appNameText = ""}) (Token {tokenText = "S\1054583\1052590\1002450"}) (ClientId {client = "3"})),(pushToken (APNSVoIP) (AppName {appNameText = "A"}) (Token {tokenText = "\SO\163268\&9W"}) (ClientId {client = "0"})),(pushToken (APNSSandbox) (AppName {appNameText = "d\EOT"}) (Token {tokenText = "'~\SUBV"}) (ClientId {client = "11"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "`4K"}) (Token {tokenText = "Y\1095477\SOHDkN"}) (ClientId {client = "e"})),(pushToken (APNSVoIPSandbox) (AppName {appNameText = "J]\DC1\20358C\CAN"}) (Token {tokenText = "\986961\DEL\172898"}) (ClientId {client = "7"}))]}
