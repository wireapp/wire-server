{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LoginCode_user where

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
testObject_LoginCode_user_1 :: LoginCode
testObject_LoginCode_user_1 = LoginCode {fromLoginCode = "I~W\DC14t\CAN\EMDQ+\993717\ETX\SUB\CAN\1028352>"}
testObject_LoginCode_user_2 :: LoginCode
testObject_LoginCode_user_2 = LoginCode {fromLoginCode = ".a5\180728})o%&\21007}\65396}^\DC4g\GS}>S\998074\1109058'\DLER\13527$\97107b\4838"}
testObject_LoginCode_user_3 :: LoginCode
testObject_LoginCode_user_3 = LoginCode {fromLoginCode = ",\SOH!\189793j\1028948\42036\ETBKPn\50539"}
testObject_LoginCode_user_4 :: LoginCode
testObject_LoginCode_user_4 = LoginCode {fromLoginCode = "Ho\t\n/aZ\vu\ETB:\1105245\47503=^T=\ESC\t\n\1111128\t0$"}
testObject_LoginCode_user_5 :: LoginCode
testObject_LoginCode_user_5 = LoginCode {fromLoginCode = ""}
testObject_LoginCode_user_6 :: LoginCode
testObject_LoginCode_user_6 = LoginCode {fromLoginCode = "$\137143\SO\189810(\1020957GeH\24281\vI\162444\f\1062135\US|\SUB"}
testObject_LoginCode_user_7 :: LoginCode
testObject_LoginCode_user_7 = LoginCode {fromLoginCode = "\1064152B\50940\r%\170439V"}
testObject_LoginCode_user_8 :: LoginCode
testObject_LoginCode_user_8 = LoginCode {fromLoginCode = "UL\27102`%Tf_\US2\187249"}
testObject_LoginCode_user_9 :: LoginCode
testObject_LoginCode_user_9 = LoginCode {fromLoginCode = "I#\1104346F3UJX\SOH\11454\1060841\1073886e\993450\ETB|\1058034\GSfZe9\1096825/\DC1IOwO"}
testObject_LoginCode_user_10 :: LoginCode
testObject_LoginCode_user_10 = LoginCode {fromLoginCode = "\ft\STX\NAK\DC3a/\1015669v.\a(s\EMX/|\1007193\143975n\NULD\1087874K^\1011599v\1006688\ESC\989245"}
testObject_LoginCode_user_11 :: LoginCode
testObject_LoginCode_user_11 = LoginCode {fromLoginCode = "8\ETX9}\1106191\RS\1083513K\r\1075759\NULm\DC1oU7L"}
testObject_LoginCode_user_12 :: LoginCode
testObject_LoginCode_user_12 = LoginCode {fromLoginCode = "y\1081737\94851J\27278\DLE\1094720\1074126\ACK\144350\45734e 0"}
testObject_LoginCode_user_13 :: LoginCode
testObject_LoginCode_user_13 = LoginCode {fromLoginCode = "rCTA3q:\US\DC4#T\99484BB`\RS\59873*u\12670F6X"}
testObject_LoginCode_user_14 :: LoginCode
testObject_LoginCode_user_14 = LoginCode {fromLoginCode = "pjGR\DLE\EOT\144029 a\CAN%7\1021034q\DC1\NAK\111048"}
testObject_LoginCode_user_15 :: LoginCode
testObject_LoginCode_user_15 = LoginCode {fromLoginCode = "\1096754\NAK-\r\998411\EMR\DLE\US#\"$\1027544\70466r&\EOT\1022522[\DLE\1099622i[v^p?"}
testObject_LoginCode_user_16 :: LoginCode
testObject_LoginCode_user_16 = LoginCode {fromLoginCode = "B\1075339 \1101825HL"}
testObject_LoginCode_user_17 :: LoginCode
testObject_LoginCode_user_17 = LoginCode {fromLoginCode = "H3-mq\97753|Z\1048539 \DC2"}
testObject_LoginCode_user_18 :: LoginCode
testObject_LoginCode_user_18 = LoginCode {fromLoginCode = "\n\ESCs\nf<\162625F\SOHh\190383\1105729\ENQ\1047932-;{\n"}
testObject_LoginCode_user_19 :: LoginCode
testObject_LoginCode_user_19 = LoginCode {fromLoginCode = "\48419\ACKMPyVv\NAK3t\21013@s\65579qI!6X\ETX"}
testObject_LoginCode_user_20 :: LoginCode
testObject_LoginCode_user_20 = LoginCode {fromLoginCode = "\DC1\1105836qBG\1002596\1085304p\184163p~HA\RS\140159\DELUGiZ-\a\ACK"}
