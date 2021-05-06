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
testObject_LoginCode_user_1 = LoginCode {fromLoginCode = "\169437\\'\152317&u4\DLECHC\14232"}
testObject_LoginCode_user_2 :: LoginCode
testObject_LoginCode_user_2 = LoginCode {fromLoginCode = "\1112501T<\1037768\"Z\1030852\ACK@V\SYN!\DEL\1008921\ETXF"}
testObject_LoginCode_user_3 :: LoginCode
testObject_LoginCode_user_3 = LoginCode {fromLoginCode = "\74131\ENQo"}
testObject_LoginCode_user_4 :: LoginCode
testObject_LoginCode_user_4 = LoginCode {fromLoginCode = "c\1105212jiAD G\DLEN7}\CAN)\"\990311\STX ^\f\166439|"}
testObject_LoginCode_user_5 :: LoginCode
testObject_LoginCode_user_5 = LoginCode {fromLoginCode = "\31037\SO\v\917862v.\1017158\1011350\DC2"}
testObject_LoginCode_user_6 :: LoginCode
testObject_LoginCode_user_6 = LoginCode {fromLoginCode = "^\46615QKP\95533\1030646\ESC\93803\DLE\1075591\179809\1087796\CANq@ve\"\55067\ESCv\n\CAND"}
testObject_LoginCode_user_7 :: LoginCode
testObject_LoginCode_user_7 = LoginCode {fromLoginCode = "\1048104x6>R{\62930"}
testObject_LoginCode_user_8 :: LoginCode
testObject_LoginCode_user_8 = LoginCode {fromLoginCode = "\173654\134709UY\38499@V}\SO\ESCz*\r\1033438\180629n\1030723\1070733\1062763"}
testObject_LoginCode_user_9 :: LoginCode
testObject_LoginCode_user_9 = LoginCode {fromLoginCode = "\ESCT\1051720H\EM@6\SUB\1067443\tSr\DC3\SI\1071433n6\ESCsr\RS\174398\ENQdm"}
testObject_LoginCode_user_10 :: LoginCode
testObject_LoginCode_user_10 = LoginCode {fromLoginCode = "sk,ka3M\DELj$br("}
testObject_LoginCode_user_11 :: LoginCode
testObject_LoginCode_user_11 = LoginCode {fromLoginCode = "\SOHpEQ\ETXH8\25342\SIx5{*"}
testObject_LoginCode_user_12 :: LoginCode
testObject_LoginCode_user_12 = LoginCode {fromLoginCode = "iUVw!V"}
testObject_LoginCode_user_13 :: LoginCode
testObject_LoginCode_user_13 = LoginCode {fromLoginCode = "j\63605x Sd\DC1\DC3@o\41426["}
testObject_LoginCode_user_14 :: LoginCode
testObject_LoginCode_user_14 = LoginCode {fromLoginCode = ""}
testObject_LoginCode_user_15 :: LoginCode
testObject_LoginCode_user_15 = LoginCode {fromLoginCode = "\t\ENQ\1008619rA\EM\166365d*\138312=\989392RW\1078216\1044366\NULA\159418\\&\156087"}
testObject_LoginCode_user_16 :: LoginCode
testObject_LoginCode_user_16 = LoginCode {fromLoginCode = "\42686\1106062Ixs\137712]O\t\ACK6`RFHZ\";\DEL!a\1071711\26742*\STX\15918"}
testObject_LoginCode_user_17 :: LoginCode
testObject_LoginCode_user_17 = LoginCode {fromLoginCode = "\SOH`>~\t\NUL\1106877B/?\155069\t'62"}
testObject_LoginCode_user_18 :: LoginCode
testObject_LoginCode_user_18 = LoginCode {fromLoginCode = "}\157140"}
testObject_LoginCode_user_19 :: LoginCode
testObject_LoginCode_user_19 = LoginCode {fromLoginCode = "\ACK\NAKJ"}
testObject_LoginCode_user_20 :: LoginCode
testObject_LoginCode_user_20 = LoginCode {fromLoginCode = "\1058070\33086\&1\1103878X\flFQ~\1097869n\1082137\ETXzF\NAK\142459\1113755,\1024014=\1063037\1025556q\ACK"}
