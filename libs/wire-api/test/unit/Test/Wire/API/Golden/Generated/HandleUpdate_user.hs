{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.HandleUpdate_user where

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
testObject_HandleUpdate_user_1 :: HandleUpdate
testObject_HandleUpdate_user_1 = HandleUpdate {huHandle = "U`M\b\993075\n\ENQ\1070055D&-\18285c\1010536$ONBS\DC416MH&M5\159020\1005547"}
testObject_HandleUpdate_user_2 :: HandleUpdate
testObject_HandleUpdate_user_2 = HandleUpdate {huHandle = "\110825\n?Iv\DC2\ENQ\NAK3;\a"}
testObject_HandleUpdate_user_3 :: HandleUpdate
testObject_HandleUpdate_user_3 = HandleUpdate {huHandle = "\DC4\63393]\FSd]}|\1042772\&4~\29616zXujye\RSir\1029112"}
testObject_HandleUpdate_user_4 :: HandleUpdate
testObject_HandleUpdate_user_4 = HandleUpdate {huHandle = "\EMk3\GS\161879!\SI\f"}
testObject_HandleUpdate_user_5 :: HandleUpdate
testObject_HandleUpdate_user_5 = HandleUpdate {huHandle = "\r0AA"}
testObject_HandleUpdate_user_6 :: HandleUpdate
testObject_HandleUpdate_user_6 = HandleUpdate {huHandle = "\6509^w\US\158055IT\158600\121255\129083-\1071581c|"}
testObject_HandleUpdate_user_7 :: HandleUpdate
testObject_HandleUpdate_user_7 = HandleUpdate {huHandle = "\f\1012934\&6\SYN=eh@\1087517V\139656\\\39851\1021347*\tN"}
testObject_HandleUpdate_user_8 :: HandleUpdate
testObject_HandleUpdate_user_8 = HandleUpdate {huHandle = "\1098518#\40889\27379$\DC3+ePxhW]'&"}
testObject_HandleUpdate_user_9 :: HandleUpdate
testObject_HandleUpdate_user_9 = HandleUpdate {huHandle = "\FSq\1015433/\ACKm\DEL\1081613D\ETB\GSR\1085758iuty["}
testObject_HandleUpdate_user_10 :: HandleUpdate
testObject_HandleUpdate_user_10 = HandleUpdate {huHandle = ""}
testObject_HandleUpdate_user_11 :: HandleUpdate
testObject_HandleUpdate_user_11 = HandleUpdate {huHandle = "\STXR\987513$"}
testObject_HandleUpdate_user_12 :: HandleUpdate
testObject_HandleUpdate_user_12 = HandleUpdate {huHandle = "a?\12641Pc\vt\SYN\1061095\186986g,>\tt0[K\142020\DC2\134865\150538x\1112580\128431"}
testObject_HandleUpdate_user_13 :: HandleUpdate
testObject_HandleUpdate_user_13 = HandleUpdate {huHandle = "\1093936UA5G\vB\64811\672\120615^"}
testObject_HandleUpdate_user_14 :: HandleUpdate
testObject_HandleUpdate_user_14 = HandleUpdate {huHandle = "K\n,\nYB"}
testObject_HandleUpdate_user_15 :: HandleUpdate
testObject_HandleUpdate_user_15 = HandleUpdate {huHandle = "\98937\SYN*\186533\161714\1107499Kl\GS\1036204+\SUB\13496K,\ACK\1098395$o"}
testObject_HandleUpdate_user_16 :: HandleUpdate
testObject_HandleUpdate_user_16 = HandleUpdate {huHandle = "\SOHV^p\SUB}\132989MF\1064161\STX"}
testObject_HandleUpdate_user_17 :: HandleUpdate
testObject_HandleUpdate_user_17 = HandleUpdate {huHandle = "G\"q\EMB-\1014891an\1086305k\SYN\1046991_\DC3G\SIb\"*[\1098558@nS"}
testObject_HandleUpdate_user_18 :: HandleUpdate
testObject_HandleUpdate_user_18 = HandleUpdate {huHandle = ">\163552dY1Ko\1090328o\r\EOT"}
testObject_HandleUpdate_user_19 :: HandleUpdate
testObject_HandleUpdate_user_19 = HandleUpdate {huHandle = "x:\GS"}
testObject_HandleUpdate_user_20 :: HandleUpdate
testObject_HandleUpdate_user_20 = HandleUpdate {huHandle = "bTte\ETB.u:Z\1057086W~\39878b\1063124\DC3`\1063874\SYNXE\ETB"}
