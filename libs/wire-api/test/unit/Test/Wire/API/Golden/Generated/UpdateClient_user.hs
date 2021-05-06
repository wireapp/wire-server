{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UpdateClient_user where

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
testObject_UpdateClient_user_1 :: UpdateClient
testObject_UpdateClient_user_1 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("i\1063632m39\ESCw\63729")), updateClientLabel = Just "'\SYN"}
testObject_UpdateClient_user_2 :: UpdateClient
testObject_UpdateClient_user_2 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "H"},Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "Q~"}], updateClientLastKey = Nothing, updateClientLabel = Just "\23947\98776z*\r"}
testObject_UpdateClient_user_3 :: UpdateClient
testObject_UpdateClient_user_3 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Just (lastPrekey ("\nT")), updateClientLabel = Nothing}
testObject_UpdateClient_user_4 :: UpdateClient
testObject_UpdateClient_user_4 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\990959"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("}")), updateClientLabel = Nothing}
testObject_UpdateClient_user_5 :: UpdateClient
testObject_UpdateClient_user_5 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "4"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "?\1047593"}], updateClientLastKey = Just (lastPrekey ("\1052513\DC3")), updateClientLabel = Just "\990177\1004846W\"\1013565"}
testObject_UpdateClient_user_6 :: UpdateClient
testObject_UpdateClient_user_6 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 4}, prekeyKey = "\57717\ACK"}], updateClientLastKey = Just (lastPrekey ("\176454'^")), updateClientLabel = Just "oIu\SOH\987546L\1078496A<"}
testObject_UpdateClient_user_7 :: UpdateClient
testObject_UpdateClient_user_7 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("\ESC'\1080149r\21994\DEL")), updateClientLabel = Just "\1049247\NAKl\17599\US\132226\t^"}
testObject_UpdateClient_user_8 :: UpdateClient
testObject_UpdateClient_user_8 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Just (lastPrekey ("")), updateClientLabel = Just "0L\136389\\F"}
testObject_UpdateClient_user_9 :: UpdateClient
testObject_UpdateClient_user_9 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Just (lastPrekey ("1Q\USih\SYN\r")), updateClientLabel = Nothing}
testObject_UpdateClient_user_10 :: UpdateClient
testObject_UpdateClient_user_10 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ";"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1036886"}], updateClientLastKey = Just (lastPrekey ("\SOHx\1081563\CAN\1008450")), updateClientLabel = Nothing}
testObject_UpdateClient_user_11 :: UpdateClient
testObject_UpdateClient_user_11 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "<"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "K"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("ymQ#\GS\1010717")), updateClientLabel = Just "G\94313\1107146\62732r\b\50270X"}
testObject_UpdateClient_user_12 :: UpdateClient
testObject_UpdateClient_user_12 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\1019497"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "V"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("")), updateClientLabel = Nothing}
testObject_UpdateClient_user_13 :: UpdateClient
testObject_UpdateClient_user_13 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Just (lastPrekey ("\n")), updateClientLabel = Nothing}
testObject_UpdateClient_user_14 :: UpdateClient
testObject_UpdateClient_user_14 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey (".2,x-\ESCT\t\aa")), updateClientLabel = Just "\1090634\1068675\62234\1107178"}
testObject_UpdateClient_user_15 :: UpdateClient
testObject_UpdateClient_user_15 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("\4430E\150401\SO7\aQ")), updateClientLabel = Just "h"}
testObject_UpdateClient_user_16 :: UpdateClient
testObject_UpdateClient_user_16 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1005714"}], updateClientLastKey = Just (lastPrekey ("\rEb")), updateClientLabel = Just "\\;\1061664a\SIdw"}
testObject_UpdateClient_user_17 :: UpdateClient
testObject_UpdateClient_user_17 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "h"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\EOT"}], updateClientLastKey = Just (lastPrekey ("K\b")), updateClientLabel = Nothing}
testObject_UpdateClient_user_18 :: UpdateClient
testObject_UpdateClient_user_18 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("\33642\SI\1045709[\168216]KNt\94475")), updateClientLabel = Just "\DEL\b\17073\DC1"}
testObject_UpdateClient_user_19 :: UpdateClient
testObject_UpdateClient_user_19 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\137002"}], updateClientLastKey = Nothing, updateClientLabel = Just "\1033479\36019\rRWF"}
testObject_UpdateClient_user_20 :: UpdateClient
testObject_UpdateClient_user_20 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Just (lastPrekey ("\6759\32251\ETX\CAN\ah\110854\US\SYNz")), updateClientLabel = Just "\166474\SID"}
