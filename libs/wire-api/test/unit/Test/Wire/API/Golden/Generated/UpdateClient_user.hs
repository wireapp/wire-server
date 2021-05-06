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
testObject_UpdateClient_user_1 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 3}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("\142268G")), updateClientLabel = Nothing}
testObject_UpdateClient_user_2 :: UpdateClient
testObject_UpdateClient_user_2 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Just (lastPrekey ("\DC37")), updateClientLabel = Nothing}
testObject_UpdateClient_user_3 :: UpdateClient
testObject_UpdateClient_user_3 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "n"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "s"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("v\DC2m\tq@Y\1033588")), updateClientLabel = Just ""}
testObject_UpdateClient_user_4 :: UpdateClient
testObject_UpdateClient_user_4 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\146114"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("D")), updateClientLabel = Just ":v\92741\131703Y\DC4#j"}
testObject_UpdateClient_user_5 :: UpdateClient
testObject_UpdateClient_user_5 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "Z"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\58747("}], updateClientLastKey = Just (lastPrekey ("\aC\DC2\1002712m\1016422\"\1006159C")), updateClientLabel = Just "\1006277\1034267\2097\33282u"}
testObject_UpdateClient_user_6 :: UpdateClient
testObject_UpdateClient_user_6 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "Y"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\985179"}], updateClientLastKey = Just (lastPrekey ("E6\100075U\1034148\ACK\1106722\RSh%")), updateClientLabel = Just ">\146204_>D'*"}
testObject_UpdateClient_user_7 :: UpdateClient
testObject_UpdateClient_user_7 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\GS^B"}], updateClientLastKey = Just (lastPrekey ("\1067467\NAK\1107240\1078597\DC3/W")), updateClientLabel = Just "\58479\SI"}
testObject_UpdateClient_user_8 :: UpdateClient
testObject_UpdateClient_user_8 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "l"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "K"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\r"}], updateClientLastKey = Just (lastPrekey ("t\1004426~\SO")), updateClientLabel = Nothing}
testObject_UpdateClient_user_9 :: UpdateClient
testObject_UpdateClient_user_9 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\v"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\FS"}], updateClientLastKey = Nothing, updateClientLabel = Just "\180705El\994831s\USKOz"}
testObject_UpdateClient_user_10 :: UpdateClient
testObject_UpdateClient_user_10 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "K\ESC"},Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "$:"}], updateClientLastKey = Just (lastPrekey ("\1066618]")), updateClientLabel = Just ""}
testObject_UpdateClient_user_11 :: UpdateClient
testObject_UpdateClient_user_11 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1044374C"}], updateClientLastKey = Just (lastPrekey ("]\170119\166882\&7\ACKE\DEL\STX")), updateClientLabel = Just "\b\FSTg\1033431"}
testObject_UpdateClient_user_12 :: UpdateClient
testObject_UpdateClient_user_12 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Just (lastPrekey ("\ETB\172726")), updateClientLabel = Just "\1009793+\135815H"}
testObject_UpdateClient_user_13 :: UpdateClient
testObject_UpdateClient_user_13 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "I"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "f"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("V%\f\DLE")), updateClientLabel = Just "pR\1034573$\ACK\STX\1026406g"}
testObject_UpdateClient_user_14 :: UpdateClient
testObject_UpdateClient_user_14 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\ETB"}], updateClientLastKey = Just (lastPrekey ("/K\ETBz")), updateClientLabel = Just "\1049223\186601E.\1043104"}
testObject_UpdateClient_user_15 :: UpdateClient
testObject_UpdateClient_user_15 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\f"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\\"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], updateClientLastKey = Nothing, updateClientLabel = Just "\140461H\95093\1047908\t6o"}
testObject_UpdateClient_user_16 :: UpdateClient
testObject_UpdateClient_user_16 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Nothing, updateClientLabel = Just "\24506\DC3\1036048q"}
testObject_UpdateClient_user_17 :: UpdateClient
testObject_UpdateClient_user_17 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Nothing, updateClientLabel = Nothing}
testObject_UpdateClient_user_18 :: UpdateClient
testObject_UpdateClient_user_18 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Just (lastPrekey ("c\1064124m\SOH")), updateClientLabel = Just "A\162360\&2Dkrx"}
testObject_UpdateClient_user_19 :: UpdateClient
testObject_UpdateClient_user_19 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Just (lastPrekey ("(oH9P\")")), updateClientLabel = Nothing}
testObject_UpdateClient_user_20 :: UpdateClient
testObject_UpdateClient_user_20 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1093885"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\6343"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\38287"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "N"}], updateClientLastKey = Just (lastPrekey ("")), updateClientLabel = Just "X"}
