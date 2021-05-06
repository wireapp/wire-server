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
testObject_UpdateClient_user_1 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\68815"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\SI"}], updateClientLastKey = Just (lastPrekey ("9'Renk\1082338q")), updateClientLabel = Just "s\45068\41561V\RS\ACKQ\\6:"}
testObject_UpdateClient_user_2 :: UpdateClient
testObject_UpdateClient_user_2 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\b"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "h"}], updateClientLastKey = Just (lastPrekey ("\SI\"\ACKj\175047")), updateClientLabel = Just "{"}
testObject_UpdateClient_user_3 :: UpdateClient
testObject_UpdateClient_user_3 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "Z"},Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "6E"}], updateClientLastKey = Just (lastPrekey ("\ACK?I\1069429")), updateClientLabel = Just "TJ\1074197\SO\ETXT]"}
testObject_UpdateClient_user_4 :: UpdateClient
testObject_UpdateClient_user_4 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], updateClientLastKey = Nothing, updateClientLabel = Just "\133884\n7\SI#\SYNg\30839"}
testObject_UpdateClient_user_5 :: UpdateClient
testObject_UpdateClient_user_5 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Just (lastPrekey ("\64438\&1")), updateClientLabel = Just "\"X\1057758\SI<G"}
testObject_UpdateClient_user_6 :: UpdateClient
testObject_UpdateClient_user_6 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Just (lastPrekey ("C^*e\184334\1029621\DC2\EM\1096531")), updateClientLabel = Just "\EM\1061820\"X["}
testObject_UpdateClient_user_7 :: UpdateClient
testObject_UpdateClient_user_7 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("")), updateClientLabel = Just "\t\10161\""}
testObject_UpdateClient_user_8 :: UpdateClient
testObject_UpdateClient_user_8 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 4}, prekeyKey = "q|Q\131557\1013751"}], updateClientLastKey = Just (lastPrekey ("s\ETX)\1050142\1105369\&8\SYN")), updateClientLabel = Just "C\1080756X\STX)&FM;\DC1"}
testObject_UpdateClient_user_9 :: UpdateClient
testObject_UpdateClient_user_9 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\65623"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1049876="}], updateClientLastKey = Nothing, updateClientLabel = Just "<\1097549vm"}
testObject_UpdateClient_user_10 :: UpdateClient
testObject_UpdateClient_user_10 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Just (lastPrekey ("L!6f\ACK3$\157016")), updateClientLabel = Just ""}
testObject_UpdateClient_user_11 :: UpdateClient
testObject_UpdateClient_user_11 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Just (lastPrekey (".Kw\GS:\DC3")), updateClientLabel = Just "\fv\f2\156669\&9"}
testObject_UpdateClient_user_12 :: UpdateClient
testObject_UpdateClient_user_12 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "<"},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "J"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\36736"}], updateClientLastKey = Just (lastPrekey ("\1099110\1011989\DC1")), updateClientLabel = Just "\34691\46298\GS "}
testObject_UpdateClient_user_13 :: UpdateClient
testObject_UpdateClient_user_13 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("\EOT\b)R")), updateClientLabel = Just "(\1067751\38816\21356"}
testObject_UpdateClient_user_14 :: UpdateClient
testObject_UpdateClient_user_14 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Just (lastPrekey ("")), updateClientLabel = Nothing}
testObject_UpdateClient_user_15 :: UpdateClient
testObject_UpdateClient_user_15 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\DLE"},Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("\1038690\50622@2\65294\144336")), updateClientLabel = Just "{\1076182"}
testObject_UpdateClient_user_16 :: UpdateClient
testObject_UpdateClient_user_16 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("")), updateClientLabel = Just "A*\33287%|\US\171370R?%"}
testObject_UpdateClient_user_17 :: UpdateClient
testObject_UpdateClient_user_17 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], updateClientLastKey = Nothing, updateClientLabel = Nothing}
testObject_UpdateClient_user_18 :: UpdateClient
testObject_UpdateClient_user_18 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\1032868"},Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\1006733\1088103"}], updateClientLastKey = Just (lastPrekey ("\SUB\1082268")), updateClientLabel = Just "7\1031145H"}
testObject_UpdateClient_user_19 :: UpdateClient
testObject_UpdateClient_user_19 = UpdateClient {updateClientPrekeys = [], updateClientLastKey = Just (lastPrekey ("\RSP\CAN")), updateClientLabel = Just "\1022444\NULL\1000857"}
testObject_UpdateClient_user_20 :: UpdateClient
testObject_UpdateClient_user_20 = UpdateClient {updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}], updateClientLastKey = Just (lastPrekey ("\n'\DC2(\61809\142288T-f\23238")), updateClientLabel = Just "$\STX\97414\19262j^\v"}
