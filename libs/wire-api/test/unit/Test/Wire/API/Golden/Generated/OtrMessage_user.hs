{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.OtrMessage_user where

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
testObject_OtrMessage_user_1 :: OtrMessage
testObject_OtrMessage_user_1 = OtrMessage {otrSender = ClientId {client = "4"}, otrRecipient = ClientId {client = "1d"}, otrCiphertext = "\1005631q\t\1022517\152779", otrData = Nothing}
testObject_OtrMessage_user_2 :: OtrMessage
testObject_OtrMessage_user_2 = OtrMessage {otrSender = ClientId {client = "1"}, otrRecipient = ClientId {client = "1b"}, otrCiphertext = "\92572j", otrData = Just "VO-"}
testObject_OtrMessage_user_3 :: OtrMessage
testObject_OtrMessage_user_3 = OtrMessage {otrSender = ClientId {client = "12"}, otrRecipient = ClientId {client = "a"}, otrCiphertext = "t\US\1020348A\ESCS", otrData = Nothing}
testObject_OtrMessage_user_4 :: OtrMessage
testObject_OtrMessage_user_4 = OtrMessage {otrSender = ClientId {client = "15"}, otrRecipient = ClientId {client = "1e"}, otrCiphertext = "U8\v", otrData = Nothing}
testObject_OtrMessage_user_5 :: OtrMessage
testObject_OtrMessage_user_5 = OtrMessage {otrSender = ClientId {client = "10"}, otrRecipient = ClientId {client = "1d"}, otrCiphertext = "R\b+M=\157573", otrData = Just "\1075247S0"}
testObject_OtrMessage_user_6 :: OtrMessage
testObject_OtrMessage_user_6 = OtrMessage {otrSender = ClientId {client = "18"}, otrRecipient = ClientId {client = "8"}, otrCiphertext = "\140286\1016904", otrData = Just ")"}
testObject_OtrMessage_user_7 :: OtrMessage
testObject_OtrMessage_user_7 = OtrMessage {otrSender = ClientId {client = "13"}, otrRecipient = ClientId {client = "b"}, otrCiphertext = "\DC3\EM\41889\190292\180201", otrData = Just "\165984"}
testObject_OtrMessage_user_8 :: OtrMessage
testObject_OtrMessage_user_8 = OtrMessage {otrSender = ClientId {client = "19"}, otrRecipient = ClientId {client = "1c"}, otrCiphertext = "\194976\37162", otrData = Just "\t"}
testObject_OtrMessage_user_9 :: OtrMessage
testObject_OtrMessage_user_9 = OtrMessage {otrSender = ClientId {client = "1"}, otrRecipient = ClientId {client = "19"}, otrCiphertext = "SS\DEL\997302\1060829\&8S", otrData = Just "\t"}
testObject_OtrMessage_user_10 :: OtrMessage
testObject_OtrMessage_user_10 = OtrMessage {otrSender = ClientId {client = "8"}, otrRecipient = ClientId {client = "2"}, otrCiphertext = ",", otrData = Nothing}
testObject_OtrMessage_user_11 :: OtrMessage
testObject_OtrMessage_user_11 = OtrMessage {otrSender = ClientId {client = "20"}, otrRecipient = ClientId {client = "5"}, otrCiphertext = "Xv\1037680u", otrData = Just "Pn+$\SYNC\SOH"}
testObject_OtrMessage_user_12 :: OtrMessage
testObject_OtrMessage_user_12 = OtrMessage {otrSender = ClientId {client = "f"}, otrRecipient = ClientId {client = "1"}, otrCiphertext = "|}#\SOHk", otrData = Just "5\1074305\1005926\GS\ETB"}
testObject_OtrMessage_user_13 :: OtrMessage
testObject_OtrMessage_user_13 = OtrMessage {otrSender = ClientId {client = "19"}, otrRecipient = ClientId {client = "13"}, otrCiphertext = "\USe:\45052", otrData = Nothing}
testObject_OtrMessage_user_14 :: OtrMessage
testObject_OtrMessage_user_14 = OtrMessage {otrSender = ClientId {client = "2"}, otrRecipient = ClientId {client = "0"}, otrCiphertext = "\SI>", otrData = Just ""}
testObject_OtrMessage_user_15 :: OtrMessage
testObject_OtrMessage_user_15 = OtrMessage {otrSender = ClientId {client = "13"}, otrRecipient = ClientId {client = "1f"}, otrCiphertext = "aE\10200", otrData = Just ""}
testObject_OtrMessage_user_16 :: OtrMessage
testObject_OtrMessage_user_16 = OtrMessage {otrSender = ClientId {client = "0"}, otrRecipient = ClientId {client = "3"}, otrCiphertext = "", otrData = Nothing}
testObject_OtrMessage_user_17 :: OtrMessage
testObject_OtrMessage_user_17 = OtrMessage {otrSender = ClientId {client = "d"}, otrRecipient = ClientId {client = "1b"}, otrCiphertext = "m\SI\FS6", otrData = Just "\162024\&3\149542R"}
testObject_OtrMessage_user_18 :: OtrMessage
testObject_OtrMessage_user_18 = OtrMessage {otrSender = ClientId {client = "16"}, otrRecipient = ClientId {client = "6"}, otrCiphertext = "\133443\1087722^", otrData = Just "\148040\1113019"}
testObject_OtrMessage_user_19 :: OtrMessage
testObject_OtrMessage_user_19 = OtrMessage {otrSender = ClientId {client = "1d"}, otrRecipient = ClientId {client = "12"}, otrCiphertext = "", otrData = Just "xV\16344"}
testObject_OtrMessage_user_20 :: OtrMessage
testObject_OtrMessage_user_20 = OtrMessage {otrSender = ClientId {client = "20"}, otrRecipient = ClientId {client = "1b"}, otrCiphertext = "C\f\1646\1025393\SO-\f", otrData = Nothing}
