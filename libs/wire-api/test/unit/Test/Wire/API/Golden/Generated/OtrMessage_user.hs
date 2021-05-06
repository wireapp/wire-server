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
testObject_OtrMessage_user_1 = OtrMessage {otrSender = ClientId {client = "a"}, otrRecipient = ClientId {client = "10"}, otrCiphertext = "jL7", otrData = Just "sP\151419\1049551*(:"}
testObject_OtrMessage_user_2 :: OtrMessage
testObject_OtrMessage_user_2 = OtrMessage {otrSender = ClientId {client = "4"}, otrRecipient = ClientId {client = "3"}, otrCiphertext = "", otrData = Just "\a"}
testObject_OtrMessage_user_3 :: OtrMessage
testObject_OtrMessage_user_3 = OtrMessage {otrSender = ClientId {client = "d"}, otrRecipient = ClientId {client = "13"}, otrCiphertext = "M\no[\1016191Z", otrData = Just "\1024540[V\"\ACK#"}
testObject_OtrMessage_user_4 :: OtrMessage
testObject_OtrMessage_user_4 = OtrMessage {otrSender = ClientId {client = "1b"}, otrRecipient = ClientId {client = "12"}, otrCiphertext = "\1003313", otrData = Just "@\DC1\DC2XNp\110978"}
testObject_OtrMessage_user_5 :: OtrMessage
testObject_OtrMessage_user_5 = OtrMessage {otrSender = ClientId {client = "1b"}, otrRecipient = ClientId {client = "1a"}, otrCiphertext = "M\1053768", otrData = Just "-\1088754"}
testObject_OtrMessage_user_6 :: OtrMessage
testObject_OtrMessage_user_6 = OtrMessage {otrSender = ClientId {client = "1b"}, otrRecipient = ClientId {client = "c"}, otrCiphertext = "", otrData = Just "x\\"}
testObject_OtrMessage_user_7 :: OtrMessage
testObject_OtrMessage_user_7 = OtrMessage {otrSender = ClientId {client = "13"}, otrRecipient = ClientId {client = "1d"}, otrCiphertext = "\SOH\f\SYNMJx`", otrData = Just "\1105919\178719we\53330\&0"}
testObject_OtrMessage_user_8 :: OtrMessage
testObject_OtrMessage_user_8 = OtrMessage {otrSender = ClientId {client = "7"}, otrRecipient = ClientId {client = "2"}, otrCiphertext = "o\ESCuN3\NUL", otrData = Just "\SOH\1087550 k"}
testObject_OtrMessage_user_9 :: OtrMessage
testObject_OtrMessage_user_9 = OtrMessage {otrSender = ClientId {client = "18"}, otrRecipient = ClientId {client = "15"}, otrCiphertext = "\DC3", otrData = Just "\1058628\1034618"}
testObject_OtrMessage_user_10 :: OtrMessage
testObject_OtrMessage_user_10 = OtrMessage {otrSender = ClientId {client = "1a"}, otrRecipient = ClientId {client = "13"}, otrCiphertext = "\142137\\sYu\US\b", otrData = Just "Uc"}
testObject_OtrMessage_user_11 :: OtrMessage
testObject_OtrMessage_user_11 = OtrMessage {otrSender = ClientId {client = "f"}, otrRecipient = ClientId {client = "f"}, otrCiphertext = "\74961V\149468\188704\65066", otrData = Just "\20594j\SUB"}
testObject_OtrMessage_user_12 :: OtrMessage
testObject_OtrMessage_user_12 = OtrMessage {otrSender = ClientId {client = "11"}, otrRecipient = ClientId {client = "0"}, otrCiphertext = "R\ETXS>6", otrData = Just "\")_\EM"}
testObject_OtrMessage_user_13 :: OtrMessage
testObject_OtrMessage_user_13 = OtrMessage {otrSender = ClientId {client = "10"}, otrRecipient = ClientId {client = "e"}, otrCiphertext = "\1022178/\48672#", otrData = Just "\f\af\1001561"}
testObject_OtrMessage_user_14 :: OtrMessage
testObject_OtrMessage_user_14 = OtrMessage {otrSender = ClientId {client = "1d"}, otrRecipient = ClientId {client = "4"}, otrCiphertext = "", otrData = Just "\100387'\1023258"}
testObject_OtrMessage_user_15 :: OtrMessage
testObject_OtrMessage_user_15 = OtrMessage {otrSender = ClientId {client = "3"}, otrRecipient = ClientId {client = "12"}, otrCiphertext = "^N", otrData = Just "\r =V\DC4"}
testObject_OtrMessage_user_16 :: OtrMessage
testObject_OtrMessage_user_16 = OtrMessage {otrSender = ClientId {client = "16"}, otrRecipient = ClientId {client = "1c"}, otrCiphertext = "J3\1039559.", otrData = Just ""}
testObject_OtrMessage_user_17 :: OtrMessage
testObject_OtrMessage_user_17 = OtrMessage {otrSender = ClientId {client = "1d"}, otrRecipient = ClientId {client = "14"}, otrCiphertext = "-\CAN\"Gts4", otrData = Just "\ACK\DC3"}
testObject_OtrMessage_user_18 :: OtrMessage
testObject_OtrMessage_user_18 = OtrMessage {otrSender = ClientId {client = "e"}, otrRecipient = ClientId {client = "2"}, otrCiphertext = "T\"", otrData = Just "v=*Z\NAK"}
testObject_OtrMessage_user_19 :: OtrMessage
testObject_OtrMessage_user_19 = OtrMessage {otrSender = ClientId {client = "11"}, otrRecipient = ClientId {client = "11"}, otrCiphertext = "\ETX\156999[m\SO$\997540", otrData = Nothing}
testObject_OtrMessage_user_20 :: OtrMessage
testObject_OtrMessage_user_20 = OtrMessage {otrSender = ClientId {client = "2"}, otrRecipient = ClientId {client = "14"}, otrCiphertext = "9", otrData = Nothing}
