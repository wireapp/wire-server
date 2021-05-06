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
testObject_OtrMessage_user_1 = OtrMessage {otrSender = ClientId {client = "1c"}, otrRecipient = ClientId {client = "1b"}, otrCiphertext = "\1035838\7467B\ENQ\160991", otrData = Just "|-/\DC1"}
testObject_OtrMessage_user_2 :: OtrMessage
testObject_OtrMessage_user_2 = OtrMessage {otrSender = ClientId {client = "d"}, otrRecipient = ClientId {client = "11"}, otrCiphertext = "\ETB4Un0", otrData = Just "\bT\SI\NUL!\SIN"}
testObject_OtrMessage_user_3 :: OtrMessage
testObject_OtrMessage_user_3 = OtrMessage {otrSender = ClientId {client = "1f"}, otrRecipient = ClientId {client = "c"}, otrCiphertext = "\a\10994>'Ouq", otrData = Just ""}
testObject_OtrMessage_user_4 :: OtrMessage
testObject_OtrMessage_user_4 = OtrMessage {otrSender = ClientId {client = "1b"}, otrRecipient = ClientId {client = "2"}, otrCiphertext = "*\1099430!", otrData = Just "\69719V\156478lU"}
testObject_OtrMessage_user_5 :: OtrMessage
testObject_OtrMessage_user_5 = OtrMessage {otrSender = ClientId {client = "19"}, otrRecipient = ClientId {client = "5"}, otrCiphertext = "*\138702_$\1099572", otrData = Just "\RS"}
testObject_OtrMessage_user_6 :: OtrMessage
testObject_OtrMessage_user_6 = OtrMessage {otrSender = ClientId {client = "1e"}, otrRecipient = ClientId {client = "1"}, otrCiphertext = "", otrData = Nothing}
testObject_OtrMessage_user_7 :: OtrMessage
testObject_OtrMessage_user_7 = OtrMessage {otrSender = ClientId {client = "1"}, otrRecipient = ClientId {client = "b"}, otrCiphertext = "\v\DC4L\1036909\DC3", otrData = Just "\DC3\1086637\49677\1083304\24820\43518/"}
testObject_OtrMessage_user_8 :: OtrMessage
testObject_OtrMessage_user_8 = OtrMessage {otrSender = ClientId {client = "20"}, otrRecipient = ClientId {client = "9"}, otrCiphertext = "\83510m", otrData = Just "."}
testObject_OtrMessage_user_9 :: OtrMessage
testObject_OtrMessage_user_9 = OtrMessage {otrSender = ClientId {client = "20"}, otrRecipient = ClientId {client = "8"}, otrCiphertext = "f3X\b\SOp\SUB", otrData = Just "})\31247E,W"}
testObject_OtrMessage_user_10 :: OtrMessage
testObject_OtrMessage_user_10 = OtrMessage {otrSender = ClientId {client = "9"}, otrRecipient = ClientId {client = "1e"}, otrCiphertext = "\SUB\FSV", otrData = Just "\1076303fq\1037612\US"}
testObject_OtrMessage_user_11 :: OtrMessage
testObject_OtrMessage_user_11 = OtrMessage {otrSender = ClientId {client = "20"}, otrRecipient = ClientId {client = "1e"}, otrCiphertext = "o\EMl-]\\", otrData = Nothing}
testObject_OtrMessage_user_12 :: OtrMessage
testObject_OtrMessage_user_12 = OtrMessage {otrSender = ClientId {client = "1b"}, otrRecipient = ClientId {client = "d"}, otrCiphertext = "'u", otrData = Nothing}
testObject_OtrMessage_user_13 :: OtrMessage
testObject_OtrMessage_user_13 = OtrMessage {otrSender = ClientId {client = "16"}, otrRecipient = ClientId {client = "c"}, otrCiphertext = "\58401m*", otrData = Just "\v\STXf\ETXy#S"}
testObject_OtrMessage_user_14 :: OtrMessage
testObject_OtrMessage_user_14 = OtrMessage {otrSender = ClientId {client = "e"}, otrRecipient = ClientId {client = "e"}, otrCiphertext = "|&\1090278", otrData = Just "I2\1019501|"}
testObject_OtrMessage_user_15 :: OtrMessage
testObject_OtrMessage_user_15 = OtrMessage {otrSender = ClientId {client = "17"}, otrRecipient = ClientId {client = "16"}, otrCiphertext = "h", otrData = Nothing}
testObject_OtrMessage_user_16 :: OtrMessage
testObject_OtrMessage_user_16 = OtrMessage {otrSender = ClientId {client = "18"}, otrRecipient = ClientId {client = "e"}, otrCiphertext = "\3191A\ETXx\SYN\185578", otrData = Just "$k\41214!"}
testObject_OtrMessage_user_17 :: OtrMessage
testObject_OtrMessage_user_17 = OtrMessage {otrSender = ClientId {client = "12"}, otrRecipient = ClientId {client = "3"}, otrCiphertext = "\DC1n\SYN\\\165666_", otrData = Just "Cm>F\23452\&1"}
testObject_OtrMessage_user_18 :: OtrMessage
testObject_OtrMessage_user_18 = OtrMessage {otrSender = ClientId {client = "8"}, otrRecipient = ClientId {client = "14"}, otrCiphertext = "", otrData = Just "\163273\f"}
testObject_OtrMessage_user_19 :: OtrMessage
testObject_OtrMessage_user_19 = OtrMessage {otrSender = ClientId {client = "1b"}, otrRecipient = ClientId {client = "14"}, otrCiphertext = "", otrData = Nothing}
testObject_OtrMessage_user_20 :: OtrMessage
testObject_OtrMessage_user_20 = OtrMessage {otrSender = ClientId {client = "b"}, otrRecipient = ClientId {client = "18"}, otrCiphertext = "'*_\ETB5", otrData = Just "z\SOv"}
