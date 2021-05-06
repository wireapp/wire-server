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
testObject_OtrMessage_user_1 = OtrMessage {otrSender = ClientId {client = "d"}, otrRecipient = ClientId {client = "1b"}, otrCiphertext = "\NAK", otrData = Just "\ETBf\49659\nOI"}
testObject_OtrMessage_user_2 :: OtrMessage
testObject_OtrMessage_user_2 = OtrMessage {otrSender = ClientId {client = "20"}, otrRecipient = ClientId {client = "1d"}, otrCiphertext = "}\DC2v9", otrData = Just ""}
testObject_OtrMessage_user_3 :: OtrMessage
testObject_OtrMessage_user_3 = OtrMessage {otrSender = ClientId {client = "10"}, otrRecipient = ClientId {client = "f"}, otrCiphertext = "X\171767o\DC3", otrData = Nothing}
testObject_OtrMessage_user_4 :: OtrMessage
testObject_OtrMessage_user_4 = OtrMessage {otrSender = ClientId {client = "7"}, otrRecipient = ClientId {client = "18"}, otrCiphertext = "\STX\rr\1094295\FS", otrData = Just "\1103148\&7\tc\1016836E\148570"}
testObject_OtrMessage_user_5 :: OtrMessage
testObject_OtrMessage_user_5 = OtrMessage {otrSender = ClientId {client = "1c"}, otrRecipient = ClientId {client = "b"}, otrCiphertext = "", otrData = Nothing}
