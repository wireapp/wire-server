{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Event_user where

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
testObject_Event_1 :: Event
testObject_Event_1 = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "00004c40-0000-4621-0000-07c3000058dd")))) ((Id (fromJust (UUID.fromString "0000771f-0000-4564-0000-59a200002e84")))) (read "1864-05-02 04:43:02.947295094043 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000008"))), cMessage = Just "\r|\1070477m0", cName = Just "", cEmail = Just ""}))))
testObject_Event_2 :: Event
testObject_Event_2 = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "0000490d-0000-3ca7-0000-12e3000047af")))) ((Id (fromJust (UUID.fromString "00000c1b-0000-3b97-0000-271700005474")))) (read "1864-04-15 14:07:51.596848591485 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "1c"}, otrRecipient = ClientId {client = "1a"}, otrCiphertext = "+\128952\142115DY8(", otrData = Just "G\1064953h\131514?"}))))
testObject_Event_3 :: Event
testObject_Event_3 = (Event (ConvRename) ((Id (fromJust (UUID.fromString "00006b4b-0000-3670-0000-230b00000265")))) ((Id (fromJust (UUID.fromString "00000730-0000-30b9-0000-686b00003d37")))) (read "1864-06-02 05:55:33.538681473823 UTC") (Just (EdConvRename (ConversationRename {cupName = "\SYN1\172633\SUB\164620FvyE\SYN\1032608\SYNh\93035,\1029625\a|r\b\r@H\SUB\190781K9\74425"}))))
testObject_Event_4 :: Event
testObject_Event_4 = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "00007c11-0000-6706-0000-0cd200000ddc")))) ((Id (fromJust (UUID.fromString "000049aa-0000-626c-0000-637f00007566")))) (read "1864-04-09 21:34:32.03696148812 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 8173802})}))))
testObject_Event_5 :: Event
testObject_Event_5 = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "00005d06-0000-6746-0000-670f0000247f")))) ((Id (fromJust (UUID.fromString "000025ad-0000-0028-0000-31da00002788")))) (read "1864-04-29 20:50:26.711335230342 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "1"}, otrRecipient = ClientId {client = "f"}, otrCiphertext = "{z", otrData = Just "hUTt"}))))
