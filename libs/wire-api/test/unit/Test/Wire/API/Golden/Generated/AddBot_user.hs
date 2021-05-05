{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.AddBot_user where

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
testObject_AddBot_1 :: AddBot
testObject_AddBot_1 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001d-0000-0000-0000-001200000011"))), addBotService = (Id (fromJust (UUID.fromString "00000011-0000-0018-0000-000100000010"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.UG, lCountry = Nothing})}
testObject_AddBot_2 :: AddBot
testObject_AddBot_2 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000010-0000-0001-0000-000d0000001e"))), addBotService = (Id (fromJust (UUID.fromString "00000007-0000-001e-0000-001900000013"))), addBotLocale = Nothing}
testObject_AddBot_3 :: AddBot
testObject_AddBot_3 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000e-0000-000e-0000-001d0000001a"))), addBotService = (Id (fromJust (UUID.fromString "0000001b-0000-0006-0000-000f0000000c"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SQ, lCountry = Nothing})}
testObject_AddBot_4 :: AddBot
testObject_AddBot_4 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000008-0000-0016-0000-001500000004"))), addBotService = (Id (fromJust (UUID.fromString "0000000d-0000-0007-0000-001b00000019"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.OS, lCountry = Nothing})}
testObject_AddBot_5 :: AddBot
testObject_AddBot_5 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000018-0000-000c-0000-001b00000009"))), addBotService = (Id (fromJust (UUID.fromString "0000000d-0000-000f-0000-000400000002"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.EE, lCountry = Just (Country {fromCountry = CZ})})}
