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
testObject_AddBot_user_1 :: AddBot
testObject_AddBot_user_1 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000d-0000-0013-0000-000b00000009"))), addBotService = (Id (fromJust (UUID.fromString "00000019-0000-0017-0000-001700000009"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.XH, lCountry = Just (Country {fromCountry = RS})})}
testObject_AddBot_user_2 :: AddBot
testObject_AddBot_user_2 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000013-0000-0012-0000-00140000000c"))), addBotService = (Id (fromJust (UUID.fromString "00000001-0000-0017-0000-000d00000000"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SU, lCountry = Just (Country {fromCountry = PY})})}
testObject_AddBot_user_3 :: AddBot
testObject_AddBot_user_3 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001c-0000-0004-0000-000500000006"))), addBotService = (Id (fromJust (UUID.fromString "00000015-0000-001a-0000-00150000000f"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TL, lCountry = Just (Country {fromCountry = UY})})}
testObject_AddBot_user_4 :: AddBot
testObject_AddBot_user_4 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000f-0000-0007-0000-000000000003"))), addBotService = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-002000000015"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.YI, lCountry = Nothing})}
testObject_AddBot_user_5 :: AddBot
testObject_AddBot_user_5 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000002-0000-0018-0000-000000000002"))), addBotService = (Id (fromJust (UUID.fromString "00000001-0000-000f-0000-001300000004"))), addBotLocale = Nothing}
