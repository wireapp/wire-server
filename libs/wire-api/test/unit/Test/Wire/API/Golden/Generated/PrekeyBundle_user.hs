{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PrekeyBundle_user where

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
testObject_PrekeyBundle_user_1 :: PrekeyBundle
testObject_PrekeyBundle_user_1 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000007e-0000-005b-0000-001f00000066"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_2 :: PrekeyBundle
testObject_PrekeyBundle_user_2 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000000-0000-0072-0000-000600000039"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "f"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "cVe"}}]}
testObject_PrekeyBundle_user_3 :: PrekeyBundle
testObject_PrekeyBundle_user_3 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000054-0000-0047-0000-004400000001"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_4 :: PrekeyBundle
testObject_PrekeyBundle_user_4 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000001c-0000-0043-0000-001600000078"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "10"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "5"}}]}
testObject_PrekeyBundle_user_5 :: PrekeyBundle
testObject_PrekeyBundle_user_5 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000007c-0000-0006-0000-00430000006b"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
