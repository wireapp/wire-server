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
testObject_PrekeyBundle_user_1 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000004e-0000-0013-0000-00720000004b"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "3"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1034946"}},ClientPrekey {prekeyClient = ClientId {client = "2"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\a"}}]}
testObject_PrekeyBundle_user_2 :: PrekeyBundle
testObject_PrekeyBundle_user_2 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000004-0000-0024-0000-007a00000017"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "l"}},ClientPrekey {prekeyClient = ClientId {client = "2"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_3 :: PrekeyBundle
testObject_PrekeyBundle_user_3 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000000c-0000-0065-0000-002100000040"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "3"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "c"}}]}
testObject_PrekeyBundle_user_4 :: PrekeyBundle
testObject_PrekeyBundle_user_4 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000071-0000-0078-0000-002c0000001d"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_5 :: PrekeyBundle
testObject_PrekeyBundle_user_5 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000058-0000-002c-0000-002b00000012"))), prekeyClients = []}
testObject_PrekeyBundle_user_6 :: PrekeyBundle
testObject_PrekeyBundle_user_6 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000000d-0000-007b-0000-000b00000059"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1d"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_7 :: PrekeyBundle
testObject_PrekeyBundle_user_7 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000070-0000-007f-0000-005600000005"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "a"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "V"}}]}
testObject_PrekeyBundle_user_8 :: PrekeyBundle
testObject_PrekeyBundle_user_8 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000079-0000-000e-0000-003b00000031"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "13"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_9 :: PrekeyBundle
testObject_PrekeyBundle_user_9 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000050-0000-0070-0000-00100000005e"))), prekeyClients = []}
testObject_PrekeyBundle_user_10 :: PrekeyBundle
testObject_PrekeyBundle_user_10 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000002c-0000-003c-0000-001a00000070"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ":"}},ClientPrekey {prekeyClient = ClientId {client = "4"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1047195"}}]}
testObject_PrekeyBundle_user_11 :: PrekeyBundle
testObject_PrekeyBundle_user_11 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000022-0000-006a-0000-004700000031"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "2"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "b\47409"}}]}
testObject_PrekeyBundle_user_12 :: PrekeyBundle
testObject_PrekeyBundle_user_12 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000028-0000-0078-0000-006a00000034"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_13 :: PrekeyBundle
testObject_PrekeyBundle_user_13 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000001a-0000-003a-0000-005b00000020"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_14 :: PrekeyBundle
testObject_PrekeyBundle_user_14 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000013-0000-0036-0000-006d00000032"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "3"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\1075447"}},ClientPrekey {prekeyClient = ClientId {client = "4"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "2"}}]}
testObject_PrekeyBundle_user_15 :: PrekeyBundle
testObject_PrekeyBundle_user_15 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000000-0000-0066-0000-00160000000e"))), prekeyClients = []}
testObject_PrekeyBundle_user_16 :: PrekeyBundle
testObject_PrekeyBundle_user_16 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000061-0000-004b-0000-000b00000065"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_17 :: PrekeyBundle
testObject_PrekeyBundle_user_17 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000051-0000-004f-0000-006000000057"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "*"}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "2"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_18 :: PrekeyBundle
testObject_PrekeyBundle_user_18 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000005b-0000-007e-0000-006c00000027"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_19 :: PrekeyBundle
testObject_PrekeyBundle_user_19 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000000c-0000-001b-0000-006a00000024"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_20 :: PrekeyBundle
testObject_PrekeyBundle_user_20 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000024-0000-0068-0000-00430000002c"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
