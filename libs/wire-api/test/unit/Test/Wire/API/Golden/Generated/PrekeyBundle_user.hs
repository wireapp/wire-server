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
testObject_PrekeyBundle_user_1 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000031-0000-006b-0000-004c0000001b"))), prekeyClients = []}
testObject_PrekeyBundle_user_2 :: PrekeyBundle
testObject_PrekeyBundle_user_2 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000010-0000-001d-0000-005c0000003e"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_3 :: PrekeyBundle
testObject_PrekeyBundle_user_3 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000030-0000-0051-0000-002d00000032"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "L"}}]}
testObject_PrekeyBundle_user_4 :: PrekeyBundle
testObject_PrekeyBundle_user_4 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000002d-0000-0059-0000-007f00000038"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "5"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "2;"}}]}
testObject_PrekeyBundle_user_5 :: PrekeyBundle
testObject_PrekeyBundle_user_5 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000079-0000-0015-0000-003000000043"))), prekeyClients = []}
testObject_PrekeyBundle_user_6 :: PrekeyBundle
testObject_PrekeyBundle_user_6 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000000b-0000-0052-0000-006a00000006"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "4"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_7 :: PrekeyBundle
testObject_PrekeyBundle_user_7 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000031-0000-0042-0000-00230000007e"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "3"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\1110694"}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_8 :: PrekeyBundle
testObject_PrekeyBundle_user_8 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000003d-0000-007c-0000-005a00000076"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "2"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "2"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "'"}}]}
testObject_PrekeyBundle_user_9 :: PrekeyBundle
testObject_PrekeyBundle_user_9 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000004a-0000-0056-0000-007300000026"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_10 :: PrekeyBundle
testObject_PrekeyBundle_user_10 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000002f-0000-0063-0000-00220000006e"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "4"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "2"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\988095"}}]}
testObject_PrekeyBundle_user_11 :: PrekeyBundle
testObject_PrekeyBundle_user_11 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000072-0000-0061-0000-00760000002a"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_12 :: PrekeyBundle
testObject_PrekeyBundle_user_12 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000002a-0000-0014-0000-003600000079"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_13 :: PrekeyBundle
testObject_PrekeyBundle_user_13 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000035-0000-001e-0000-005b0000005b"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "6"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "I"}}]}
testObject_PrekeyBundle_user_14 :: PrekeyBundle
testObject_PrekeyBundle_user_14 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000005b-0000-000a-0000-00560000006f"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_15 :: PrekeyBundle
testObject_PrekeyBundle_user_15 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000050-0000-0021-0000-003500000069"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_16 :: PrekeyBundle
testObject_PrekeyBundle_user_16 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000032-0000-000b-0000-003b0000007f"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_17 :: PrekeyBundle
testObject_PrekeyBundle_user_17 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000004c-0000-0061-0000-006500000032"))), prekeyClients = []}
testObject_PrekeyBundle_user_18 :: PrekeyBundle
testObject_PrekeyBundle_user_18 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000062-0000-0003-0000-003c0000001d"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_19 :: PrekeyBundle
testObject_PrekeyBundle_user_19 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000017-0000-0049-0000-008000000009"))), prekeyClients = []}
testObject_PrekeyBundle_user_20 :: PrekeyBundle
testObject_PrekeyBundle_user_20 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000067-0000-0060-0000-001d0000002d"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
