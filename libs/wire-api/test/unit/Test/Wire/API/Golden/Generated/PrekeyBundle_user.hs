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
testObject_PrekeyBundle_user_1 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000068-0000-0059-0000-00190000004b"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "L"}}]}
testObject_PrekeyBundle_user_2 :: PrekeyBundle
testObject_PrekeyBundle_user_2 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000003e-0000-0024-0000-006f00000053"))), prekeyClients = []}
testObject_PrekeyBundle_user_3 :: PrekeyBundle
testObject_PrekeyBundle_user_3 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000006c-0000-006c-0000-00020000007e"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_4 :: PrekeyBundle
testObject_PrekeyBundle_user_4 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000002f-0000-0045-0000-001600000064"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_5 :: PrekeyBundle
testObject_PrekeyBundle_user_5 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000001a-0000-0017-0000-000600000018"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "2"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "V"}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "|"}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "("}}]}
testObject_PrekeyBundle_user_6 :: PrekeyBundle
testObject_PrekeyBundle_user_6 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000001-0000-0074-0000-003a0000007d"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_7 :: PrekeyBundle
testObject_PrekeyBundle_user_7 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000015-0000-003e-0000-001500000007"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_8 :: PrekeyBundle
testObject_PrekeyBundle_user_8 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000017-0000-0011-0000-007e00000013"))), prekeyClients = []}
testObject_PrekeyBundle_user_9 :: PrekeyBundle
testObject_PrekeyBundle_user_9 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000048-0000-001e-0000-002b0000004b"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_10 :: PrekeyBundle
testObject_PrekeyBundle_user_10 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000022-0000-007e-0000-006c00000054"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_11 :: PrekeyBundle
testObject_PrekeyBundle_user_11 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000003-0000-0050-0000-000600000040"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "2"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\RS"}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = " "}}]}
testObject_PrekeyBundle_user_12 :: PrekeyBundle
testObject_PrekeyBundle_user_12 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000036-0000-0009-0000-007000000062"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\1027174"}}]}
testObject_PrekeyBundle_user_13 :: PrekeyBundle
testObject_PrekeyBundle_user_13 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000040-0000-0003-0000-003000000016"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "10"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\STX\1073127@"}}]}
testObject_PrekeyBundle_user_14 :: PrekeyBundle
testObject_PrekeyBundle_user_14 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000041-0000-0069-0000-00570000004c"))), prekeyClients = []}
testObject_PrekeyBundle_user_15 :: PrekeyBundle
testObject_PrekeyBundle_user_15 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000007c-0000-0064-0000-005800000061"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_16 :: PrekeyBundle
testObject_PrekeyBundle_user_16 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000000-0000-005a-0000-000900000041"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}},ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_17 :: PrekeyBundle
testObject_PrekeyBundle_user_17 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000064-0000-0037-0000-000200000050"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "7"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1072713EZ"}}]}
testObject_PrekeyBundle_user_18 :: PrekeyBundle
testObject_PrekeyBundle_user_18 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000006a-0000-007b-0000-000700000026"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1a"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}
testObject_PrekeyBundle_user_19 :: PrekeyBundle
testObject_PrekeyBundle_user_19 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000003b-0000-003b-0000-004c00000062"))), prekeyClients = []}
testObject_PrekeyBundle_user_20 :: PrekeyBundle
testObject_PrekeyBundle_user_20 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000050-0000-0030-0000-006800000031"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "2"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\129125\139432\1052323"}}]}
