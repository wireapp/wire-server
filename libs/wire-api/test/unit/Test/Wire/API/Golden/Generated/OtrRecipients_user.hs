{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.OtrRecipients_user where

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
testObject_OtrRecipients_user_1 :: OtrRecipients
testObject_OtrRecipients_user_1 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000001-0000-007b-0000-006500000031"))),fromList [(ClientId {client = "1a"},"\STX|\NAK)\NUL+\v"),(ClientId {client = "20"},"\fuHh\ETB^")]),((Id (fromJust (UUID.fromString "0000005b-0000-0028-0000-006400000023"))),fromList [])]}}
testObject_OtrRecipients_user_2 :: OtrRecipients
testObject_OtrRecipients_user_2 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00004ddc-0000-59a8-0000-417e000039e1"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"\ACK")])]}}
testObject_OtrRecipients_user_3 :: OtrRecipients
testObject_OtrRecipients_user_3 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),fromList [(ClientId {client = "0"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),fromList [(ClientId {client = "1"},"&")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),fromList [(ClientId {client = "0"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),fromList [(ClientId {client = "0"},"g")]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),fromList [(ClientId {client = "0"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),fromList [(ClientId {client = "0"},"")])]}}
testObject_OtrRecipients_user_4 :: OtrRecipients
testObject_OtrRecipients_user_4 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}
testObject_OtrRecipients_user_5 :: OtrRecipients
testObject_OtrRecipients_user_5 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),fromList [(ClientId {client = "0"},"]"),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))),fromList [(ClientId {client = "0"},"\190465\&1")]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))),fromList [(ClientId {client = "0"},"\b"),(ClientId {client = "1"},"r")]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))),fromList [])]}}
testObject_OtrRecipients_user_6 :: OtrRecipients
testObject_OtrRecipients_user_6 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),fromList [])]}}
testObject_OtrRecipients_user_7 :: OtrRecipients
testObject_OtrRecipients_user_7 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}
testObject_OtrRecipients_user_8 :: OtrRecipients
testObject_OtrRecipients_user_8 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000300000003"))),fromList [(ClientId {client = "5"},"%j")]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000300000001"))),fromList [(ClientId {client = "2"},"(\1042940l")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000004"))),fromList [(ClientId {client = "6"},"^8\42854")]),((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000200000001"))),fromList []),((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000000000004"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000400000001"))),fromList [(ClientId {client = "7"},"\v")]),((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000002"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")])]}}
testObject_OtrRecipients_user_9 :: OtrRecipients
testObject_OtrRecipients_user_9 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00005624-0000-35ee-0000-4be800005cbb"))),fromList [(ClientId {client = "0"}," "),(ClientId {client = "1"},""),(ClientId {client = "2"},"")])]}}
testObject_OtrRecipients_user_10 :: OtrRecipients
testObject_OtrRecipients_user_10 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000005-0000-0006-0000-008000000069"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "2"},"\141764\43950-]t"),(ClientId {client = "3"},"")]),((Id (fromJust (UUID.fromString "00000069-0000-0065-0000-004e00000023"))),fromList [])]}}
testObject_OtrRecipients_user_11 :: OtrRecipients
testObject_OtrRecipients_user_11 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000000000008"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"\98418")]),((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000700000003"))),fromList [(ClientId {client = "0"},"\t"),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000300000006"))),fromList [])]}}
testObject_OtrRecipients_user_12 :: OtrRecipients
testObject_OtrRecipients_user_12 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),fromList [(ClientId {client = "0"},"c")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),fromList [(ClientId {client = "1"},"")])]}}
testObject_OtrRecipients_user_13 :: OtrRecipients
testObject_OtrRecipients_user_13 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000050-0000-0047-0000-002b0000006f"))),fromList [(ClientId {client = "67d"},"\1041343%c\NUL\"")]),((Id (fromJust (UUID.fromString "00000077-0000-0066-0000-003000000067"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")])]}}
testObject_OtrRecipients_user_14 :: OtrRecipients
testObject_OtrRecipients_user_14 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000005"))),fromList [(ClientId {client = "0"},"\EM"),(ClientId {client = "1"},"B")]),((Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000100000007"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000100000006"))),fromList [(ClientId {client = "0"},"="),(ClientId {client = "1"},"Y")]),((Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000800000005"))),fromList [])]}}
testObject_OtrRecipients_user_15 :: OtrRecipients
testObject_OtrRecipients_user_15 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"\RS")]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))),fromList [(ClientId {client = "0"},"wW")])]}}
testObject_OtrRecipients_user_16 :: OtrRecipients
testObject_OtrRecipients_user_16 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00006d7d-0000-321a-0000-0ef200004245"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")])]}}
testObject_OtrRecipients_user_17 :: OtrRecipients
testObject_OtrRecipients_user_17 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000003-0000-007a-0000-003800000080"))),fromList [(ClientId {client = "f36"},"\EOT")]),((Id (fromJust (UUID.fromString "00000007-0000-0070-0000-001f00000062"))),fromList [(ClientId {client = "0"},"\161722"),(ClientId {client = "2"},";M"),(ClientId {client = "4"},"t\ACKB")])]}}
testObject_OtrRecipients_user_18 :: OtrRecipients
testObject_OtrRecipients_user_18 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00003bb5-0000-057e-0000-3feb00006e6a"))),fromList [(ClientId {client = "a6509e"},"\US1\995784B2)KJh5\1090235\NUL\EOT\1062267J0e\8080")])]}}
testObject_OtrRecipients_user_19 :: OtrRecipients
testObject_OtrRecipients_user_19 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),fromList [])]}}
testObject_OtrRecipients_user_20 :: OtrRecipients
testObject_OtrRecipients_user_20 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000003"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000003"))),fromList [(ClientId {client = "8"},"\12402u\GS")]),((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000300000003"))),fromList []),((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000004"))),fromList [(ClientId {client = "1"},"&")]),((Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")])]}}
