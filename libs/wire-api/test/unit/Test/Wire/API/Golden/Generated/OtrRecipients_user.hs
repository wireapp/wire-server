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
testObject_OtrRecipients_user_1 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))),fromList [(ClientId {client = "0"},"|"),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))),fromList [(ClientId {client = "0"},"\FS"),(ClientId {client = "1"},"L")]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000400000003"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"\t")]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000300000003"))),fromList [(ClientId {client = "0"},"")]),((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000004"))),fromList []),((Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000400000004"))),fromList [(ClientId {client = "d"},"\SO")])]}}
testObject_OtrRecipients_user_2 :: OtrRecipients
testObject_OtrRecipients_user_2 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),fromList [(ClientId {client = "0"},"&\1062030")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))),fromList [(ClientId {client = "2"},"$")]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))),fromList [(ClientId {client = "2"},"\1023776^")])]}}
testObject_OtrRecipients_user_3 :: OtrRecipients
testObject_OtrRecipients_user_3 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000041-0000-0048-0000-001b00000004"))),fromList []),((Id (fromJust (UUID.fromString "00000066-0000-005c-0000-004d0000007e"))),fromList [(ClientId {client = "0"},"<"),(ClientId {client = "1"},""),(ClientId {client = "2"},"G")])]}}
testObject_OtrRecipients_user_4 :: OtrRecipients
testObject_OtrRecipients_user_4 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))),fromList [(ClientId {client = "0"},"+"),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))),fromList [(ClientId {client = "0"},"u\SYN")]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))),fromList [(ClientId {client = "3"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))),fromList [(ClientId {client = "0"},"J")]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")])]}}
testObject_OtrRecipients_user_5 :: OtrRecipients
testObject_OtrRecipients_user_5 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}
testObject_OtrRecipients_user_6 :: OtrRecipients
testObject_OtrRecipients_user_6 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))),fromList [(ClientId {client = "1"},"%[")]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"5")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))),fromList [(ClientId {client = "3"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))),fromList [(ClientId {client = "3"},"\USs!")]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))),fromList [])]}}
testObject_OtrRecipients_user_7 :: OtrRecipients
testObject_OtrRecipients_user_7 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00005a69-0000-0fb0-0000-6afa00006d06"))),fromList [(ClientId {client = "0"},"\998264_\DLE"),(ClientId {client = "1"},"0z;"),(ClientId {client = "2"},"z\172658"),(ClientId {client = "3"},":%")])]}}
testObject_OtrRecipients_user_8 :: OtrRecipients
testObject_OtrRecipients_user_8 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000079-0000-0025-0000-000a00000048"))),fromList [(ClientId {client = "aff"},"P\1091695+\SI@\132409H;")]),((Id (fromJust (UUID.fromString "0000007a-0000-0011-0000-003b00000026"))),fromList [(ClientId {client = "12"},"+?\151670\184720\1005231\1052131"),(ClientId {client = "13"},"^\rbN")])]}}
testObject_OtrRecipients_user_9 :: OtrRecipients
testObject_OtrRecipients_user_9 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000003-0000-000c-0000-000300000019"))),fromList [(ClientId {client = "4"},"NE"),(ClientId {client = "d"},"")]),((Id (fromJust (UUID.fromString "00000005-0000-0003-0000-001000000010"))),fromList []),((Id (fromJust (UUID.fromString "00000014-0000-000d-0000-001f00000010"))),fromList [(ClientId {client = "0"},"\DC2\r"),(ClientId {client = "2"},"")])]}}
testObject_OtrRecipients_user_10 :: OtrRecipients
testObject_OtrRecipients_user_10 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")])]}}
testObject_OtrRecipients_user_11 :: OtrRecipients
testObject_OtrRecipients_user_11 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}
testObject_OtrRecipients_user_12 :: OtrRecipients
testObject_OtrRecipients_user_12 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000025-0000-0022-0000-001d0000007b"))),fromList []),((Id (fromJust (UUID.fromString "00000073-0000-002d-0000-002400000062"))),fromList [(ClientId {client = "0"},"\31002"),(ClientId {client = "1"},"")])]}}
testObject_OtrRecipients_user_13 :: OtrRecipients
testObject_OtrRecipients_user_13 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000400000003"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"g")]),((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000004"))),fromList [(ClientId {client = "7"},"")]),((Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000400000004"))),fromList [(ClientId {client = "6"},"}S\DC3")]),((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000004"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000100000001"))),fromList [(ClientId {client = "0"},"k,")]),((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000004"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")])]}}
testObject_OtrRecipients_user_14 :: OtrRecipients
testObject_OtrRecipients_user_14 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),fromList [(ClientId {client = "1"},"(")]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))),fromList [(ClientId {client = "0"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))),fromList [(ClientId {client = "1"},"\fy")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"m")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))),fromList [(ClientId {client = "2"},"F\ETX")])]}}
testObject_OtrRecipients_user_15 :: OtrRecipients
testObject_OtrRecipients_user_15 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}
testObject_OtrRecipients_user_16 :: OtrRecipients
testObject_OtrRecipients_user_16 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000027-0000-0029-0000-006500000064"))),fromList [(ClientId {client = "0"},"2"),(ClientId {client = "2"},""),(ClientId {client = "6"},"E\1105129p\74318 ")]),((Id (fromJust (UUID.fromString "0000002b-0000-0072-0000-00560000001e"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"11"),(ClientId {client = "2"},"~{")])]}}
testObject_OtrRecipients_user_17 :: OtrRecipients
testObject_OtrRecipients_user_17 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000002-0000-0007-0000-000100000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000500000006"))),fromList [(ClientId {client = "0"},"Q"),(ClientId {client = "1"},"\DC2")]),((Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000700000002"))),fromList [(ClientId {client = "1"},"`"),(ClientId {client = "2"},"\1052961\1100124")]),((Id (fromJust (UUID.fromString "00000007-0000-0001-0000-000100000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"B")])]}}
testObject_OtrRecipients_user_18 :: OtrRecipients
testObject_OtrRecipients_user_18 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "0000002e-0000-0030-0000-000700000036"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"\179735")]),((Id (fromJust (UUID.fromString "00000060-0000-0032-0000-004400000069"))),fromList [(ClientId {client = "0"},"`\1016468"),(ClientId {client = "1"},"\1007865H"),(ClientId {client = "2"},"")])]}}
testObject_OtrRecipients_user_19 :: OtrRecipients
testObject_OtrRecipients_user_19 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}
testObject_OtrRecipients_user_20 :: OtrRecipients
testObject_OtrRecipients_user_20 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "0000000b-0000-001b-0000-000800000010"))),fromList [(ClientId {client = "0"},"\1045217\DC4"),(ClientId {client = "1"},"\CAN")]),((Id (fromJust (UUID.fromString "0000001c-0000-0000-0000-00010000000f"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000020-0000-001e-0000-000700000020"))),fromList [])]}}
