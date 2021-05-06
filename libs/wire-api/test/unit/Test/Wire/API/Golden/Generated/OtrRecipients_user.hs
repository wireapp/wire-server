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
testObject_OtrRecipients_user_1 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}
testObject_OtrRecipients_user_2 :: OtrRecipients
testObject_OtrRecipients_user_2 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "0000003a-0000-005b-0000-004f0000003b"))),fromList [(ClientId {client = "1"},"["),(ClientId {client = "6"},"RN4@"),(ClientId {client = "9"},"6u")]),((Id (fromJust (UUID.fromString "0000006b-0000-003b-0000-00060000003a"))),fromList [(ClientId {client = "0"},"I"),(ClientId {client = "1"},"\101062"),(ClientId {client = "2"},"\a\994282")])]}}
testObject_OtrRecipients_user_3 :: OtrRecipients
testObject_OtrRecipients_user_3 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000400000004"))),fromList []),((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000100000002"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000800000008"))),fromList []),((Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000800000004"))),fromList [])]}}
testObject_OtrRecipients_user_4 :: OtrRecipients
testObject_OtrRecipients_user_4 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),fromList [(ClientId {client = "0"},"\988843")]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),fromList [(ClientId {client = "0"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")])]}}
testObject_OtrRecipients_user_5 :: OtrRecipients
testObject_OtrRecipients_user_5 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))),fromList [(ClientId {client = "0"},"c"),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))),fromList [(ClientId {client = "1"},"\138434")]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")])]}}
testObject_OtrRecipients_user_6 :: OtrRecipients
testObject_OtrRecipients_user_6 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000003-0000-0015-0000-000100000014"))),fromList []),((Id (fromJust (UUID.fromString "00000014-0000-001a-0000-00030000000a"))),fromList [(ClientId {client = "0"},"\1069830]`"),(ClientId {client = "5"},"")]),((Id (fromJust (UUID.fromString "00000015-0000-000c-0000-000a00000017"))),fromList [(ClientId {client = "0"},"."),(ClientId {client = "1"},"T")])]}}
testObject_OtrRecipients_user_7 :: OtrRecipients
testObject_OtrRecipients_user_7 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}
testObject_OtrRecipients_user_8 :: OtrRecipients
testObject_OtrRecipients_user_8 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),fromList [(ClientId {client = "0"},"S"),(ClientId {client = "1"},"0")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))),fromList [(ClientId {client = "3"},"\1027488")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))),fromList [(ClientId {client = "0"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))),fromList [(ClientId {client = "2"},"<Hs")]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))),fromList [])]}}
testObject_OtrRecipients_user_9 :: OtrRecipients
testObject_OtrRecipients_user_9 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))),fromList [(ClientId {client = "2"},"\1029515")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),fromList [(ClientId {client = "0"},"\GS\1005912")]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))),fromList [(ClientId {client = "0"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),fromList [(ClientId {client = "0"},"\128069"),(ClientId {client = "1"},"c")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),fromList [(ClientId {client = "0"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"{")])]}}
testObject_OtrRecipients_user_10 :: OtrRecipients
testObject_OtrRecipients_user_10 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))),fromList [(ClientId {client = "1"},"t")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))),fromList [(ClientId {client = "0"},"7")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))),fromList [(ClientId {client = "0"},"\t\132591")]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))),fromList [(ClientId {client = "0"},"Y"),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))),fromList [(ClientId {client = "0"},"")]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))),fromList [(ClientId {client = "1"},"@G")]),((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))),fromList [])]}}
testObject_OtrRecipients_user_11 :: OtrRecipients
testObject_OtrRecipients_user_11 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000019-0000-0070-0000-006b0000005a"))),fromList [(ClientId {client = "10"},"\STX;\174515"),(ClientId {client = "e"},"/v\73938\171172%"),(ClientId {client = "f"},"\997107rC")]),((Id (fromJust (UUID.fromString "0000004a-0000-001e-0000-00170000000e"))),fromList [(ClientId {client = "0"},"9"),(ClientId {client = "1"},"\138104"),(ClientId {client = "2"},"l")])]}}
testObject_OtrRecipients_user_12 :: OtrRecipients
testObject_OtrRecipients_user_12 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "0000004e-0000-005d-0000-00650000005c"))),fromList [(ClientId {client = "0"},"\NAK"),(ClientId {client = "1"},"\95479")]),((Id (fromJust (UUID.fromString "00000069-0000-0042-0000-006a00000061"))),fromList [(ClientId {client = "a38"},";\SOx\SUB\1023037\128978Z\31678\NUL")])]}}
testObject_OtrRecipients_user_13 :: OtrRecipients
testObject_OtrRecipients_user_13 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),fromList [(ClientId {client = "0"},"\986785"),(ClientId {client = "1"},"\n")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),fromList [(ClientId {client = "1"},"S")]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))),fromList [(ClientId {client = "0"},"/")]),((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")])]}}
testObject_OtrRecipients_user_14 :: OtrRecipients
testObject_OtrRecipients_user_14 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),fromList [(ClientId {client = "1"},"d")]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),fromList [(ClientId {client = "0"},"z")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),fromList [(ClientId {client = "0"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),fromList [(ClientId {client = "0"},"")])]}}
testObject_OtrRecipients_user_15 :: OtrRecipients
testObject_OtrRecipients_user_15 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),fromList []),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),fromList [])]}}
testObject_OtrRecipients_user_16 :: OtrRecipients
testObject_OtrRecipients_user_16 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000100000004"))),fromList [(ClientId {client = "0"},"V")]),((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000400000002"))),fromList [(ClientId {client = "7"},"_\DC3\v")]),((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000200000000"))),fromList [(ClientId {client = "1"},"\39105")]),((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000400000002"))),fromList [(ClientId {client = "1"},"Q\SI")]),((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000002"))),fromList [(ClientId {client = "0"},";"),(ClientId {client = "2"},"\989238\&7")]),((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000300000003"))),fromList [(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000400000003"))),fromList [])]}}
testObject_OtrRecipients_user_17 :: OtrRecipients
testObject_OtrRecipients_user_17 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),fromList [(ClientId {client = "0"},"J&"),(ClientId {client = "4"},"")]),((Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000000000008"))),fromList []),((Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000100000008"))),fromList []),((Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000400000000"))),fromList [(ClientId {client = "0"},""),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000300000001"))),fromList [(ClientId {client = "3"},"M"),(ClientId {client = "4"},"n+\ETX")])]}}
testObject_OtrRecipients_user_18 :: OtrRecipients
testObject_OtrRecipients_user_18 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}
testObject_OtrRecipients_user_19 :: OtrRecipients
testObject_OtrRecipients_user_19 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList []}}
testObject_OtrRecipients_user_20 :: OtrRecipients
testObject_OtrRecipients_user_20 = OtrRecipients {otrRecipientsMap = UserClientMap {userClientMap = fromList [((Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000500000004"))),fromList [(ClientId {client = "0"},"\DC2Z\98811"),(ClientId {client = "4"},"\132041\1089188\1054651")]),((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000300000007"))),fromList []),((Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000200000004"))),fromList []),((Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000200000002"))),fromList [(ClientId {client = "0"},"L"),(ClientId {client = "1"},"")]),((Id (fromJust (UUID.fromString "00000007-0000-0001-0000-000200000006"))),fromList [])]}}
