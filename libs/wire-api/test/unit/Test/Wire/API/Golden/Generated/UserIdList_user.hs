{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserIdList_user where

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
testObject_UserIdList_1 :: UserIdList
testObject_UserIdList_1 = UserIdList {mUsers = [(Id (fromJust (UUID.fromString "000020bb-0000-308f-0000-163f0000595c"))),(Id (fromJust (UUID.fromString "000046a4-0000-4d04-0000-217600002196"))),(Id (fromJust (UUID.fromString "00007e6b-0000-47fc-0000-07ab00000c2c"))),(Id (fromJust (UUID.fromString "00005bde-0000-654d-0000-7d4900005be8"))),(Id (fromJust (UUID.fromString "00000b75-0000-755f-0000-765000006bf4"))),(Id (fromJust (UUID.fromString "000022c5-0000-22a6-0000-28cb00003fa8"))),(Id (fromJust (UUID.fromString "00003e91-0000-64a8-0000-262600002b3e"))),(Id (fromJust (UUID.fromString "0000136e-0000-718f-0000-1b0100007ecc"))),(Id (fromJust (UUID.fromString "00004f3d-0000-3e0d-0000-542a00006f21")))]}
testObject_UserIdList_2 :: UserIdList
testObject_UserIdList_2 = UserIdList {mUsers = [(Id (fromJust (UUID.fromString "00002c23-0000-40df-0000-6b010000042f"))),(Id (fromJust (UUID.fromString "000001c9-0000-4633-0000-58e6000047a3"))),(Id (fromJust (UUID.fromString "00004b44-0000-7e11-0000-2ee4000053ab"))),(Id (fromJust (UUID.fromString "00003654-0000-5c41-0000-437500005509"))),(Id (fromJust (UUID.fromString "00002b19-0000-12e1-0000-71a100003578"))),(Id (fromJust (UUID.fromString "00005b24-0000-6342-0000-5e0b00007440"))),(Id (fromJust (UUID.fromString "00003797-0000-5438-0000-5fb0000001d3"))),(Id (fromJust (UUID.fromString "00005ab2-0000-4f79-0000-65c100003e5f"))),(Id (fromJust (UUID.fromString "00001437-0000-491a-0000-157f00005a5f"))),(Id (fromJust (UUID.fromString "000059d8-0000-57f8-0000-72c600000658"))),(Id (fromJust (UUID.fromString "00001cf8-0000-14e1-0000-495000004d71")))]}
testObject_UserIdList_3 :: UserIdList
testObject_UserIdList_3 = UserIdList {mUsers = [(Id (fromJust (UUID.fromString "000009c9-0000-45db-0000-5027000019db"))),(Id (fromJust (UUID.fromString "00004fd2-0000-48a4-0000-2d4100001e63"))),(Id (fromJust (UUID.fromString "00003e87-0000-4d06-0000-336400004800"))),(Id (fromJust (UUID.fromString "00000264-0000-37ed-0000-3d8b00007eea"))),(Id (fromJust (UUID.fromString "000067e3-0000-56b9-0000-7a7b00007f22"))),(Id (fromJust (UUID.fromString "00004aec-0000-4cdb-0000-793200002854"))),(Id (fromJust (UUID.fromString "0000058d-0000-1703-0000-3807000078a0"))),(Id (fromJust (UUID.fromString "00002af8-0000-2b4e-0000-001a00004b3b"))),(Id (fromJust (UUID.fromString "000075b4-0000-6a4f-0000-67d1000050cb"))),(Id (fromJust (UUID.fromString "00002eee-0000-5b30-0000-0d9800000039"))),(Id (fromJust (UUID.fromString "00002e81-0000-554c-0000-53d300000dd8")))]}
testObject_UserIdList_4 :: UserIdList
testObject_UserIdList_4 = UserIdList {mUsers = [(Id (fromJust (UUID.fromString "00006820-0000-7752-0000-65e600002365"))),(Id (fromJust (UUID.fromString "00002a84-0000-1017-0000-7bce00007156"))),(Id (fromJust (UUID.fromString "00007bd2-0000-10a0-0000-6ecd0000001d"))),(Id (fromJust (UUID.fromString "0000614e-0000-4bad-0000-5520000047da"))),(Id (fromJust (UUID.fromString "00000147-0000-6384-0000-585c00002f47"))),(Id (fromJust (UUID.fromString "000067bf-0000-4ea6-0000-55c800003d14"))),(Id (fromJust (UUID.fromString "00003c9b-0000-38cb-0000-6f77000050c6"))),(Id (fromJust (UUID.fromString "00002efb-0000-1cd8-0000-2ee600006d92"))),(Id (fromJust (UUID.fromString "00006324-0000-41a5-0000-56b90000104b"))),(Id (fromJust (UUID.fromString "00005d2c-0000-53ad-0000-0ea200005385"))),(Id (fromJust (UUID.fromString "000077d4-0000-7d13-0000-69e300001398"))),(Id (fromJust (UUID.fromString "00003ee4-0000-7ad9-0000-723900003654"))),(Id (fromJust (UUID.fromString "00005ff5-0000-0058-0000-6ad7000067d1"))),(Id (fromJust (UUID.fromString "00004d81-0000-31ff-0000-182600005f11"))),(Id (fromJust (UUID.fromString "00005eb3-0000-62fd-0000-691e00006cf9"))),(Id (fromJust (UUID.fromString "000023bf-0000-3108-0000-075300005c67"))),(Id (fromJust (UUID.fromString "00001f5a-0000-4d87-0000-746000003a40"))),(Id (fromJust (UUID.fromString "0000091d-0000-27e7-0000-18f2000058ce"))),(Id (fromJust (UUID.fromString "000048ac-0000-106d-0000-02ec00005180"))),(Id (fromJust (UUID.fromString "00004f4d-0000-1fda-0000-44c70000626b"))),(Id (fromJust (UUID.fromString "0000078f-0000-60f2-0000-19570000184c"))),(Id (fromJust (UUID.fromString "00005966-0000-6916-0000-3d4600007fc4"))),(Id (fromJust (UUID.fromString "000035de-0000-008e-0000-199b00000d51"))),(Id (fromJust (UUID.fromString "00007c67-0000-27b0-0000-1b59000049f2"))),(Id (fromJust (UUID.fromString "000079cc-0000-540f-0000-2da900001d92"))),(Id (fromJust (UUID.fromString "00007dda-0000-073d-0000-3cb000001eae"))),(Id (fromJust (UUID.fromString "00007544-0000-5415-0000-1de6000045d0"))),(Id (fromJust (UUID.fromString "00001131-0000-28d4-0000-4ca0000070af")))]}
testObject_UserIdList_5 :: UserIdList
testObject_UserIdList_5 = UserIdList {mUsers = [(Id (fromJust (UUID.fromString "00002828-0000-34e6-0000-1c55000064e6"))),(Id (fromJust (UUID.fromString "0000567a-0000-54a3-0000-5e3b00005f2f"))),(Id (fromJust (UUID.fromString "000030f6-0000-60a2-0000-61b4000070e3"))),(Id (fromJust (UUID.fromString "0000030f-0000-511a-0000-0538000039d8"))),(Id (fromJust (UUID.fromString "000010f0-0000-6e83-0000-77290000731b"))),(Id (fromJust (UUID.fromString "0000207f-0000-72dc-0000-556800004283"))),(Id (fromJust (UUID.fromString "00002e21-0000-53fb-0000-6eb1000075bd"))),(Id (fromJust (UUID.fromString "00002cd5-0000-496f-0000-679f000009d6"))),(Id (fromJust (UUID.fromString "00002b01-0000-6858-0000-655d00007fd3"))),(Id (fromJust (UUID.fromString "000057fc-0000-3da6-0000-0d7d000079a2"))),(Id (fromJust (UUID.fromString "00007145-0000-5e38-0000-6a8d00004fd4"))),(Id (fromJust (UUID.fromString "00006a4f-0000-62d5-0000-105900001d5e"))),(Id (fromJust (UUID.fromString "000050de-0000-154e-0000-28490000355b"))),(Id (fromJust (UUID.fromString "00001ef0-0000-4a38-0000-09bf000066ef"))),(Id (fromJust (UUID.fromString "000074ce-0000-6585-0000-318200000845"))),(Id (fromJust (UUID.fromString "00004c4c-0000-431a-0000-012c000026f1")))]}
