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
testObject_UserIdList_user_1 :: UserIdList
testObject_UserIdList_user_1 = UserIdList {mUsers = [(Id (fromJust (UUID.fromString "000033dd-0000-2b16-0000-7a6200005ba4"))),(Id (fromJust (UUID.fromString "00001e90-0000-5da8-0000-2e3600001d14"))),(Id (fromJust (UUID.fromString "0000729b-0000-2c8f-0000-40db00000650"))),(Id (fromJust (UUID.fromString "00000ecb-0000-6026-0000-5027000002fb"))),(Id (fromJust (UUID.fromString "0000628e-0000-32ce-0000-57f700000a69"))),(Id (fromJust (UUID.fromString "00005bc7-0000-6059-0000-3a430000236f"))),(Id (fromJust (UUID.fromString "00001810-0000-2e9f-0000-6e7900002d35"))),(Id (fromJust (UUID.fromString "00001b78-0000-2821-0000-6fa700007956"))),(Id (fromJust (UUID.fromString "000069a3-0000-3770-0000-64700000789a"))),(Id (fromJust (UUID.fromString "000072b3-0000-2e09-0000-61990000284d"))),(Id (fromJust (UUID.fromString "00003a34-0000-19b5-0000-27f10000436c"))),(Id (fromJust (UUID.fromString "00002295-0000-152d-0000-21ba00001514"))),(Id (fromJust (UUID.fromString "00007f5f-0000-29cb-0000-5b9e000076cd"))),(Id (fromJust (UUID.fromString "0000593d-0000-68c4-0000-1d6b000033d4"))),(Id (fromJust (UUID.fromString "00003cf3-0000-40ab-0000-76a100004d63"))),(Id (fromJust (UUID.fromString "00003a48-0000-5542-0000-4e45000018b0")))]}
testObject_UserIdList_user_2 :: UserIdList
testObject_UserIdList_user_2 = UserIdList {mUsers = [(Id (fromJust (UUID.fromString "00002ef5-0000-18ee-0000-01b0000062be"))),(Id (fromJust (UUID.fromString "00006a37-0000-79fa-0000-7483000016e7"))),(Id (fromJust (UUID.fromString "00003baa-0000-4ac3-0000-462d00004c0c"))),(Id (fromJust (UUID.fromString "00003834-0000-1b63-0000-33ba00002a5a"))),(Id (fromJust (UUID.fromString "000026c2-0000-7821-0000-0d820000148f"))),(Id (fromJust (UUID.fromString "00007ed1-0000-5c5e-0000-0acd00004b52"))),(Id (fromJust (UUID.fromString "00005d72-0000-17cc-0000-64d800002470"))),(Id (fromJust (UUID.fromString "000004e9-0000-400a-0000-100a00003d67"))),(Id (fromJust (UUID.fromString "00003a15-0000-0e57-0000-20830000010f"))),(Id (fromJust (UUID.fromString "000010df-0000-3f65-0000-0bd600005585"))),(Id (fromJust (UUID.fromString "000042d4-0000-51e9-0000-277f00004ccc"))),(Id (fromJust (UUID.fromString "00006402-0000-13c6-0000-669000007c4d"))),(Id (fromJust (UUID.fromString "000019db-0000-435d-0000-56d000004767"))),(Id (fromJust (UUID.fromString "00001f15-0000-1107-0000-169600001907"))),(Id (fromJust (UUID.fromString "0000585c-0000-0f23-0000-4b86000072ff"))),(Id (fromJust (UUID.fromString "000072d7-0000-73ca-0000-680d00002f1b"))),(Id (fromJust (UUID.fromString "000053e0-0000-2583-0000-194600006855"))),(Id (fromJust (UUID.fromString "00006dae-0000-04c1-0000-4318000026ab"))),(Id (fromJust (UUID.fromString "00001844-0000-161b-0000-561e000072f2"))),(Id (fromJust (UUID.fromString "000010b0-0000-5217-0000-27e0000025c4"))),(Id (fromJust (UUID.fromString "000011bd-0000-0079-0000-1089000055c6"))),(Id (fromJust (UUID.fromString "00003ef3-0000-756b-0000-68dd000014bb"))),(Id (fromJust (UUID.fromString "00005ca4-0000-50df-0000-04dc00001cf9"))),(Id (fromJust (UUID.fromString "0000070b-0000-24d9-0000-526a00006ec2")))]}
testObject_UserIdList_user_3 :: UserIdList
testObject_UserIdList_user_3 = UserIdList {mUsers = [(Id (fromJust (UUID.fromString "000071fa-0000-396f-0000-5e8500001762"))),(Id (fromJust (UUID.fromString "000005e9-0000-4edb-0000-7344000009c2"))),(Id (fromJust (UUID.fromString "000077eb-0000-7383-0000-784d000079ad"))),(Id (fromJust (UUID.fromString "00003a7d-0000-313f-0000-3b7700006dae"))),(Id (fromJust (UUID.fromString "000065fa-0000-2d8e-0000-5fec00005b99"))),(Id (fromJust (UUID.fromString "00003961-0000-694b-0000-312500007fd9"))),(Id (fromJust (UUID.fromString "00001ae7-0000-0d92-0000-396800006538"))),(Id (fromJust (UUID.fromString "00003b17-0000-1cb3-0000-55f500007783"))),(Id (fromJust (UUID.fromString "00007ef5-0000-3e2d-0000-506c00007fc3"))),(Id (fromJust (UUID.fromString "000072a1-0000-5134-0000-21ef000034ed"))),(Id (fromJust (UUID.fromString "00000c71-0000-3965-0000-0ee200003ad3"))),(Id (fromJust (UUID.fromString "000061b8-0000-2c60-0000-2cd700004730"))),(Id (fromJust (UUID.fromString "000013b4-0000-0c83-0000-11b2000000d8"))),(Id (fromJust (UUID.fromString "000003c7-0000-56d3-0000-7d1000000a0d"))),(Id (fromJust (UUID.fromString "000057e2-0000-3834-0000-724000000e10"))),(Id (fromJust (UUID.fromString "00001cda-0000-5aea-0000-184d0000020b"))),(Id (fromJust (UUID.fromString "00000235-0000-7f2c-0000-6c49000005bd"))),(Id (fromJust (UUID.fromString "00002031-0000-2d46-0000-7b9000007d9c"))),(Id (fromJust (UUID.fromString "0000687d-0000-6b2d-0000-3e0e00001ef6"))),(Id (fromJust (UUID.fromString "00001541-0000-4464-0000-687400001307"))),(Id (fromJust (UUID.fromString "00006de8-0000-7711-0000-3c51000061d6"))),(Id (fromJust (UUID.fromString "00005513-0000-7698-0000-0afb00001d6e"))),(Id (fromJust (UUID.fromString "0000613f-0000-7baa-0000-7cae00002445"))),(Id (fromJust (UUID.fromString "0000654b-0000-28cb-0000-709400006986"))),(Id (fromJust (UUID.fromString "00004216-0000-7460-0000-782700006aee"))),(Id (fromJust (UUID.fromString "00007487-0000-5c90-0000-43ff00002f14")))]}
testObject_UserIdList_user_4 :: UserIdList
testObject_UserIdList_user_4 = UserIdList {mUsers = [(Id (fromJust (UUID.fromString "000020ba-0000-65f6-0000-7e5600003aea")))]}
testObject_UserIdList_user_5 :: UserIdList
testObject_UserIdList_user_5 = UserIdList {mUsers = []}
