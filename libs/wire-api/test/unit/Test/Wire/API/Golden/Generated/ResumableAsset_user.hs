{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ResumableAsset_user where

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
testObject_ResumableAsset_user_1 :: ResumableAsset
testObject_ResumableAsset_user_1 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000075-0000-0001-0000-003100000043"))) AssetExpiring) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("OESjXWM3WeIi8n7BFk76UJFUP6M23fQ=")))}))) (read "1864-04-22 08:05:14.129 UTC") (ChunkSize {chunkSizeBytes = 20}))
testObject_ResumableAsset_user_2 :: ResumableAsset
testObject_ResumableAsset_user_2 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000033-0000-004d-0000-000500000078"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-04-18 06:06:37.596 UTC")) & assetToken .~ Nothing)) (read "1864-04-25 21:26:17.919 UTC") (ChunkSize {chunkSizeBytes = 25}))
testObject_ResumableAsset_user_3 :: ResumableAsset
testObject_ResumableAsset_user_3 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000000e-0000-0024-0000-005c00000043"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-07 10:12:29.079 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("KZnLU5BRCK8rOLAK7fFPpubx3WT4")))}))) (read "1864-04-21 19:13:09.441 UTC") (ChunkSize {chunkSizeBytes = 22}))
testObject_ResumableAsset_user_4 :: ResumableAsset
testObject_ResumableAsset_user_4 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000003e-0000-0011-0000-006a0000002e"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-24 21:39:19.947 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("cT2apRuMN2Wjzg==")))}))) (read "1864-04-26 13:20:07.766 UTC") (ChunkSize {chunkSizeBytes = 27}))
testObject_ResumableAsset_user_5 :: ResumableAsset
testObject_ResumableAsset_user_5 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000069-0000-0007-0000-006c0000004f"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-05-01 02:10:42.299 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("9DNBfUgHWA==")))}))) (read "1864-04-29 15:30:41.988 UTC") (ChunkSize {chunkSizeBytes = 27}))
testObject_ResumableAsset_user_6 :: ResumableAsset
testObject_ResumableAsset_user_6 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000033-0000-006d-0000-005500000040"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-04-17 19:36:14.107 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("MPTh9GiCd1AU742oA4BzixmIA7Q_gy4r")))}))) (read "1864-04-15 00:47:12.29 UTC") (ChunkSize {chunkSizeBytes = 1}))
testObject_ResumableAsset_user_7 :: ResumableAsset
testObject_ResumableAsset_user_7 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000043-0000-0008-0000-005a0000005b"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-17 08:44:53.64 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("")))}))) (read "1864-05-28 08:56:29.582 UTC") (ChunkSize {chunkSizeBytes = 27}))
testObject_ResumableAsset_user_8 :: ResumableAsset
testObject_ResumableAsset_user_8 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000005b-0000-007a-0000-004d00000031"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-10 15:32:14.033 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("PVZ5K_AAVw==")))}))) (read "1864-05-13 00:08:32.422 UTC") (ChunkSize {chunkSizeBytes = 3}))
testObject_ResumableAsset_user_9 :: ResumableAsset
testObject_ResumableAsset_user_9 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000043-0000-002c-0000-002000000066"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-04-12 10:36:53.766 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("jl4YtL0cPg_udSUQrLA=")))}))) (read "1864-05-31 07:48:59.733 UTC") (ChunkSize {chunkSizeBytes = 16}))
testObject_ResumableAsset_user_10 :: ResumableAsset
testObject_ResumableAsset_user_10 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000069-0000-004c-0000-005100000076"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-05-16 01:04:57.917 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("")))}))) (read "1864-05-22 06:18:26.911 UTC") (ChunkSize {chunkSizeBytes = 21}))
testObject_ResumableAsset_user_11 :: ResumableAsset
testObject_ResumableAsset_user_11 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000022-0000-0022-0000-00190000006d"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-27 05:08:56.037 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("3dXrqw8q")))}))) (read "1864-04-28 01:30:56.035 UTC") (ChunkSize {chunkSizeBytes = 5}))
testObject_ResumableAsset_user_12 :: ResumableAsset
testObject_ResumableAsset_user_12 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000018-0000-002c-0000-002500000050"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-06-04 18:19:30.48 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("zxTyjOzDAFA=")))}))) (read "1864-05-12 11:09:50.063 UTC") (ChunkSize {chunkSizeBytes = 17}))
testObject_ResumableAsset_user_13 :: ResumableAsset
testObject_ResumableAsset_user_13 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000004b-0000-0047-0000-000a00000056"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-04-20 05:38:09.118 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("WAX0Ig==")))}))) (read "1864-04-21 14:09:58.18 UTC") (ChunkSize {chunkSizeBytes = 12}))
testObject_ResumableAsset_user_14 :: ResumableAsset
testObject_ResumableAsset_user_14 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000034-0000-0078-0000-007b00000038"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-01 03:37:10.705 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("E-chLTuCeHTlSFg71w==")))}))) (read "1864-04-12 14:24:29.465 UTC") (ChunkSize {chunkSizeBytes = 30}))
testObject_ResumableAsset_user_15 :: ResumableAsset
testObject_ResumableAsset_user_15 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000001d-0000-002f-0000-005200000002"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-20 10:09:10.123 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("s2BLzVNp983FOGSMrIKf")))}))) (read "1864-04-20 08:03:27.85 UTC") (ChunkSize {chunkSizeBytes = 28}))
testObject_ResumableAsset_user_16 :: ResumableAsset
testObject_ResumableAsset_user_16 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000078-0000-0078-0000-003d0000001a"))) AssetExpiring) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("2NW5vCe9IT2fjK0p4-AsV666pPLdatGbxAkd")))}))) (read "1864-05-24 21:12:39.026 UTC") (ChunkSize {chunkSizeBytes = 10}))
testObject_ResumableAsset_user_17 :: ResumableAsset
testObject_ResumableAsset_user_17 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000068-0000-005c-0000-003500000073"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-04-16 01:47:02.854 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("IA==")))}))) (read "1864-06-02 14:20:57.382 UTC") (ChunkSize {chunkSizeBytes = 14}))
testObject_ResumableAsset_user_18 :: ResumableAsset
testObject_ResumableAsset_user_18 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000012-0000-0052-0000-004a00000022"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-04-27 10:43:08.147 UTC")) & assetToken .~ Nothing)) (read "1864-05-26 16:10:23.046 UTC") (ChunkSize {chunkSizeBytes = 10}))
testObject_ResumableAsset_user_19 :: ResumableAsset
testObject_ResumableAsset_user_19 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000005a-0000-0062-0000-002c00000022"))) AssetEternal) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("P0QS9A==")))}))) (read "1864-04-18 23:07:57.841 UTC") (ChunkSize {chunkSizeBytes = 7}))
testObject_ResumableAsset_user_20 :: ResumableAsset
testObject_ResumableAsset_user_20 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000049-0000-0006-0000-004700000075"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-04-18 11:01:04.899 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("-KFV")))}))) (read "1864-05-26 12:37:39.432 UTC") (ChunkSize {chunkSizeBytes = 30}))
