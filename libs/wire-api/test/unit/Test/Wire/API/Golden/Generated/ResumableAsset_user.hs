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
testObject_ResumableAsset_user_1 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000000-0000-004e-0000-005200000033"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-05 20:13:17.711 UTC")) & assetToken .~ Nothing)) (read "1864-05-23 11:11:28.677 UTC") (ChunkSize {chunkSizeBytes = 9}))
testObject_ResumableAsset_user_2 :: ResumableAsset
testObject_ResumableAsset_user_2 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000027-0000-0060-0000-000e00000065"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-14 06:54:23.065 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("YmC8rXtxN3f0_wR0FRp5UV-KAESvYUjz6FQ=")))}))) (read "1864-05-04 08:38:07.795 UTC") (ChunkSize {chunkSizeBytes = 21}))
testObject_ResumableAsset_user_3 :: ResumableAsset
testObject_ResumableAsset_user_3 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000003d-0000-0010-0000-002100000078"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-28 23:29:42.106 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("EzG0Ol5F-_NDEtLpqKgMFQ==")))}))) (read "1864-05-11 06:33:54.06 UTC") (ChunkSize {chunkSizeBytes = 30}))
testObject_ResumableAsset_user_4 :: ResumableAsset
testObject_ResumableAsset_user_4 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000051-0000-0053-0000-006400000051"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-04-19 19:40:42.085 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("FEUyiHLyceSlvB_p")))}))) (read "1864-06-04 05:54:44.326 UTC") (ChunkSize {chunkSizeBytes = 15}))
testObject_ResumableAsset_user_5 :: ResumableAsset
testObject_ResumableAsset_user_5 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000053-0000-0073-0000-006500000070"))) AssetPersistent) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("ZIl8bmJQ1YB4")))}))) (read "1864-05-16 18:58:24.163 UTC") (ChunkSize {chunkSizeBytes = 12}))
testObject_ResumableAsset_user_6 :: ResumableAsset
testObject_ResumableAsset_user_6 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000075-0000-0034-0000-006e0000005d"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-20 12:39:39.919 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("BJhDcuQSDZk43mZCEMthrxo3O_qdytZX")))}))) (read "1864-05-28 10:57:39.935 UTC") (ChunkSize {chunkSizeBytes = 22}))
testObject_ResumableAsset_user_7 :: ResumableAsset
testObject_ResumableAsset_user_7 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000004a-0000-001f-0000-002000000031"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-10 15:30:18.679 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("thuTsmFSrfedAiuuZXDxCWKx2W5ZfjJebr7mRA==")))}))) (read "1864-05-02 15:56:30.069 UTC") (ChunkSize {chunkSizeBytes = 19}))
testObject_ResumableAsset_user_8 :: ResumableAsset
testObject_ResumableAsset_user_8 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000027-0000-0005-0000-000600000005"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-04-22 15:46:15.125 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("yKAjUxwOffahYcN5If8Uc2NqRCk=")))}))) (read "1864-04-13 00:27:00.121 UTC") (ChunkSize {chunkSizeBytes = 23}))
testObject_ResumableAsset_user_9 :: ResumableAsset
testObject_ResumableAsset_user_9 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000000d-0000-0080-0000-007200000042"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-04-25 13:25:58.723 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("Ua7C4Rn_jI_xRwLBJhz2")))}))) (read "1864-06-06 00:36:06.459 UTC") (ChunkSize {chunkSizeBytes = 11}))
testObject_ResumableAsset_user_10 :: ResumableAsset
testObject_ResumableAsset_user_10 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000073-0000-000a-0000-00110000003f"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-10 22:01:12.264 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("pFEEncJkxOm0_diy1jwckzLbFiUY")))}))) (read "1864-06-01 23:07:02.868 UTC") (ChunkSize {chunkSizeBytes = 1}))
testObject_ResumableAsset_user_11 :: ResumableAsset
testObject_ResumableAsset_user_11 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000005c-0000-0048-0000-002b00000048"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-22 17:09:13.328 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("HO-Xf6JaCQ==")))}))) (read "1864-05-12 17:28:05.622 UTC") (ChunkSize {chunkSizeBytes = 5}))
testObject_ResumableAsset_user_12 :: ResumableAsset
testObject_ResumableAsset_user_12 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000006a-0000-001f-0000-001e00000066"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-04-12 01:23:50.908 UTC")) & assetToken .~ Nothing)) (read "1864-06-08 07:41:24.384 UTC") (ChunkSize {chunkSizeBytes = 8}))
testObject_ResumableAsset_user_13 :: ResumableAsset
testObject_ResumableAsset_user_13 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000031-0000-002c-0000-00720000001b"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-06-03 10:15:20.273 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("mzaxvOtJnKph9rblrzfUCAR_")))}))) (read "1864-05-01 22:15:26.097 UTC") (ChunkSize {chunkSizeBytes = 20}))
testObject_ResumableAsset_user_14 :: ResumableAsset
testObject_ResumableAsset_user_14 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000000a-0000-0017-0000-003d0000001f"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-01 07:34:15.013 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("VHLZw5ZFeNcUnU2_WwSCKw2QqWr5")))}))) (read "1864-05-13 07:03:52.012 UTC") (ChunkSize {chunkSizeBytes = 14}))
testObject_ResumableAsset_user_15 :: ResumableAsset
testObject_ResumableAsset_user_15 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000014-0000-0045-0000-00790000006b"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-02 11:59:42.802 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("bxdH1D6z0sJJAE2klNZ5q-wQpOFWRQ==")))}))) (read "1864-05-26 23:17:27.429 UTC") (ChunkSize {chunkSizeBytes = 23}))
testObject_ResumableAsset_user_16 :: ResumableAsset
testObject_ResumableAsset_user_16 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000007e-0000-0037-0000-006200000026"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("kaAZJB90K5E42djI1KIxxoXLeN6IEmm4lA==")))}))) (read "1864-04-30 16:12:28.234 UTC") (ChunkSize {chunkSizeBytes = 2}))
testObject_ResumableAsset_user_17 :: ResumableAsset
testObject_ResumableAsset_user_17 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000003b-0000-0030-0000-000a0000004d"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-01 23:27:58.477 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("m_DtO0HLZiCI")))}))) (read "1864-06-01 21:49:34.139 UTC") (ChunkSize {chunkSizeBytes = 28}))
testObject_ResumableAsset_user_18 :: ResumableAsset
testObject_ResumableAsset_user_18 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000025-0000-000e-0000-001100000020"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-31 18:06:53.938 UTC")) & assetToken .~ Nothing)) (read "1864-04-30 14:40:23.1 UTC") (ChunkSize {chunkSizeBytes = 20}))
testObject_ResumableAsset_user_19 :: ResumableAsset
testObject_ResumableAsset_user_19 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000051-0000-0034-0000-003d00000071"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-04-17 19:53:19.958 UTC")) & assetToken .~ Nothing)) (read "1864-05-14 05:15:13.391 UTC") (ChunkSize {chunkSizeBytes = 18}))
testObject_ResumableAsset_user_20 :: ResumableAsset
testObject_ResumableAsset_user_20 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000004d-0000-0054-0000-001700000078"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-04-24 23:29:39.73 UTC")) & assetToken .~ Nothing)) (read "1864-05-21 20:26:39.433 UTC") (ChunkSize {chunkSizeBytes = 7}))
