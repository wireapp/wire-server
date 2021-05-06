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
testObject_ResumableAsset_user_1 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000002a-0000-0065-0000-001400000067"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-04-11 05:54:51.528 UTC")) & assetToken .~ Nothing)) (read "1864-04-13 19:59:24.477 UTC") (ChunkSize {chunkSizeBytes = 6}))
testObject_ResumableAsset_user_2 :: ResumableAsset
testObject_ResumableAsset_user_2 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000003c-0000-0044-0000-00250000003c"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-04-12 20:45:47.128 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("nHgXe1xmML3qC5TczqgeACHD7L7I2ZcgVJZLsw==")))}))) (read "1864-05-25 12:55:42.529 UTC") (ChunkSize {chunkSizeBytes = 15}))
testObject_ResumableAsset_user_3 :: ResumableAsset
testObject_ResumableAsset_user_3 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000003d-0000-0017-0000-000a0000003a"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-05-02 18:03:41.513 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("kj51ur2qhwuK3zlQ4Wjv")))}))) (read "1864-05-18 15:37:45.955 UTC") (ChunkSize {chunkSizeBytes = 12}))
testObject_ResumableAsset_user_4 :: ResumableAsset
testObject_ResumableAsset_user_4 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000000f-0000-0075-0000-002000000050"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-16 07:21:00.269 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("tLBi8CuxJq26Itr4Czbs")))}))) (read "1864-05-14 14:20:35.745 UTC") (ChunkSize {chunkSizeBytes = 1}))
testObject_ResumableAsset_user_5 :: ResumableAsset
testObject_ResumableAsset_user_5 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000034-0000-0079-0000-00190000007c"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-05-23 15:14:38.548 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("PY0kim9v_6nisTYKNlA=")))}))) (read "1864-05-22 08:33:25.32 UTC") (ChunkSize {chunkSizeBytes = 23}))
testObject_ResumableAsset_user_6 :: ResumableAsset
testObject_ResumableAsset_user_6 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000045-0000-0008-0000-00750000001e"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-04-21 13:58:46.829 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("9cD2mqtS")))}))) (read "1864-06-01 10:23:28.303 UTC") (ChunkSize {chunkSizeBytes = 7}))
testObject_ResumableAsset_user_7 :: ResumableAsset
testObject_ResumableAsset_user_7 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000036-0000-001c-0000-00650000007b"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-26 11:56:39.84 UTC")) & assetToken .~ Nothing)) (read "1864-05-04 07:36:28.66 UTC") (ChunkSize {chunkSizeBytes = 8}))
testObject_ResumableAsset_user_8 :: ResumableAsset
testObject_ResumableAsset_user_8 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000003-0000-0074-0000-00140000005e"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-05-20 13:55:44.183 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("4AY=")))}))) (read "1864-04-16 23:27:03.205 UTC") (ChunkSize {chunkSizeBytes = 0}))
testObject_ResumableAsset_user_9 :: ResumableAsset
testObject_ResumableAsset_user_9 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000071-0000-0046-0000-001300000015"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-04-30 17:07:50.653 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("lcU=")))}))) (read "1864-04-30 21:22:29.487 UTC") (ChunkSize {chunkSizeBytes = 17}))
testObject_ResumableAsset_user_10 :: ResumableAsset
testObject_ResumableAsset_user_10 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000064-0000-0062-0000-005e0000004b"))) AssetEternal) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Nothing)) (read "1864-04-21 12:02:44.846 UTC") (ChunkSize {chunkSizeBytes = 23}))
testObject_ResumableAsset_user_11 :: ResumableAsset
testObject_ResumableAsset_user_11 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000003a-0000-0056-0000-005c00000080"))) AssetPersistent) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("A1Nctl89crrt7EdZuA0MGyNOOOu6")))}))) (read "1864-05-21 16:35:56.22 UTC") (ChunkSize {chunkSizeBytes = 19}))
testObject_ResumableAsset_user_12 :: ResumableAsset
testObject_ResumableAsset_user_12 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000006-0000-000d-0000-002200000043"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-05-20 07:48:05.161 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("iw==")))}))) (read "1864-05-15 20:20:44.833 UTC") (ChunkSize {chunkSizeBytes = 5}))
testObject_ResumableAsset_user_13 :: ResumableAsset
testObject_ResumableAsset_user_13 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000019-0000-002b-0000-004900000035"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-05-24 11:47:27.169 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("5XF3A8nsbFgrZQ1QjxF7IYr-kXF2AW63")))}))) (read "1864-05-15 17:28:33.358 UTC") (ChunkSize {chunkSizeBytes = 29}))
testObject_ResumableAsset_user_14 :: ResumableAsset
testObject_ResumableAsset_user_14 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000004a-0000-005c-0000-00170000000e"))) AssetPersistent) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("RmgBFVUKfu4dRWbVHMaN5qWMTMjYX6efX-GZeIU=")))}))) (read "1864-05-21 08:09:07.182 UTC") (ChunkSize {chunkSizeBytes = 20}))
testObject_ResumableAsset_user_15 :: ResumableAsset
testObject_ResumableAsset_user_15 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000003e-0000-006d-0000-000d00000009"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-04-15 19:22:29.376 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("uMfjh1E9EA==")))}))) (read "1864-05-04 09:10:24.745 UTC") (ChunkSize {chunkSizeBytes = 16}))
testObject_ResumableAsset_user_16 :: ResumableAsset
testObject_ResumableAsset_user_16 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000043-0000-0066-0000-005900000075"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-16 19:44:03.221 UTC")) & assetToken .~ Nothing)) (read "1864-05-11 03:43:08.379 UTC") (ChunkSize {chunkSizeBytes = 10}))
testObject_ResumableAsset_user_17 :: ResumableAsset
testObject_ResumableAsset_user_17 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000006-0000-0062-0000-005e00000036"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-28 22:45:35.991 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("04iwXqBQky90s23jGhmBGg==")))}))) (read "1864-05-08 15:04:54.164 UTC") (ChunkSize {chunkSizeBytes = 15}))
testObject_ResumableAsset_user_18 :: ResumableAsset
testObject_ResumableAsset_user_18 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000018-0000-0021-0000-002100000071"))) AssetExpiring) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Nothing)) (read "1864-05-18 13:37:15.074 UTC") (ChunkSize {chunkSizeBytes = 7}))
testObject_ResumableAsset_user_19 :: ResumableAsset
testObject_ResumableAsset_user_19 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000064-0000-006f-0000-00090000001d"))) AssetEternal) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("Io8odv9Ulg==")))}))) (read "1864-05-09 02:48:05.169 UTC") (ChunkSize {chunkSizeBytes = 18}))
testObject_ResumableAsset_user_20 :: ResumableAsset
testObject_ResumableAsset_user_20 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000042-0000-0067-0000-006000000022"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-23 04:42:03.306 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("Lnk=")))}))) (read "1864-06-05 04:56:13.198 UTC") (ChunkSize {chunkSizeBytes = 24}))
