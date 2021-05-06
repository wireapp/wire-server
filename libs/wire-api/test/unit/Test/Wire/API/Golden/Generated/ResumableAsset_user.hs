{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ResumableAsset_user where
import Control.Lens ( (.~) )
import Data.Id ( Id(Id) )
import Data.Text.Ascii ( AsciiChars(validate) )
import Imports
    ( Functor(fmap),
      Maybe(Just, Nothing),
      undefined,
      read,
      fromRight,
      (&),
      fromJust )
import qualified Data.UUID as UUID ( fromString )
import Wire.API.Asset
    ( assetExpires,
      assetToken,
      mkAsset,
      mkResumableAsset,
      AssetKey(AssetKeyV3),
      AssetRetention(AssetEternalInfrequentAccess, AssetExpiring,
                     AssetEternal, AssetVolatile, AssetPersistent),
      AssetToken(AssetToken, assetTokenAscii),
      ChunkSize(ChunkSize, chunkSizeBytes),
      ResumableAsset )

testObject_ResumableAsset_user_1 :: ResumableAsset
testObject_ResumableAsset_user_1 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000050-0000-000e-0000-006e00000080"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-06-05 06:55:08.946 UTC")) & assetToken .~ Nothing)) (read "1864-05-14 04:56:49.274 UTC") (ChunkSize {chunkSizeBytes = 23}))
testObject_ResumableAsset_user_2 :: ResumableAsset
testObject_ResumableAsset_user_2 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000002-0000-002a-0000-002a0000006b"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-30 00:12:17.947 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("bE2iHqrH1gvgGhghiPLdOGwRh4K6hKl5RA==")))}))) (read "1864-04-14 02:10:24.869 UTC") (ChunkSize {chunkSizeBytes = 30}))
testObject_ResumableAsset_user_3 :: ResumableAsset
testObject_ResumableAsset_user_3 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000003e-0000-000e-0000-005a00000063"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-09 10:40:12.141 UTC")) & assetToken .~ Nothing)) (read "1864-04-25 13:23:42.509 UTC") (ChunkSize {chunkSizeBytes = 3}))
testObject_ResumableAsset_user_4 :: ResumableAsset
testObject_ResumableAsset_user_4 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000052-0000-0043-0000-005200000046"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-13 07:31:17.174 UTC")) & assetToken .~ Nothing)) (read "1864-05-04 03:54:52.284 UTC") (ChunkSize {chunkSizeBytes = 21}))
testObject_ResumableAsset_user_5 :: ResumableAsset
testObject_ResumableAsset_user_5 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000078-0000-002a-0000-006000000073"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-20 23:24:27.148 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("yjlc5Jyb")))}))) (read "1864-05-04 04:02:12.362 UTC") (ChunkSize {chunkSizeBytes = 29}))
testObject_ResumableAsset_user_6 :: ResumableAsset
testObject_ResumableAsset_user_6 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000016-0000-0014-0000-003100000003"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-04-16 22:06:28.087 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("IsQusfkHnmjjwYcZCm8zwHMq")))}))) (read "1864-05-17 07:23:53.218 UTC") (ChunkSize {chunkSizeBytes = 2}))
testObject_ResumableAsset_user_7 :: ResumableAsset
testObject_ResumableAsset_user_7 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000000-0000-003f-0000-006700000076"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-23 09:59:36.904 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("grrLvIfGVg==")))}))) (read "1864-05-08 07:52:28.66 UTC") (ChunkSize {chunkSizeBytes = 16}))
testObject_ResumableAsset_user_8 :: ResumableAsset
testObject_ResumableAsset_user_8 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000022-0000-0024-0000-00760000001d"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-04-11 05:04:08.422 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("-smEKQBboYifmx6BLxId")))}))) (read "1864-05-12 10:24:15.817 UTC") (ChunkSize {chunkSizeBytes = 7}))
testObject_ResumableAsset_user_9 :: ResumableAsset
testObject_ResumableAsset_user_9 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000013-0000-001b-0000-003c00000068"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-28 04:05:02.146 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("faKdy8abY4bo9vzamtMRXr3b-qw=")))}))) (read "1864-04-28 12:02:30.257 UTC") (ChunkSize {chunkSizeBytes = 25}))
testObject_ResumableAsset_user_10 :: ResumableAsset
testObject_ResumableAsset_user_10 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000001a-0000-004e-0000-005b0000001a"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-07 05:20:30.707 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("oJw8BHzskCQkbmkPo3ROA2D1E0A=")))}))) (read "1864-05-04 08:03:17.182 UTC") (ChunkSize {chunkSizeBytes = 8}))
testObject_ResumableAsset_user_11 :: ResumableAsset
testObject_ResumableAsset_user_11 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000022-0000-005a-0000-00000000004f"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-30 07:42:31.987 UTC")) & assetToken .~ Nothing)) (read "1864-04-25 21:13:48.21 UTC") (ChunkSize {chunkSizeBytes = 20}))
testObject_ResumableAsset_user_12 :: ResumableAsset
testObject_ResumableAsset_user_12 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000003a-0000-0067-0000-00030000001f"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-06-07 06:10:55.138 UTC")) & assetToken .~ Nothing)) (read "1864-05-20 17:41:45.874 UTC") (ChunkSize {chunkSizeBytes = 16}))
testObject_ResumableAsset_user_13 :: ResumableAsset
testObject_ResumableAsset_user_13 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000004e-0000-0069-0000-001e00000031"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-23 19:58:03.065 UTC")) & assetToken .~ Nothing)) (read "1864-05-21 09:40:37.976 UTC") (ChunkSize {chunkSizeBytes = 26}))
testObject_ResumableAsset_user_14 :: ResumableAsset
testObject_ResumableAsset_user_14 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000014-0000-0070-0000-000200000049"))) AssetEternal) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("LQYxXp0xy-BQPXJ2eniYumIrPg59rUG_")))}))) (read "1864-04-14 03:02:56.233 UTC") (ChunkSize {chunkSizeBytes = 27}))
testObject_ResumableAsset_user_15 :: ResumableAsset
testObject_ResumableAsset_user_15 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000076-0000-0067-0000-007e00000048"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-03 01:35:52.139 UTC")) & assetToken .~ Nothing)) (read "1864-04-26 22:52:11.21 UTC") (ChunkSize {chunkSizeBytes = 11}))
testObject_ResumableAsset_user_16 :: ResumableAsset
testObject_ResumableAsset_user_16 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000026-0000-0049-0000-001f00000037"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-25 10:11:32.381 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("kJ9Nr_-xUxVW1LknuBhrOblMMVdPR5jn")))}))) (read "1864-06-07 18:41:41.113 UTC") (ChunkSize {chunkSizeBytes = 2}))
testObject_ResumableAsset_user_17 :: ResumableAsset
testObject_ResumableAsset_user_17 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000003-0000-0011-0000-000b00000068"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-04-23 06:52:41.397 UTC")) & assetToken .~ Nothing)) (read "1864-04-19 11:02:12.344 UTC") (ChunkSize {chunkSizeBytes = 17}))
testObject_ResumableAsset_user_18 :: ResumableAsset
testObject_ResumableAsset_user_18 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000059-0000-0075-0000-004f00000006"))) AssetVolatile) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("XgifDHwKOyr0yFd4i086xlakblJ9CeM=")))}))) (read "1864-04-10 05:42:12.897 UTC") (ChunkSize {chunkSizeBytes = 10}))
testObject_ResumableAsset_user_19 :: ResumableAsset
testObject_ResumableAsset_user_19 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000055-0000-003c-0000-005d00000033"))) AssetPersistent) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Nothing)) (read "1864-06-02 07:43:12.328 UTC") (ChunkSize {chunkSizeBytes = 15}))
testObject_ResumableAsset_user_20 :: ResumableAsset
testObject_ResumableAsset_user_20 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000055-0000-0071-0000-002500000072"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-04-21 08:25:01.451 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("8CffkDw=")))}))) (read "1864-04-23 23:01:00.658 UTC") (ChunkSize {chunkSizeBytes = 15}))
