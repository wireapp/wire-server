{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Wire.API.Golden.Generated.ResumableAsset_user where

import Control.Lens ((.~))
import Data.Id (Id (Id))
import Data.Text.Ascii (AsciiChars (validate))
import qualified Data.UUID as UUID (fromString)
import Imports
  ( Functor (fmap),
    Maybe (Just, Nothing),
    fromJust,
    fromRight,
    read,
    undefined,
    (&),
  )
import Wire.API.Asset
  ( AssetKey (AssetKeyV3),
    AssetRetention
      ( AssetEternal,
        AssetEternalInfrequentAccess,
        AssetExpiring,
        AssetPersistent,
        AssetVolatile
      ),
    AssetToken (AssetToken, assetTokenAscii),
    ChunkSize (ChunkSize, chunkSizeBytes),
    ResumableAsset,
    assetExpires,
    assetToken,
    mkAsset,
    mkResumableAsset,
  )

testObject_ResumableAsset_user_1 :: ResumableAsset
testObject_ResumableAsset_user_1 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000010-0000-0008-0000-004300000006"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-04-13 11:37:47.393 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("5A==")))}))) (read "1864-04-09 06:01:25.576 UTC") (ChunkSize {chunkSizeBytes = 17}))

testObject_ResumableAsset_user_2 :: ResumableAsset
testObject_ResumableAsset_user_2 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000027-0000-0020-0000-003200000062"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-06-05 22:55:33.083 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("tw0NZPaJk0Hl3VHZFPTyjet4u2ZErQ==")))}))) (read "1864-04-26 00:29:12.625 UTC") (ChunkSize {chunkSizeBytes = 19}))

testObject_ResumableAsset_user_3 :: ResumableAsset
testObject_ResumableAsset_user_3 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000056-0000-0062-0000-00230000007d"))) AssetExpiring) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("f7r-XByS")))}))) (read "1864-05-12 00:54:09.852 UTC") (ChunkSize {chunkSizeBytes = 24}))

testObject_ResumableAsset_user_4 :: ResumableAsset
testObject_ResumableAsset_user_4 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000002e-0000-0005-0000-004500000020"))) AssetVolatile) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("dsGh0JVaM5x3-22SN_bey09Sxg==")))}))) (read "1864-06-07 01:55:50.143 UTC") (ChunkSize {chunkSizeBytes = 5}))

testObject_ResumableAsset_user_5 :: ResumableAsset
testObject_ResumableAsset_user_5 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000026-0000-006e-0000-005200000005"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-05-04 21:29:18.482 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("jYmC3r4Wb2wSciaI2Hgwwrzgq02OzgmU8xIr")))}))) (read "1864-05-09 07:28:34.68 UTC") (ChunkSize {chunkSizeBytes = 12}))

testObject_ResumableAsset_user_6 :: ResumableAsset
testObject_ResumableAsset_user_6 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000055-0000-007e-0000-003d00000039"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-06-07 14:00:07.598 UTC")) & assetToken .~ Nothing)) (read "1864-04-13 09:12:36.767 UTC") (ChunkSize {chunkSizeBytes = 19}))

testObject_ResumableAsset_user_7 :: ResumableAsset
testObject_ResumableAsset_user_7 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000004b-0000-0017-0000-00310000006e"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-30 20:13:45.847 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("KdZT7_v6l6f5OTs5")))}))) (read "1864-05-03 02:47:46.506 UTC") (ChunkSize {chunkSizeBytes = 8}))

testObject_ResumableAsset_user_8 :: ResumableAsset
testObject_ResumableAsset_user_8 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000001c-0000-0075-0000-004d0000007c"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-04-24 18:20:27.32 UTC")) & assetToken .~ Nothing)) (read "1864-05-07 17:08:41.651 UTC") (ChunkSize {chunkSizeBytes = 27}))

testObject_ResumableAsset_user_9 :: ResumableAsset
testObject_ResumableAsset_user_9 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000001c-0000-0037-0000-00770000002d"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-05-23 18:01:30.768 UTC")) & assetToken .~ Nothing)) (read "1864-05-17 02:44:11.614 UTC") (ChunkSize {chunkSizeBytes = 25}))

testObject_ResumableAsset_user_10 :: ResumableAsset
testObject_ResumableAsset_user_10 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000007d-0000-002e-0000-00750000006d"))) AssetEternal) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("UfKdZy8dO38=")))}))) (read "1864-05-23 07:18:12.803 UTC") (ChunkSize {chunkSizeBytes = 20}))

testObject_ResumableAsset_user_11 :: ResumableAsset
testObject_ResumableAsset_user_11 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000001d-0000-0078-0000-00470000001b"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-06-07 11:41:46.477 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("")))}))) (read "1864-05-01 11:31:08.393 UTC") (ChunkSize {chunkSizeBytes = 10}))

testObject_ResumableAsset_user_12 :: ResumableAsset
testObject_ResumableAsset_user_12 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000022-0000-007e-0000-000d00000055"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-17 11:30:44.469 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("ae_eVOnDu38P3iQh_3uBbMpIABEc_ppAoiTGmg==")))}))) (read "1864-05-06 10:40:58.261 UTC") (ChunkSize {chunkSizeBytes = 9}))

testObject_ResumableAsset_user_13 :: ResumableAsset
testObject_ResumableAsset_user_13 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000002d-0000-0075-0000-000600000003"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-04-23 13:14:02.408 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("qRj2meQ68UfzECKD4C-y")))}))) (read "1864-05-29 22:51:00.991 UTC") (ChunkSize {chunkSizeBytes = 1}))

testObject_ResumableAsset_user_14 :: ResumableAsset
testObject_ResumableAsset_user_14 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000030-0000-0069-0000-001a00000032"))) AssetPersistent) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("sJVOwiBtfAqMC56d-f7qGja7EdQ=")))}))) (read "1864-04-13 15:39:20.608 UTC") (ChunkSize {chunkSizeBytes = 12}))

testObject_ResumableAsset_user_15 :: ResumableAsset
testObject_ResumableAsset_user_15 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000002f-0000-0064-0000-007f0000004b"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-05-22 22:28:50.009 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("K2FfT2hIH0Wc-do=")))}))) (read "1864-06-02 06:12:50.515 UTC") (ChunkSize {chunkSizeBytes = 28}))

testObject_ResumableAsset_user_16 :: ResumableAsset
testObject_ResumableAsset_user_16 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000058-0000-0076-0000-00280000001b"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-05-02 08:47:43.202 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("g_73-UjzHqlT_rTQYPpXvDdEQO5VOIXyrR8=")))}))) (read "1864-05-09 15:19:28.092 UTC") (ChunkSize {chunkSizeBytes = 11}))

testObject_ResumableAsset_user_17 :: ResumableAsset
testObject_ResumableAsset_user_17 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000007c-0000-0030-0000-001a0000000f"))) AssetVolatile) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("_ozCuHYT5FEamP2vYKON")))}))) (read "1864-04-20 04:33:39.822 UTC") (ChunkSize {chunkSizeBytes = 1}))

testObject_ResumableAsset_user_18 :: ResumableAsset
testObject_ResumableAsset_user_18 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000024-0000-0042-0000-00680000004b"))) AssetEternal) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Nothing)) (read "1864-05-13 21:43:48.733 UTC") (ChunkSize {chunkSizeBytes = 26}))

testObject_ResumableAsset_user_19 :: ResumableAsset
testObject_ResumableAsset_user_19 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000016-0000-0072-0000-007700000065"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("Q41gte2seld_Ng==")))}))) (read "1864-05-26 11:47:03.579 UTC") (ChunkSize {chunkSizeBytes = 14}))

testObject_ResumableAsset_user_20 :: ResumableAsset
testObject_ResumableAsset_user_20 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000002f-0000-003c-0000-004600000044"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Nothing)) (read "1864-05-30 05:11:58.499 UTC") (ChunkSize {chunkSizeBytes = 21}))
