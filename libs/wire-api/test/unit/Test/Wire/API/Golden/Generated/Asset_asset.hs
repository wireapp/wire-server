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
module Test.Wire.API.Golden.Generated.Asset_asset where

import Control.Lens ((.~))
import Data.Id (Id (Id))
import Data.Text.Ascii (AsciiChars (validate))
import qualified Data.UUID as UUID (fromString)
import Imports (Functor (fmap), Maybe (Just, Nothing), fromJust, fromRight, read, undefined, (&))
import Wire.API.Asset
  ( Asset,
    AssetKey (AssetKeyV3),
    AssetRetention
      ( AssetEternal,
        AssetEternalInfrequentAccess,
        AssetExpiring,
        AssetPersistent,
        AssetVolatile
      ),
    AssetToken (AssetToken, assetTokenAscii),
    assetExpires,
    assetToken,
    mkAsset,
  )

testObject_Asset_asset_1 :: Asset
testObject_Asset_asset_1 =
  ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000004b-0000-0017-0000-003e00000033"))) AssetExpiring)
      & assetExpires .~ (fmap read (Just "1864-04-30 15:58:55.452 UTC"))
      & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("Kun4JaxR6QuASXywDhzx")))})
  )

testObject_Asset_asset_2 :: Asset
testObject_Asset_asset_2 =
  ( mkAsset
      (AssetKeyV3 (Id (fromJust (UUID.fromString "00000008-0000-006c-0000-001900000036"))) AssetEternalInfrequentAccess)
      & assetExpires .~ (fmap read (Just "1864-06-04 17:39:43.924 UTC"))
      & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("mPuul678vuJVZ_u9lQ==")))})
  )

testObject_Asset_asset_3 :: Asset
testObject_Asset_asset_3 =
  ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000055-0000-0071-0000-002e00000020"))) AssetEternal)
      & assetExpires .~ (fmap read (Just "1864-05-18 20:18:13.438 UTC"))
      & assetToken .~ Nothing
  )

testObject_Asset_asset_4 :: Asset
testObject_Asset_asset_4 =
  ( mkAsset
      (AssetKeyV3 (Id (fromJust (UUID.fromString "00000063-0000-0044-0000-003000000059"))) AssetEternalInfrequentAccess)
      & assetExpires .~ (fmap read (Nothing))
      & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("IRKruiPSiANiX1fL")))})
  )

testObject_Asset_asset_5 :: Asset
testObject_Asset_asset_5 =
  ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000019-0000-005b-0000-001d00000056"))) AssetVolatile)
      & assetExpires .~ (fmap read (Just "1864-05-11 14:38:25.874 UTC"))
      & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("BrbiaM1RxJlqjlqq7quuPSc=")))})
  )

testObject_Asset_asset_6 :: Asset
testObject_Asset_asset_6 =
  ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000000e-0000-0046-0000-00560000005e"))) AssetPersistent)
      & assetExpires .~ (fmap read (Just "1864-05-25 01:19:16.676 UTC"))
      & assetToken .~ Nothing
  )

testObject_Asset_asset_7 :: Asset
testObject_Asset_asset_7 =
  ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000013-0000-002e-0000-003000000042"))) AssetEternal)
      & assetExpires .~ (fmap read (Just "1864-04-14 08:45:43.05 UTC"))
      & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("_N9ERJGmbZtd6XlW_6O12bxuNe4=")))})
  )

testObject_Asset_asset_8 :: Asset
testObject_Asset_asset_8 =
  ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000073-0000-003e-0000-00120000000c"))) AssetEternal)
      & assetExpires .~ (fmap read (Nothing))
      & assetToken .~ Nothing
  )

testObject_Asset_asset_9 :: Asset
testObject_Asset_asset_9 =
  ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000006-0000-004b-0000-004f00000025"))) AssetPersistent)
      & assetExpires .~ (fmap read (Just "1864-05-21 01:34:09.726 UTC"))
      & assetToken .~ Nothing
  )

testObject_Asset_asset_10 :: Asset
testObject_Asset_asset_10 =
  ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000065-0000-0080-0000-003400000061"))) AssetEternal)
      & assetExpires .~ (fmap read (Nothing))
      & assetToken .~ Nothing
  )

testObject_Asset_asset_11 :: Asset
testObject_Asset_asset_11 =
  ( mkAsset
      (AssetKeyV3 (Id (fromJust (UUID.fromString "00000014-0000-0077-0000-001e00000076"))) AssetEternalInfrequentAccess)
      & assetExpires .~ (fmap read (Just "1864-05-11 16:58:59.746 UTC"))
      & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("DnlRW9Q=")))})
  )

testObject_Asset_asset_12 :: Asset
testObject_Asset_asset_12 =
  ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000001d-0000-0076-0000-003800000021"))) AssetPersistent)
      & assetExpires .~ (fmap read (Nothing))
      & assetToken .~ Nothing
  )

testObject_Asset_asset_13 :: Asset
testObject_Asset_asset_13 =
  ( mkAsset
      (AssetKeyV3 (Id (fromJust (UUID.fromString "00000030-0000-0036-0000-003c0000000a"))) AssetEternalInfrequentAccess)
      & assetExpires .~ (fmap read (Just "1864-04-30 19:37:57.302 UTC"))
      & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("n7CJBcdOSKznRmOypWXsGfEE0g==")))})
  )

testObject_Asset_asset_14 :: Asset
testObject_Asset_asset_14 =
  ( mkAsset
      (AssetKeyV3 (Id (fromJust (UUID.fromString "00000047-0000-0012-0000-005500000062"))) AssetEternalInfrequentAccess)
      & assetExpires .~ (fmap read (Just "1864-05-06 09:09:55.146 UTC"))
      & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("LYfUg4qlMjw=")))})
  )

testObject_Asset_asset_15 :: Asset
testObject_Asset_asset_15 =
  ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000030-0000-0074-0000-00660000004c"))) AssetPersistent)
      & assetExpires .~ (fmap read (Nothing))
      & assetToken .~ Nothing
  )

testObject_Asset_asset_16 :: Asset
testObject_Asset_asset_16 =
  ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000048-0000-0051-0000-005d00000070"))) AssetVolatile)
      & assetExpires .~ (fmap read (Just "1864-05-04 02:19:12.52 UTC"))
      & assetToken .~ Nothing
  )

testObject_Asset_asset_17 :: Asset
testObject_Asset_asset_17 =
  ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000017-0000-000d-0000-00680000003e"))) AssetPersistent)
      & assetExpires .~ (fmap read (Just "1864-04-09 17:00:39.763 UTC"))
      & assetToken .~ Nothing
  )

testObject_Asset_asset_18 :: Asset
testObject_Asset_asset_18 =
  ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000003e-0000-0032-0000-004d00000070"))) AssetEternal)
      & assetExpires .~ (fmap read (Just "1864-04-12 20:53:21.25 UTC"))
      & assetToken .~ Nothing
  )

testObject_Asset_asset_19 :: Asset
testObject_Asset_asset_19 =
  ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000021-0000-0062-0000-002a0000006b"))) AssetVolatile)
      & assetExpires
      .~ (fmap read (Nothing))
      & assetToken
      .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("4wm3D03aqvZ_0oKFtwXCYnSTC7m_z1E=")))})
  )

testObject_Asset_asset_20 :: Asset
testObject_Asset_asset_20 =
  ( mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000053-0000-0072-0000-001700000047"))) AssetVolatile)
      & assetExpires .~ (fmap read (Just "1864-04-25 16:48:39.986 UTC"))
      & assetToken .~ Nothing
  )
