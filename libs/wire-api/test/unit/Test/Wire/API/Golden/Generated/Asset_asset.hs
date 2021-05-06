{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Asset_asset where
import Control.Lens ( (.~) )
import Data.Id ( Id(Id) )
import Data.Text.Ascii ( AsciiChars(validate) )
import Imports
    ( Functor(fmap),
      Maybe(Nothing, Just),
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
      Asset,
      AssetKey(AssetKeyV3),
      AssetRetention(AssetPersistent, AssetVolatile, AssetEternal,
                     AssetExpiring, AssetEternalInfrequentAccess),
      AssetToken(AssetToken, assetTokenAscii) )

testObject_Asset_asset_1 :: Asset
testObject_Asset_asset_1 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000024-0000-0067-0000-004700000045"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-04-18 21:18:03.722 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("qkfY4dMXNAVhjIrew0_GriI=")))}))
testObject_Asset_asset_2 :: Asset
testObject_Asset_asset_2 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000023-0000-0040-0000-002200000049"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-05-08 14:40:41.714 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("vyEalbAJaRI=")))}))
testObject_Asset_asset_3 :: Asset
testObject_Asset_asset_3 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000054-0000-007c-0000-005b00000064"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-04-27 12:24:02.109 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("gka0gL0qqHIcSUUvsQDL8gghQ6k=")))}))
testObject_Asset_asset_4 :: Asset
testObject_Asset_asset_4 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000051-0000-004a-0000-007e00000076"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-05-25 03:19:09.746 UTC")) & assetToken .~ Nothing)
testObject_Asset_asset_5 :: Asset
testObject_Asset_asset_5 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000050-0000-002e-0000-003a00000043"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-04-09 00:40:22.046 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("KmBmcFNfuv2wEba1_2sfKb9KBbo0VbfQeRdA-yI=")))}))
testObject_Asset_asset_6 :: Asset
testObject_Asset_asset_6 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000048-0000-000b-0000-002b0000007f"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-07 19:10:55.438 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("V5skjfKE3Ih0R4YXXRIsnfsJrqcqZ9TRDiM=")))}))
testObject_Asset_asset_7 :: Asset
testObject_Asset_asset_7 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000044-0000-0051-0000-00540000002e"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-04-30 12:13:31.549 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("iUx-RaA=")))}))
testObject_Asset_asset_8 :: Asset
testObject_Asset_asset_8 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000003c-0000-004a-0000-005400000064"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-03 14:15:17.08 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("6iw2gV0-nje8vw==")))}))
testObject_Asset_asset_9 :: Asset
testObject_Asset_asset_9 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000030-0000-0080-0000-000600000043"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-12 08:37:16.149 UTC")) & assetToken .~ Nothing)
testObject_Asset_asset_10 :: Asset
testObject_Asset_asset_10 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000047-0000-0048-0000-006b00000044"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-06-06 11:05:22.669 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("LNY=")))}))
testObject_Asset_asset_11 :: Asset
testObject_Asset_asset_11 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000067-0000-005f-0000-001a00000002"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-24 19:19:23.278 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("nDPO5QpM1CE2p8Aptm8XoKMCwwV1")))}))
testObject_Asset_asset_12 :: Asset
testObject_Asset_asset_12 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000073-0000-0008-0000-000200000006"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-04-10 16:59:04.591 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("j1j3p850VdVPxtH4")))}))
testObject_Asset_asset_13 :: Asset
testObject_Asset_asset_13 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000000e-0000-0065-0000-005400000043"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-06-07 23:52:13.731 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("mUjE")))}))
testObject_Asset_asset_14 :: Asset
testObject_Asset_asset_14 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000063-0000-0005-0000-002600000057"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-11 00:30:12.084 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("-4_EL0-PbP0=")))}))
testObject_Asset_asset_15 :: Asset
testObject_Asset_asset_15 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000006e-0000-0038-0000-00510000003b"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-01 11:17:26.268 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("bhIAJ2LCvw27tgzbpabjiYwekUhzr7Q=")))}))
testObject_Asset_asset_16 :: Asset
testObject_Asset_asset_16 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000047-0000-0006-0000-000500000018"))) AssetPersistent) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Nothing)
testObject_Asset_asset_17 :: Asset
testObject_Asset_asset_17 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000006f-0000-0024-0000-001300000013"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-15 08:32:29.528 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("QR2td5s=")))}))
testObject_Asset_asset_18 :: Asset
testObject_Asset_asset_18 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000005e-0000-0010-0000-00270000004a"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-05-24 04:09:38.204 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("1MUpJUN-ijgzfWLzvHPZ6HCGi0Dr7g==")))}))
testObject_Asset_asset_19 :: Asset
testObject_Asset_asset_19 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000006b-0000-0035-0000-000700000017"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-04-24 17:23:02.91 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("-bPYfj6xLeY=")))}))
testObject_Asset_asset_20 :: Asset
testObject_Asset_asset_20 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000009-0000-005a-0000-003a00000047"))) AssetPersistent) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Nothing)
