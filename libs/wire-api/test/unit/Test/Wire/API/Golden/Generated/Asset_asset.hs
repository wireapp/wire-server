{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Asset_asset where

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
testObject_Asset_asset_1 :: Asset
testObject_Asset_asset_1 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000036-0000-0035-0000-005a0000001e"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-04-20 14:55:04.668 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("oow1MwHLcxn6YQ==")))}))
testObject_Asset_asset_2 :: Asset
testObject_Asset_asset_2 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000005a-0000-0023-0000-001f00000049"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-02 14:05:59.187 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("FZE8mUaW50uiiLcelGUVFR6K8tc1i2csvuA=")))}))
testObject_Asset_asset_3 :: Asset
testObject_Asset_asset_3 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000001b-0000-006b-0000-006a0000003e"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-10 15:45:43.905 UTC")) & assetToken .~ Nothing)
testObject_Asset_asset_4 :: Asset
testObject_Asset_asset_4 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000038-0000-0065-0000-00450000002c"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-04-19 20:30:30.846 UTC")) & assetToken .~ Nothing)
testObject_Asset_asset_5 :: Asset
testObject_Asset_asset_5 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000026-0000-0056-0000-003300000016"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-06-03 11:34:03.012 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("wRtjl95seDL6bvMiLGQPg1wnO8ztch_4semHI40=")))}))
testObject_Asset_asset_6 :: Asset
testObject_Asset_asset_6 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000023-0000-0056-0000-004800000001"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-07 14:16:07.289 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("gw==")))}))
testObject_Asset_asset_7 :: Asset
testObject_Asset_asset_7 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000078-0000-0021-0000-007700000067"))) AssetVolatile) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("1zdRNcBWcW99xieSCWQ=")))}))
testObject_Asset_asset_8 :: Asset
testObject_Asset_asset_8 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000033-0000-006f-0000-008000000063"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-29 07:49:41.202 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("mW6lIW8Q9eDEOKgd2GinpffIvUJxy5vW")))}))
testObject_Asset_asset_9 :: Asset
testObject_Asset_asset_9 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000043-0000-0014-0000-006300000013"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Nothing)
testObject_Asset_asset_10 :: Asset
testObject_Asset_asset_10 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000068-0000-0053-0000-002a0000006b"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-16 08:32:38.645 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("zMP_6aNaHSgJNi692Q==")))}))
testObject_Asset_asset_11 :: Asset
testObject_Asset_asset_11 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000007c-0000-0047-0000-000b00000023"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-08 21:11:06.466 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("UKKEv5x2t6WV_pPReHJAKA==")))}))
testObject_Asset_asset_12 :: Asset
testObject_Asset_asset_12 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000001c-0000-0005-0000-00700000002c"))) AssetExpiring) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Nothing)
testObject_Asset_asset_13 :: Asset
testObject_Asset_asset_13 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000074-0000-0045-0000-000100000015"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-05-16 08:23:51.545 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("YNWBuv3oB-FDlKfBDLuy")))}))
testObject_Asset_asset_14 :: Asset
testObject_Asset_asset_14 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000002e-0000-0060-0000-00770000002d"))) AssetEternal) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("OM3oPnTebVz7ijQ=")))}))
testObject_Asset_asset_15 :: Asset
testObject_Asset_asset_15 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000041-0000-006a-0000-00240000005b"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-24 00:01:01.296 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("orrTIJb_HIhA4OIJ7Z6i")))}))
testObject_Asset_asset_16 :: Asset
testObject_Asset_asset_16 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000035-0000-0062-0000-000f0000003d"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-05-13 03:02:00.718 UTC")) & assetToken .~ Nothing)
testObject_Asset_asset_17 :: Asset
testObject_Asset_asset_17 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000000a-0000-002a-0000-004b0000001d"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-12 15:46:42.683 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("r8vBwBsmA0dQ_keDqvBO9A-LIpYgqLy3KB_r")))}))
testObject_Asset_asset_18 :: Asset
testObject_Asset_asset_18 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000002f-0000-0012-0000-005d0000007e"))) AssetEternal) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("Y7m_1eHelDs=")))}))
testObject_Asset_asset_19 :: Asset
testObject_Asset_asset_19 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000080-0000-003d-0000-006d00000021"))) AssetPersistent) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("B-oWcGCwRlqp")))}))
testObject_Asset_asset_20 :: Asset
testObject_Asset_asset_20 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000038-0000-0028-0000-00000000004f"))) AssetPersistent) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("xDMDrGdCEQ==")))}))
