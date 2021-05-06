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
testObject_Asset_asset_1 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000003-0000-0047-0000-002b00000035"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-04-30 03:10:19.364 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("Qbg=")))}))
testObject_Asset_asset_2 :: Asset
testObject_Asset_asset_2 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000001d-0000-0046-0000-000300000009"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-04-12 02:42:12.21 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("AkEj3E90pCo=")))}))
testObject_Asset_asset_3 :: Asset
testObject_Asset_asset_3 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000000a-0000-003a-0000-005400000040"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-04-18 12:32:43.397 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("")))}))
testObject_Asset_asset_4 :: Asset
testObject_Asset_asset_4 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000034-0000-0015-0000-001f00000000"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-17 11:43:40.098 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("BJ3h7lPkTc8CAK7WIYnfO3mq")))}))
testObject_Asset_asset_5 :: Asset
testObject_Asset_asset_5 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000038-0000-0024-0000-000c00000054"))) AssetPersistent) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("zab3EB02AgEDEkuhIw5hhkze7YA=")))}))
testObject_Asset_asset_6 :: Asset
testObject_Asset_asset_6 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000001c-0000-0050-0000-000c00000070"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-06-05 00:17:19.553 UTC")) & assetToken .~ Nothing)
testObject_Asset_asset_7 :: Asset
testObject_Asset_asset_7 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000063-0000-0076-0000-007100000067"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-17 14:20:44.514 UTC")) & assetToken .~ Nothing)
testObject_Asset_asset_8 :: Asset
testObject_Asset_asset_8 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000040-0000-0014-0000-005500000069"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-05-20 02:05:08.616 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("w0Q=")))}))
testObject_Asset_asset_9 :: Asset
testObject_Asset_asset_9 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000053-0000-0009-0000-005f00000033"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-06-02 01:37:15.569 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("fb3pqQ==")))}))
testObject_Asset_asset_10 :: Asset
testObject_Asset_asset_10 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000003-0000-0014-0000-00360000001b"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-06-05 19:18:07.558 UTC")) & assetToken .~ Nothing)
testObject_Asset_asset_11 :: Asset
testObject_Asset_asset_11 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000006e-0000-0004-0000-00340000006b"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-26 13:43:34.501 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("q5tqcuPvIvC6nfI7hc6C1jg=")))}))
testObject_Asset_asset_12 :: Asset
testObject_Asset_asset_12 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000007b-0000-0006-0000-00700000006b"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-03 00:10:37.429 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("RQ==")))}))
testObject_Asset_asset_13 :: Asset
testObject_Asset_asset_13 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000042-0000-0077-0000-00600000005d"))) AssetVolatile) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("vzFsr9vKc9vWedU94wMj4-NpaTZpMWUEeRA=")))}))
testObject_Asset_asset_14 :: Asset
testObject_Asset_asset_14 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000003c-0000-0006-0000-002c0000001e"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-05-27 13:47:42.259 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("eu4lW3MMyrI_Yl5yBFjrpg==")))}))
testObject_Asset_asset_15 :: Asset
testObject_Asset_asset_15 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000003f-0000-001a-0000-000b0000003d"))) AssetVolatile) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("iV2VfgLYp4o=")))}))
testObject_Asset_asset_16 :: Asset
testObject_Asset_asset_16 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000005b-0000-003a-0000-00440000002a"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-05-25 02:55:26.776 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("cJ-UadqxyUUFYMJNbEo-56rWqDzjvQ==")))}))
testObject_Asset_asset_17 :: Asset
testObject_Asset_asset_17 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000070-0000-005f-0000-007900000002"))) AssetExpiring) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("")))}))
testObject_Asset_asset_18 :: Asset
testObject_Asset_asset_18 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000053-0000-001f-0000-001200000025"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-04-22 05:18:58.472 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("L0dQ")))}))
testObject_Asset_asset_19 :: Asset
testObject_Asset_asset_19 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000000e-0000-0010-0000-004d0000001d"))) AssetEternal) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("jhCUa29AwYbRBOC9uyw=")))}))
testObject_Asset_asset_20 :: Asset
testObject_Asset_asset_20 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000005d-0000-0053-0000-002a00000039"))) AssetExpiring) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("pSXKUgZJOgY=")))}))
