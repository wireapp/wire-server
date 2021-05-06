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
testObject_Asset_asset_1 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000005-0000-0039-0000-002500000029"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-06-02 19:00:06.462 UTC")) & assetToken .~ Nothing)
testObject_Asset_asset_2 :: Asset
testObject_Asset_asset_2 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000039-0000-006c-0000-00590000006c"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-19 10:56:32.71 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("wMf-0-QYubIvic4O90_WzkW5EJWA50mc")))}))
testObject_Asset_asset_3 :: Asset
testObject_Asset_asset_3 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000007c-0000-0063-0000-001a0000006a"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-05-27 08:06:00.201 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("KtTyqFQwJSg41w==")))}))
testObject_Asset_asset_4 :: Asset
testObject_Asset_asset_4 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000025-0000-003c-0000-00800000004b"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-05-02 03:56:10.797 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("JSozPDkEsxCToZEOgjk-Q4tlzMwP8P2yadQ=")))}))
testObject_Asset_asset_5 :: Asset
testObject_Asset_asset_5 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000003c-0000-0054-0000-003b00000017"))) AssetExpiring) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("JW4NDtKnWt9pBJ2Gpznfabpef1I=")))}))
testObject_Asset_asset_6 :: Asset
testObject_Asset_asset_6 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000023-0000-0002-0000-007c00000026"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("zNlmUd5jasD4_WA0mlllfcdZIfw-4mbwQd62cQ==")))}))
testObject_Asset_asset_7 :: Asset
testObject_Asset_asset_7 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000001b-0000-0016-0000-001d00000003"))) AssetPersistent) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("")))}))
testObject_Asset_asset_8 :: Asset
testObject_Asset_asset_8 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000007c-0000-0051-0000-000400000066"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-05-14 14:04:52.257 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("KdWe")))}))
testObject_Asset_asset_9 :: Asset
testObject_Asset_asset_9 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000003f-0000-004d-0000-00580000007d"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-13 22:56:00.755 UTC")) & assetToken .~ Nothing)
testObject_Asset_asset_10 :: Asset
testObject_Asset_asset_10 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000070-0000-005a-0000-007100000023"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-06-04 08:27:20.275 UTC")) & assetToken .~ Nothing)
testObject_Asset_asset_11 :: Asset
testObject_Asset_asset_11 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000015-0000-006d-0000-00590000006b"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-04-12 23:07:20.226 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("XHtOWXMqTfx1")))}))
testObject_Asset_asset_12 :: Asset
testObject_Asset_asset_12 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000071-0000-0074-0000-003800000073"))) AssetEternal) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("dHeQCzCyRbMusg5a")))}))
testObject_Asset_asset_13 :: Asset
testObject_Asset_asset_13 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000012-0000-0024-0000-00150000002b"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-05-21 21:50:03.821 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("d9QUdTaqwM7VzK5IvTvNkRSsd4wHMeDl")))}))
testObject_Asset_asset_14 :: Asset
testObject_Asset_asset_14 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000004a-0000-0049-0000-00390000002a"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-12 20:16:55.074 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("CgvHY4j0bmygtlqkTAN0tExqtY5zTwQidA==")))}))
testObject_Asset_asset_15 :: Asset
testObject_Asset_asset_15 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000001e-0000-0062-0000-006500000048"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-19 07:20:32.213 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("KLdjbwLuTKQZpQqbk8zX0Z1v5R8xB5Kpl9mXBmo=")))}))
testObject_Asset_asset_16 :: Asset
testObject_Asset_asset_16 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000022-0000-0061-0000-003700000023"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-05 06:46:39.885 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("Opol5iLs6uxB8_RkwA==")))}))
testObject_Asset_asset_17 :: Asset
testObject_Asset_asset_17 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000002c-0000-007c-0000-001600000032"))) AssetVolatile) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("lA==")))}))
testObject_Asset_asset_18 :: Asset
testObject_Asset_asset_18 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000007c-0000-004a-0000-005600000034"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-05-25 02:27:26.78 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("XbGn4m-hef9WMg7UyQ1VsA==")))}))
testObject_Asset_asset_19 :: Asset
testObject_Asset_asset_19 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000001d-0000-000e-0000-005800000066"))) AssetVolatile) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("rOw=")))}))
testObject_Asset_asset_20 :: Asset
testObject_Asset_asset_20 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000006b-0000-0049-0000-00540000003e"))) AssetExpiring) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("LcXY2JoebqwFu9j19YFuXwBcaQ==")))}))
