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
testObject_Asset_asset_1 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000016-0000-007b-0000-00600000000c"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-11 07:04:49.646 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("vYJYIGksNw==")))}))
testObject_Asset_asset_2 :: Asset
testObject_Asset_asset_2 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000043-0000-0013-0000-004700000057"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-06-03 09:39:16.659 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("huk7ApZvilrs")))}))
testObject_Asset_asset_3 :: Asset
testObject_Asset_asset_3 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000036-0000-001e-0000-003b00000034"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-13 06:04:27.567 UTC")) & assetToken .~ Nothing)
testObject_Asset_asset_4 :: Asset
testObject_Asset_asset_4 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000014-0000-0003-0000-00160000006f"))) AssetExpiring) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("FwVm7L0aBop-vtkpZjbgHPLV51kREQ==")))}))
testObject_Asset_asset_5 :: Asset
testObject_Asset_asset_5 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000067-0000-0048-0000-007b0000003d"))) AssetPersistent) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("1U4gfDpJfGoU8xgWUvzaoxy2Ry5GXYa83aKt")))}))
