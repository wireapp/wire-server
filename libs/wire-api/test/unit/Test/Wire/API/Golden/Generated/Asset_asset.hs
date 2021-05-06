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
testObject_Asset_asset_1 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000003"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-17 10:17:39.487551152524 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("")))}))
testObject_Asset_asset_2 :: Asset
testObject_Asset_asset_2 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000004"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-05-05 15:22:12.114964834916 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("yQ==")))}))
testObject_Asset_asset_3 :: Asset
testObject_Asset_asset_3 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000004"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-09 02:46:19.208218528732 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("KQ==")))}))
testObject_Asset_asset_4 :: Asset
testObject_Asset_asset_4 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000000000003"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-05-15 09:06:55.687378720213 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("yVZvCw==")))}))
testObject_Asset_asset_5 :: Asset
testObject_Asset_asset_5 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000200000001"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-18 06:22:40.173777191739 UTC")) & assetToken .~ Nothing)
