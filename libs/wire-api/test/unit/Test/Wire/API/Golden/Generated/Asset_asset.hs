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
testObject_Asset_1 :: Asset
testObject_Asset_1 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000400000002"))) AssetEternal) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("Dg==")))}))
testObject_Asset_2 :: Asset
testObject_Asset_2 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-04-29 14:01:52.725618973701 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("zVKxfqkqj5M=")))}))
testObject_Asset_3 :: Asset
testObject_Asset_3 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000400000002"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-14 20:18:53.312326166967 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("")))}))
testObject_Asset_4 :: Asset
testObject_Asset_4 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000400000000"))) AssetExpiring) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("3VY=")))}))
testObject_Asset_5 :: Asset
testObject_Asset_5 = (mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000002"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Nothing)
