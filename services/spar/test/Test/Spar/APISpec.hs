{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Spar.APISpec where

import Imports

import Arbitrary ()
import Data.Aeson (encode, eitherDecode)
import Data.Metrics.Servant (routesToPaths)
import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Proxy (Proxy(Proxy))
import Servant.Swagger (validateEveryToJSON)
import Spar.API as API
import Spar.Types (IdPMetadataInfo(IdPMetadataValue))
import Test.Hspec (Spec, it, shouldBe)
import Test.QuickCheck (property)


spec :: Spec
spec = do
  -- Note: SCIM types are not validated because their content-type is 'SCIM'.
  validateEveryToJSON (Proxy @API.OutsideWorldAPI)

  it "api consistency" $ do
    pathsConsistencyCheck (routesToPaths @API.API) `shouldBe` mempty

  it "roundtrip: IdPMetadataInfo" . property $ \(val :: IdPMetadataInfo) -> do
    let withoutRaw (IdPMetadataValue _ x) = x
    (withoutRaw <$> (eitherDecode . encode) val) `shouldBe` Right (withoutRaw val)
